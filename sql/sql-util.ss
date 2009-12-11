#lang scheme/base

(require (for-syntax scheme/base
                     "../base.ss")
         "../base.ss")

(require (only-in srfi/1 append-map)
         (unlib-in match symbol)
         "../core/struct.ss"
         "sql-struct.ss")

; Extract info and automatic #:what arguments ----

; source -> source-list
;
; where source-list : (listof source-alias)
(define source->sources
  (match-lambda
    [(? join? join)          (append (source->sources (join-left join)) (source->sources (join-right join)))]
    [(? query-alias? alias)  (list alias)]
    [(? entity-alias? alias) (list alias)]))

; source -> column-list column-list
;
; where column-list : (listof column)
;   and column      : (U expression-alias attribute-alias)
(define source->columns
  (match-lambda
    [(? join? join)          
     (let*-values ([(left-local  left-imported)  (source->columns (join-left  join))]
                   [(right-local right-imported) (source->columns (join-right join))])
       (values (append left-local right-local)
               (append left-imported right-imported)))]
    [(? query-alias? alias)  
     (values null (source-alias-columns alias))]
    [(? entity-alias? alias)
     (values (source-alias-columns alias) null)]))

; join -> (hasheqof attribute-alias (U attribute-alias literal))
(define (source->foreign-keys source)
  (let* (; Collect the criteria from all joins in the select:
         [exprs (let loop ([source source])
                  (match source
                    [(? join? join)
                     (cons (join-on join)
                           (append (loop (join-left join))
                                   (loop (join-right join))))]
                    [(? query-alias? alias)  null]
                    [(? entity-alias? alias) null]))]
         ; Empty hash table in which to store the results:
         [hash  (make-hasheq)])
    ; Iterate through the expressions collected above, extracting and caching any fk=pk constraints:
    (for ([expr (in-list exprs)])
      (let loop ([expr expr])
        (match expr
          [(struct function (_ (eq? '=) (list (? attribute-alias? col1) (? attribute-alias? col2))))
           (let ([fk    (or (and (attribute-foreign-key? (attribute-alias-attribute col1)) col1)
                            (and (attribute-foreign-key? (attribute-alias-attribute col2)) col2))]
                 [pk    (or (and (attribute-primary-key? (attribute-alias-attribute col1)) col1)
                            (and (attribute-primary-key? (attribute-alias-attribute col2)) col2)
                            (and (literal? col1) col1)
                            (and (literal? col2) col2))])
             (when (and fk pk)
               (hash-set! hash fk pk)))]
          [(? function?)
           (for-each loop (function-args expr))]
          [(? expression-alias?)
           (loop (expression-alias-value expr))]
          [_ (void)])))
    hash))

; source -> (opt-listof (U expression entity-alias query-alias))
(define (make-default-what-argument from)
  (if (join? from)
      (source->sources from)
      (car (source->sources from))))

;  ((opt-listof (U expression entity-alias query-alias))
; ->
;  (listof expression)
(define (expand-distinct-argument argument)
  (cond [(eq? argument #t) null]
        [(eq? argument #f) #f]
        [(pair? argument)  (expand-distinct-list argument)]
        [else              (expand-distinct-item argument)]))

;  ((listof (U expression entity-alias query-alias))
; -> 
;  (listof expression)
(define (expand-distinct-list argument)
  (for/fold ([accum null])
            ([arg   argument])
            (append accum (expand-distinct-item arg))))

;  (U expression entity-alias query-alias)
; -> 
;  (listof expression)
(define expand-distinct-item
  (match-lambda
    [(? expression? expr)
     (list expr)]
    [(? entity-alias? alias)
     (list (make-attribute-alias alias (car (entity-attributes (source-alias-value alias)))))]
    [(? query-alias? alias)
     (source-alias-columns alias)]))

;   ((opt-listof (U expression entity-alias query-alias))
; ->
;    (listof column)
;    (opt-listof (U entity type)))
;
; where (opt-listof x) = (U x (listof x))
(define (expand-what-argument argument)
  (if (list? argument)
      (expand-what-list argument)
      (expand-what-item argument)))

;   ((listof (U expression entity-alias query-alias))
; -> 
;    (listof column)
;    (listof (U entity type)))
(define (expand-what-list argument)
  (for/fold ([what-accum null] [info-accum null])
            ([arg argument])
            (define-values (what-term info-term)
              (expand-what-item arg))
            (values (append what-accum what-term)
                    (append info-accum (listify info-term)))))

;   ((U expression entity-alias query-alias)
; -> 
;    (listof column)
;    (opt-listof (U entity type)))
(define expand-what-item
  (match-lambda
    [(and argument (struct attribute-alias (type _ _ attr)))
     (values (list argument)
             type)]
    [(? expression-alias? argument)
     (values (list argument) 
             (expression-type argument))]
    [(? expression? expr)
     (values (list (make-expression-alias (gensym 'expr) expr))
             (expression-type expr))]
    [(? entity-alias? alias)
     (values (source-alias-columns alias)
             (source-alias-value alias))]
    [(? query-alias? alias)
     (values (source-alias-columns alias)
             (query-extract-info (source-alias-value alias)))]))

; (listof (U expression entity-alias query-alias)) -> (listof column)
(define (expand-group-argument group)
  (append-map expand-group-item group))

; (U expression entity-alias group-alias) -> (listof column)
(define expand-group-item
  (match-lambda
    [(? column? column)      (list column)]
    [(? source-alias? alias) (source-alias-columns alias)]))

; (listof expression) source-list column-list -> void
(define (check-what-clause what sources columns)
  ; -> expression void
  (define (check-item item)
    (cond [(attribute-alias? item)
           (check-attribute-in-scope 'what-clause item columns)]
          [(expression-alias? item) 
           (with-handlers ([exn? (lambda _ (check-expression 'what-clause (expression-alias-value item) sources columns))])
             (check-expression-in-scope 'what-clause item columns))]
          [else (raise-type-error 'what-clause "(U attribute-alias expression-alias)" item)]))
  (if (list? what)
      (for-each check-item what)
      (check-item what)))

; (U (listof expression) #f) source-list column-list -> void
(define (check-distinct-clause distinct sources columns)
  (when distinct
    (for-each (cut check-expression 'distinct-clause <> sources columns)
              distinct)))

; (U expression #f) source-list column-list -> void
(define (check-where-clause where sources columns)
  (when where
    (check-expression 'where-clause where sources columns)))

; (listof expression) source-list column-list -> void
(define (check-group-clause group sources columns)
  (for-each (cut check-expression 'group-clause <> sources columns) group))

; (listof order) source-list column-list -> void
(define (check-order-clause order sources columns)
  (for-each (cut check-expression 'order-clause <> sources columns)
            (map order-expression order)))

; (U expression #f) source-list column-list -> void
(define (check-having-clause having sources columns)
  (when having
    (check-expression 'having-clause having sources columns)))

; Expression predicates --------------------------

; type ... -> (any -> boolean)
(define (make-expression-predicate . types)
  (match-lambda
    [(struct expression (type))
     (ormap (cut type-compatible? type <>) types)]
    [_ #f]))

; any -> boolean
(define boolean-expression?
  (make-expression-predicate type:boolean))

; any -> boolean
(define integer-expression?
  (make-expression-predicate type:integer))

; any -> boolean
(define real-expression?
  (make-expression-predicate type:real))

; any -> boolean
(define numeric-expression?
  (make-expression-predicate type:integer type:real))

; any -> boolean
(define string-expression?
  (make-expression-predicate type:string))

; any -> boolean
(define symbol-expression?
  (make-expression-predicate type:symbol))

; any -> boolean
(define character-expression?
  (make-expression-predicate type:string type:symbol))

; any -> boolean
(define time-utc-expression?
  (make-expression-predicate type:time-utc))

; any -> boolean
(define time-tai-expression?
  (make-expression-predicate type:time-tai))

; any -> boolean
(define temporal-expression?
  (make-expression-predicate type:time-utc type:time-tai))

; define-function syntax -------------------------

(define-syntax (define-function stx)
  (define (remove-prefix sym)
    (let ([match (regexp-match #rx"^sql:(.*)$" (symbol->string sym))])
      (if match
          (string->symbol (cadr match))
          (raise-exn exn:fail:snooze
            (format "define-function identifier does not have 'sql:' prefix: ~a" sym)))))
  (syntax-case stx (else)
    [(_ (id arg ...) [rule type] ...)
     (identifier? #'id)
     (with-syntax ([plain-id           (remove-prefix (syntax->datum #'id))]
                   [(arg-contract ...) (map (lambda _ #'quotable?) (syntax->list #'(arg ...)))])
       #'(begin (define (id arg ...)
                  (let ([arg (quote-argument arg)] ...)
                    (make-function (cond [rule type] ...
                                         [else (raise-exn exn:fail:snooze
                                                 (format "Function not defined for the supplied argument types: ~a"
                                                         (cons 'id (map type-name (map expression-type (list arg ...))))))])
                                   'plain-id
                                   (list arg ...))))
                (provide/contract [rename id plain-id (-> arg-contract ... function?)])))]
    [(_ (id . args) [rule type] ...)
     (identifier? #'id)
     (with-syntax ([plain-id (remove-prefix (syntax->datum #'id))])
       #'(begin (define (id . args)
                  (let ([args (map quote-argument args)])
                    (make-function (cond [rule type] ...
                                         [else (raise-exn exn:fail:snooze
                                                 (format "Function not defined for the supplied argument types: ~a"
                                                         (cons 'id (map type-name (map expression-type args)))))])
                                   'plain-id
                                   args)))
                (provide/contract [rename id plain-id (->* () () #:rest (listof quotable?) function?)])))]))

; Helpers ----------------------------------------

; (U x (listof x)) -> (listof x)
(define (listify item)
  (if (or (pair? item) (null? item))
      item
      (list item)))

; source-list -> void
(define (check-repeated-sources sources)
  ; void
  (let loop ([sources sources] [names (map source-alias-name sources)])
    (match sources
      [(list) (void)]
      [(list-rest curr rest)
       (when (memq (car names) (cdr names))
         (raise-exn exn:fail:contract
           (format "~a: source selected more than once: ~a ~s" 'from-clause (car names) (car sources))))
       (loop (cdr sources) (cdr names))])))

; column-list -> void
(define (check-repeated-columns columns)
  ; void
  (let loop ([columns columns] [names (map column-name columns)])
    (match columns
      [(list) (void)]
      [(list-rest curr rest)
       (when (memq (car names) (cdr names))
         (raise-exn exn:fail:contract
           (format "~a: column selected more than once: ~a ~s" 'what-clause (car names) (car columns))))
       (loop (cdr columns) (cdr names))])))

; symbol attribute-alias column-list -> void
(define (check-attribute-in-scope name attr columns)
  ; Use member here as some attributes in columns are inferred from entity-aliases:
  (unless (member attr columns)
    (raise-exn exn:fail:contract
      (format "~a: attribute not in scope: ~s" name attr))))

; symbol expression-alias column-list -> void
(define (check-expression-in-scope name expr columns)
  ; Use memq here as expressions have to be eq? to one another:
  (unless (memq expr columns)
    (raise-exn exn:fail:contract
      (format "~a: expression not in scope: ~s" name expr))))

; symbol source-alias source-list -> void
(define (check-source-in-scope name source sources)
  (unless (memq source sources)
    (raise-exn exn:fail:contract
      (format "~a: source not in scope: ~s" name source))))

; join -> void
(define (check-join j)
  (define sources (source->sources j))
  (define columns (call-with-values (cut source->columns j) append))
  (match j
    [(struct join (op left right on))
     (check-repeated-sources sources)
     (check-repeated-columns columns)
     (when on
       (check-expression op on sources columns)
       (check-no-aggregates op on))]))

; symbol attribute-alias source-list column-list -> void
(define (check-expression name expr sources columns)
  (cond [(attribute-alias? expr)  (check-attribute-in-scope name expr columns)]
        [(expression-alias? expr) (check-expression-in-scope name expr columns)]
        [(function? expr)         (for-each (cut check-expression name <> sources columns)
                                            ; Skip query arguments to sql:in:
                                            (if (and (eq? (function-op expr) 'in) 
                                                     (query? (cadr (function-args expr))))
                                                (list (car (function-args expr)))
                                                (function-args expr)))]
        [(literal? expr)          (void)]
        ; Special cases for count* and in:
        [(source-alias? expr)     (check-source-in-scope name expr sources)]
        [(list? expr)             (for-each (cut check-expression name <> sources columns) expr)]))

; symbol expression -> void
(define (check-no-aggregates name expr)
  (cond [(aggregate? expr) (raise-exn exn:fail:snooze
                             (format  "~a: aggregates not allowed: ~s" name expr))]
        [(function? expr)  (for-each (cut check-no-aggregates name <>) (function-args expr))]
        ; Special case for in:
        [(list? expr)      (for-each (cut check-no-aggregates name <>) expr)]
        [else (void)]))

; Provide statements -----------------------------

; (opt-listof contract) -> contract
(define (opt-listof item/c)
  (or/c item/c (listof item/c)))

; Provide statements -----------------------------

(provide check-join
         check-what-clause
         check-distinct-clause
         check-where-clause
         check-group-clause
         check-order-clause
         check-having-clause
         check-no-aggregates
         boolean-expression?
         integer-expression?
         real-expression?
         string-expression?
         symbol-expression?
         time-utc-expression?
         time-tai-expression?
         numeric-expression?
         character-expression?
         temporal-expression?
         define-function)

(provide/contract
 [source->sources            (-> source? (listof source/c))]
 [source->columns            (-> source? (values (listof column?) (listof column?)))]
 [source->foreign-keys       (-> source? (and/c hash? hash-eq?))]
 [make-default-what-argument (-> source? (opt-listof source/c))]
 [expand-distinct-argument   (-> (or/c boolean? (opt-listof (or/c expression? source/c)))
                                 (or/c (listof expression?) #f))]
 [expand-what-argument       (-> (opt-listof (or/c expression? source/c))
                                 (values (listof column?)
                                         (opt-listof (or/c entity? type?))))]
 [expand-group-argument      (-> (opt-listof (or/c expression? source/c))
                                 (listof column?))])
