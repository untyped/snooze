#lang scheme/base

(require (for-syntax scheme/base)
         mzlib/kw
         scheme/contract
         scheme/match
         srfi/19/time
         srfi/26/cut
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/symbol)
         "../base.ss"
         "../core/struct.ss"
         "sql-struct.ss"
         "sql-util.ss")

; Aliases --------------------------------------

; (U (symbol entity                 -> entity-alias)
;    (symbol query                  -> query-alias)
;    (symbol expression             -> expression-alias)
;    (symbol entity-alias attribute -> attribute-alias))
(define sql:alias
  (match-lambda*
    [(list (? symbol? id) (? entity? item))
     (make-entity-alias id item)]
    [(list (? symbol? id) (? query? item))
     (make-query-alias id item)]
    [(list (? symbol? id) (? non-alias-expression? item))
     (make-expression-alias id item)]
    [(list (and alias (struct entity-alias (name entity))) (? attribute? attr))
     (define entity (source-alias-value alias))
     (if (eq? (attribute-entity attr) entity)
         (make-attribute-alias alias attr)
         (raise-exn exn:fail:contract
           (format "Entity does not contain that attribute: ~a ~a" entity attr)))]
    [other (raise-exn exn:fail:contract (format "Bad arguments to sql:alias: ~s" other))]))

; entity-alias (U symbol attribute) -> attribute-alias
;
; Provides backwards compatibility with the q:attr form in a previous Snooze query language.
(define (sql:attr alias attr+name)
  (make-attribute-alias alias (entity-attribute (source-alias-value alias) attr+name)))

; any -> boolean
(define (non-alias-expression? item)
  (and (expression? item)
       (not (expression-alias? item))
       (not (attribute-alias? item))))

; Select ---------------------------------------

;  [#:what     (U entity-alias attribute-alias expression-alias (listof (U entity-alias attribute-alias expression-alias)))]
;  [#:distinct (U expression #f)
;   #:from     (U source query)
;  [#:where    (U expression #f)]
;  [#:group    (listof (U entity-alias group-alias expression))]
;  [#:order    (listof order)]
;  [#:having   (U expression #f)]
;  [#:limit    (U integer #f)]
;  [#:offset   (U integer #f)]
; -> 
;  query
(define (sql:select #:what     [what #f] 
                    #:distinct [distinct #f]
                    #:from     from
                    #:where    [where #f]
                    #:group    [group null]
                    #:order    [order null]
                    #:having   [having #f]
                    #:limit    [limit #f]
                    #:offset   [offset #f])
  (sql:select/internal what distinct from where group order having limit offset))

;  (U (opt-listof (U column source-alias)) #f)
;  (U (opt-listof (U expression source-alias)) #t #f)
;  source
;  (U expression #f)
;  (listof (U source-alias column))
;  (listof order)
;  (U expression #f)
;  (U integer #f)
;  (U integer #f)
; -> 
;  query
(define (sql:select/internal what* distinct* from* where group* order having limit offset)
  
  ; source
  (define from
    (quote-argument from*))
  
  ; (listof (U attribute-alias expression-alias))
  ; (U entity type (listof (U entity type)))
  (define-values (what expand-info)
    (expand-what-argument (if what* what* (make-default-what-argument from))))
  
  ; (U (listof expression) #f)
  (define distinct
    (expand-distinct-argument distinct*))
  
  ; (listof expression-alias)
  (define group
    (expand-group-argument group*))
  
  ; Check #:from:
  (unless (source? from)
    (raise-select-exn '#:from "source" from))
  
  ; Check #:where:
  (unless (or (expression? where) (not where))
    (raise-select-exn '#:where "(U expression #f)" where))
  
  ; Check #:group:
  (unless (and (list? group) (andmap expression? group))
    (raise-select-exn '#:group "(listof expression)" group))
  
  ; Check #:order:
  (unless (and (list? order) (andmap order? order))
    (raise-select-exn '#:order "(listof order)" order))
  
  (unless (or (expression? having) (not having))
    (raise-select-exn '#:having "(U expression #f)" having))
  
  ; Check #:limit:
  (unless (or (integer? limit) (not limit))
    (raise-select-exn '#:limit "(U integer #f)" limit))
  
  ; Check #:offset:
  (unless (or (integer? offset) (not offset))
    (raise-select-exn '#:offset "(U integer #f)" offset))
  
  (let*-values (; (listof source-alias)
                [(sources) (source->sources from)]
                ; (listof column)
                ; (listof column)
                [(local-columns imported-columns) (source->columns from)]
                ; (listof column)
                [(columns) (append local-columns imported-columns)]
                [(columns*) (append what columns)])
    ; No need to check the from clause: create-join does that for us.
    (check-what-clause what sources columns)
    (check-distinct-clause distinct sources columns)
    (check-where-clause where sources columns)
    (check-group-clause group sources columns*)
    (check-order-clause order sources columns*)
    (check-having-clause having sources columns*)
    
    ; Make and return the structure:
    (make-query what distinct from where group order having limit offset local-columns imported-columns expand-info)))

; keyword string any -> void | exn:fail:contract
(define (raise-select-exn kw expected received)
  (raise-exn exn:fail:contract
    (format "~a argument to select: expected ~a, received ~s" kw expected received)))

; Sources --------------------------------------

; source source expression -> join
(define (sql:inner left right op)
  (create-join 'inner left right (quote-argument op)))

; source source expression -> join
(define (sql:left left right op)
  (create-join 'left left right (quote-argument op)))

; source source expression -> join
(define (sql:right left right op)
  (create-join 'right left right (quote-argument op)))

; source source -> join
(define (sql:outer left right)
  (create-join 'outer left right #f))

; (U 'inner 'outer 'left 'right) source source (U expression #f) -> join
(define (create-join op left right on)
  (define ans (make-join op (quote-argument left) (quote-argument right) on))
  (check-join ans)
  ans)

; Expressions ----------------------------------

; attribute-alias -> aggregate
(define (sql:count arg)
  (let ([arg (quote-argument arg)])
    (make-aggregate type:integer 'count (list arg))))

; [source-alias] -> aggregate
(define sql:count*
  (case-lambda
    [()      (make-aggregate type:integer 'count* null)]
    [(alias) (make-aggregate type:integer 'count* (list alias))]))

; expression+quotable -> aggregate
(define (sql:min arg)     
  (let ([arg (quote-argument arg)])
    (make-aggregate (expression-type arg) 'min (list arg))))

; expression+quotable -> aggregate
(define (sql:max arg)     
  (let ([arg (quote-argument arg)])
    (make-aggregate (expression-type arg) 'max (list arg))))

; expression+quotable -> aggregate
(define (sql:average alias)     
  (let ([arg (quote-argument alias)])
    (if (numeric-expression? arg)
        (make-aggregate type:real 'average (list arg))
        (raise-type-error 'average "numeric-expression" arg))))

; expression+quotable ... -> function
(define-function (sql:and . args)
  [(andmap boolean-expression? args) type:boolean])

; expression+quotable ... -> function
(define-function (sql:or  . args)
  [(andmap boolean-expression? args) type:boolean])

; expression+quotable -> function
(define-function (sql:not arg)
  [(boolean-expression? arg) type:boolean])

; expression+quotable ... -> function
(define-function (sql:+ . args)
  [(andmap integer-expression? args)  type:integer]
  [(andmap numeric-expression? args)  type:real]
  [(andmap time-tai-expression? args) type:time-tai]
  [(andmap temporal-expression? args) type:time-utc])

; expression+quotable ... -> function
(define-function (sql:- . args)
  [(andmap integer-expression? args)  type:integer]
  [(andmap numeric-expression? args)  type:real]
  [(andmap time-tai-expression? args) type:time-tai]
  [(andmap temporal-expression? args) type:time-utc])

; expression+quotable ... -> function
(define-function (sql:* . args)
  [(andmap integer-expression? args) type:integer]
  [(andmap numeric-expression? args) type:real])

; expression+quotable ... -> function
(define-function (sql:/ arg1 arg2)
  [(and (numeric-expression? arg1) (numeric-expression? arg2))  type:real])

; expression+quotable -> function
(define-function (sql:abs arg)
  [(integer-expression? arg) type:integer]
  [(numeric-expression? arg) type:real])

; expression+quotable -> function
(define-function (sql:floor arg)
  [(numeric-expression? arg) type:integer])

; expression+quotable -> function
(define-function (sql:ceiling arg)
  [(numeric-expression? arg) type:integer])

; expression+quotable -> function
(define-function (sql:round arg)
  [(numeric-expression? arg) type:integer])

; TODO : Functions from http://www.postgresql.org/docs/8.2/static/functions-math.html

; expression+quotable expression+quotable -> function
(define-function (sql:like arg1 arg2)
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:regexp-match arg1 arg2)
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:regexp-match-ci arg1 arg2)
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean])

; expression+quotable ... -> function
(define-function (sql:string-append . args)
  [(andmap symbol-expression? args) type:symbol]
  [(andmap character-expression? args) type:string])

; expression+quotable ... -> function
(define-function (sql:string-replace arg1 arg2 arg3)
  [(andmap symbol-expression? (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; expression+quotable ... -> function
(define-function (sql:regexp-replace arg1 arg2 arg3)
  [(andmap symbol-expression? (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; expression+quotable ... -> function
(define-function (sql:regexp-replace-ci arg1 arg2 arg3)
  [(andmap symbol-expression? (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; expression+quotable ... -> function
(define-function (sql:regexp-replace* arg1 arg2 arg3)
  [(andmap symbol-expression? (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; expression+quotable ... -> function
(define-function (sql:regexp-replace*-ci arg1 arg2 arg3)
  [(andmap symbol-expression? (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; expression+quotable expression+quotable -> function
(define-function (sql:= arg1 arg2)
  [#t type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:<> arg1 arg2)
  [#t type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:< arg1 arg2)
  [(and (boolean-expression? arg1) (boolean-expression? arg2))     type:boolean]
  [(and (numeric-expression? arg1) (numeric-expression? arg2))     type:boolean]
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean]
  [(and (temporal-expression? arg1) (temporal-expression? arg2))   type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:> arg1 arg2)
  [(and (boolean-expression? arg1) (boolean-expression? arg2))     type:boolean]
  [(and (numeric-expression? arg1) (numeric-expression? arg2))     type:boolean]
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean]
  [(and (temporal-expression? arg1) (temporal-expression? arg2))   type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:>= arg1 arg2)
  [(and (boolean-expression? arg1) (boolean-expression? arg2))     type:boolean]
  [(and (numeric-expression? arg1) (numeric-expression? arg2))     type:boolean]
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean]
  [(and (temporal-expression? arg1) (temporal-expression? arg2))   type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:<= arg1 arg2)
  [(and (boolean-expression? arg1) (boolean-expression? arg2))     type:boolean]
  [(and (numeric-expression? arg1) (numeric-expression? arg2))     type:boolean]
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean]
  [(and (temporal-expression? arg1) (temporal-expression? arg2))   type:boolean])

; expression+quotable -> function
(define-function (sql:null? arg)
  [#t type:boolean])

; expression+quotable ... -> function
(define-function (sql:coalesce . args)
  [(andmap boolean-expression? args)  type:boolean]
  [(andmap integer-expression? args)  type:integer]
  [(andmap numeric-expression? args)  type:real]
  [(andmap symbol-expression? args)   type:symbol]
  [(andmap string-expression? args)   type:string]
  [(andmap time-tai-expression? args) type:time-tai]
  [(andmap temporal-expression? args) type:time-utc])

; expression+quotable expression+quotable -> function
(define-function (sql:->string arg1 arg2)
  [(string-expression? arg2) type:string])

; expression+quotable expression+quotable -> function
(define-function (sql:->symbol arg1 arg2)
  [(string-expression? arg2) type:symbol])

; expression+quotable (U query-alias (listof expression+quotable)) -> function
(define (sql:in arg1 arg2)
  (let* ([arg1  (quote-argument arg1)]
         [arg2  (if (list? arg2)
                    (map quote-argument arg2)
                    arg2)]
         [type1 (expression-type arg1)]
         [type2 (cond [(null? arg2)  type1] ; hack to make sure the type check passes
                      [(pair? arg2)  (let ([type2 (expression-type (car arg2))])
                                       (unless (andmap (cut type-compatible? type2 <>)
                                                       (map expression-type arg2))
                                         (raise-exn exn:fail:contract
                                           (format "sql:in: list elements must all be of the same type: ~a" arg2)))
                                       type2)]
                      [(query? arg2) (let ([columns (query-what arg2)])
                                       (unless (= (length columns) 1)
                                         (raise-exn exn:fail:contract
                                           (format "sql:in: subquery must have exactly one column: ~a" arg2)))
                                       (expression-type (car columns)))])])
    (unless (type-compatible? type1 type2)
      (raise-exn exn:fail:contract
        (format "sql:in: type mismatch: argument types do not match: ~a ~a" (type-name type1) (type-name type2))))
    (make-function type:boolean 'in (list arg1 arg2))))

; expression+quotable expression+quotable [expression+quotable] -> function
(define sql:if
  (case-lambda
    [(test pos)
     (define pos* (quote-argument pos))
     (sql:if test pos* (sql:null (expression-type pos*)))]
    [(test pos neg)
     (let* ([test (quote-argument test)]
            [pos  (quote-argument pos)]
            [neg  (quote-argument neg)]
            [type (cond [(and (boolean-expression? test) (andmap boolean-expression?   (list pos neg))) type:boolean]
                        [(and (boolean-expression? test) (andmap integer-expression?   (list pos neg))) type:integer]
                        [(and (boolean-expression? test) (andmap numeric-expression?   (list pos neg))) type:real]
                        [(and (boolean-expression? test) (andmap symbol-expression?    (list pos neg))) type:symbol]
                        [(and (boolean-expression? test) (andmap character-expression? (list pos neg))) type:string]
                        [(and (boolean-expression? test) (andmap time-tai-expression?  (list pos neg))) type:time-tai]
                        [(and (boolean-expression? test) (andmap temporal-expression?  (list pos neg))) type:time-utc]
                        [else (raise-exn exn:fail:snooze
                                (format "Function not defined for the supplied argument types: ~a"
                                        (cons 'id (map type-name (map expression-type (list test pos neg))))))])])
       (make-function type 'if (list test pos neg)))]))

; (_ [expt expr] ... [else expr])
(define-syntax sql:cond
  (syntax-rules (else)
    [(_ [test expr1])
     (sql:if test expr1)]
    [(_ [test expr1] [else expr2])
     (sql:if test expr1 expr2)]
    [(_ [test1 expr1] [test2 expr2] ...)
     (sql:if test1 expr1 (sql:cond [test2 expr2] ...))]))

; quotable -> expression
(define sql:literal make-literal)

; type -> literal
(define (sql:null type)
  (make-null type))

; Order ------------------------------------------

; expression+quotable (U 'asc 'desc) -> order
(define (sql:order expr dir)
  (make-order (quote-argument expr) dir))

; expression -> order
(define sql:asc  (cut sql:order <> 'asc))
(define sql:desc (cut sql:order <> 'desc))

; Contracts --------------------------------------

(define select-distinct/c (or/c expression? (listof expression?) boolean?))
(define select-what/c     (or/c expression? source-alias? (listof (or/c expression? source-alias?)) false/c))
(define select-from/c     (or/c source? query?))
(define select-where/c    (or/c expression? false/c))
(define select-group/c    (listof (or/c column? source-alias?)))
(define select-order/c    (listof order?))
(define select-having/c   (or/c expression? false/c))
(define select-limit/c    (or/c natural-number/c #f))
(define select-offset/c   (or/c natural-number/c #f))

; Provide statements -----------------------------

(provide (rename-out [sql:alias alias]
                     [sql:cond  cond])
         select-distinct/c
         select-what/c    
         select-from/c    
         select-where/c   
         select-group/c   
         select-order/c   
         select-having/c  
         select-limit/c   
         select-offset/c)

(provide/contract
 [rename sql:select          select          (->* (#:from     select-from/c)
                                                  (#:what     select-what/c
                                                              #:distinct select-distinct/c
                                                              #:where    select-where/c
                                                              #:group    select-group/c
                                                              #:order    select-order/c
                                                              #:having   select-having/c
                                                              #:limit    select-limit/c
                                                              #:offset   select-offset/c)
                                                  query?)]
 [rename sql:select/internal select/internal (-> (or/c expression? source-alias? (listof (or/c expression? source-alias?)) false/c)
                                                 (or/c expression? (listof expression?) boolean?)
                                                 (or/c source? query?)
                                                 (or/c expression? false/c)
                                                 (listof (or/c column? source-alias?))
                                                 (listof order?)
                                                 (or/c expression? false/c)
                                                 (or/c integer? false/c)
                                                 (or/c integer? false/c)
                                                 query?)]
 [rename sql:attr            attr            (-> entity-alias? (or/c attribute? symbol?) attribute-alias?)]
 [rename sql:count           count           (-> attribute-alias? aggregate?)]
 [rename sql:count*          count*          (->* () ((or/c entity-alias? query-alias?)) aggregate?)]
 [rename sql:min             min             (-> quotable? aggregate?)]
 [rename sql:max             max             (-> quotable? aggregate?)]
 [rename sql:average         average         (-> quotable? aggregate?)]
 [rename sql:inner           inner           (-> source+query? source+query? quotable? join?)]
 [rename sql:left            left            (-> source+query? source+query? quotable? join?)]
 [rename sql:right           right           (-> source+query? source+query? quotable? join?)]
 [rename sql:outer           outer           (-> source+query? source+query? join?)]
 [rename sql:in              in              (-> quotable? (or/c query? (listof quotable?)) function?)]
 [rename sql:if              if              (->* (quotable? quotable?) (quotable?) function?)]
 [rename sql:literal         literal         (-> quotable? literal?)]
 [rename sql:null            null            (-> type? literal?)]
 [rename sql:order           order           (-> quotable? (symbols 'asc 'desc) order?)]
 [rename sql:asc             asc             (-> quotable? order?)]
 [rename sql:desc            desc            (-> quotable? order?)])
