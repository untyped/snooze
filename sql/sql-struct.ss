#lang scheme/base

(require "../base.ss")

(require srfi/19
         (unlib-in time symbol)
         "../core/snooze-struct.ss"
         "../core/struct.ss")

; ***** NOTE *****
; The terms "entity" and "attribute" are used here
; to refer to parts of the query representation. The
; "entity" and "attribute" from core/core.ss are referred
; to as "era:entity" and "era:attribute".
; ****************

; Sources --------------------------------------

(define-struct source () #:prefab)

(define-struct (source-alias source) (name value) #:prefab)

; (struct symbol entity)
(define-struct (entity-alias source-alias) () #:prefab)

; (struct symbol query)
(define-struct (query-alias source-alias) () #:prefab)

; source-alias -> (listof column)
(define (source-alias-columns alias)
  (if (entity-alias? alias)
      (map (cut create-attribute-alias alias <>)
           (entity-attributes (source-alias-value alias)))
      (query-what (source-alias-value alias))))

; (struct symbol source source (U expression #f))
(define-struct (join source) (op left right on) #:prefab)

; Expressions ------------------------------------

; (struct type)
(define-struct expression (type) #:prefab)

; (struct type symbol)
(define-struct (column expression) (name) #:prefab)

; (struct type entity-alias attribute)
(define-struct (attribute-alias column) (entity attribute) #:prefab)

; entity-alias attribute -> value
(define (create-attribute-alias entity attr)
  (make-attribute-alias (attribute-type attr) 
                        (symbol-append (source-alias-name entity) '- (attribute-name attr))
                        entity
                        attr))

; (struct type symbol expression)
(define-struct (expression-alias column) (value) #:prefab) 

; symbol expression -> value
(define (create-expression-alias name value)
  (make-expression-alias (expression-type value) name value))

; (struct type symbol (listof expression)
(define-struct (function expression) (op args) #:prefab)

; (struct type symbol (listof (U entity-alias attribute-alias)))
(define-struct (aggregate function) () #:transparent)

; (struct type any)
(define-struct (literal expression) (value) #:prefab)

; literal-value -> literal
(define (create-literal val)
  (cond [(boolean? val)       (make-literal type:boolean  val)]
        [(integer? val)       (make-literal type:integer  val)]
        [(real? val)          (make-literal type:real     val)]
        [(string? val)        (make-literal type:string   val)]
        [(symbol? val)        (make-literal type:symbol   val)]
        [(time-utc? val)      (make-literal type:time-utc val)]
        [(time-tai? val)      (make-literal type:time-tai val)]
        [(guid? val)          (make-literal (make-guid-type #f (guid-entity val)) val)]
        [(snooze-struct? val) (create-literal (snooze-struct-guid val))]
        [else                 (raise-type-error 'create-literal
                                                "(U boolean integer real string symbol time-tai time-utc)"
                                                val)]))

; type -> literal
(define (create-null type)
  (make-literal type (type-null type)))

; Ordering ---------------------------------------

; (struct expression (U 'asc 'desc))
(define-struct order (expression direction) #:prefab)

; Queries ----------------------------------------

; (struct (listof column)
;         (U expression #t #f)
;         source
;         (U expression #f)
;         (listof expression)
;         (listof order)
;         (U expression #f)
;         (U integer #f)
;         (U integer #f)
;         (listof column)
;         (listof column)
;         (U entity type (listof (U entity type))))
(define-struct query
  (what
   distinct
   from
   where
   group
   order
   having
   limit
   offset
   local-columns
   imported-columns
   extract-info)
  #:prefab)

; Predicates -----------------------------------

; any -> boolean
(define (source+query? item)
  (or (source? item)
      (query? item)))

; any -> boolean
(define (literal-value? item)
  (or (boolean? item)
      (integer? item)
      (real? item)
      (string? item)
      (symbol? item)
      (time-tai? item)
      (time-utc? item)
      (guid? item)
      (snooze-struct? item)))

; any -> boolean
(define (quotable? item)
  (or (expression? item)
      (literal-value? item)
      (query? item)
      (query-alias? item)))

; (U expression source query boolean integer real string symbol time-tai time-utc) -> (U expression source)
(define (quote-argument arg)
  (cond [(expression? arg)    arg]
        [(source? arg)        arg]
        [(literal-value? arg) (create-literal arg)]
        [(query? arg)         (make-query-alias (string->symbol (symbol->string (gensym 'subq))) arg)]
        [else (raise-type-error
               'quote-argument
               "(opt-listof (U expression entity attribute query boolean integer real string symbol time-tai time-utc guid))"
               arg)]))

; Provide statements --------------------------

; contract
;
; "in" takes some weird argument types:
(define function-arg/c
  (or/c expression? query-alias? entity-alias? query? (listof expression?)))

; contract
(define aggregate-arg/c
  (or/c function-arg/c entity-alias? query-alias?))

; contract
(define source/c
  (or/c entity-alias? query-alias?))

; Provide statements -----------------------------

(provide (except-out (struct-out attribute-alias) make-attribute-alias)
         (except-out (struct-out expression-alias) make-expression-alias)
         (except-out (struct-out literal) make-literal)
         (rename-out (create-attribute-alias make-attribute-alias))
         (rename-out (create-expression-alias make-expression-alias))
         (rename-out (create-literal make-literal))
         (rename-out (create-null make-null))
         quotable?
         quote-argument
         source+query?
         source/c)

(provide/contract
 [struct source                        ()]
 [struct (source-alias source)         ([name symbol?] [value (or/c entity? query?)])]
 [struct (entity-alias source-alias)   ([name symbol?] [value entity?])]
 [struct (query-alias source-alias)    ([name symbol?] [value query?])]
 [struct (join source)                 ([op symbol?] [left source?] [right source?] [on (or/c expression? false/c)])]
 [struct expression                    ([type type?])]
 [struct (column expression)           ([type type?] [name symbol?])]
 [struct (function expression)         ([type type?] [op symbol?] [args (listof function-arg/c)])]
 [struct (aggregate function)          ([type type?] [op symbol?] [args (listof function-arg/c)])]
 [struct order                         ([expression expression?] [direction (symbols 'asc 'desc)])]
 [struct query                         ([what             (listof column?)]
                                        [distinct         (or/c (listof expression?) false/c)]
                                        [from             source?]
                                        [where            (or/c expression? false/c)]
                                        [group            (listof expression?)]
                                        [order            (listof order?)]
                                        [having           (or/c expression? false/c)]
                                        [limit            (or/c integer? false/c)]
                                        [offset           (or/c integer? false/c)]
                                        [local-columns    (listof column?)]
                                        [imported-columns (listof column?)]
                                        [extract-info     (or/c entity? type? (listof (or/c entity? type?)))])]
 [source-alias-columns                 (-> source-alias? (listof column?))])
