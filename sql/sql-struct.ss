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

(define-serializable-struct source () #:transparent)

; (struct symbol)
(define-serializable-struct (source-alias source) (name) #:transparent)

; (struct symbol symbol)
(define-serializable-struct (entity-alias source-alias) (entity-name) #:transparent)

; (struct symbol entity)
(define (entity-alias-entity alias)
  (model-entity (current-model) (entity-alias-entity-name alias)))

; (struct symbol query)
(define-serializable-struct (query-alias source-alias) (query) #:transparent)

; source-alias -> (listof column)
(define (source-alias-columns alias)
  (if (entity-alias? alias)
      (map (cut create-attribute-alias alias <>)
           (entity-attributes (entity-alias-entity alias)))
      (query-what (query-alias-query alias))))

; (struct symbol source source (U expression #f))
(define-serializable-struct (join source) (op left right on) #:transparent)

; Expressions ------------------------------------

; (struct type)
(define-serializable-struct expression (type) #:transparent)

; (struct type symbol)
(define-serializable-struct (column expression) (name) #:transparent)

; (struct type entity-alias symbol)
(define-serializable-struct (attribute-alias column) (entity attribute-name) #:transparent)

; entity-alias attribute -> value
(define (create-attribute-alias entity attr)
  (make-attribute-alias (attribute-type attr) 
                        (symbol-append (source-alias-name entity) '- (attribute-name attr))
                        entity
                        (attribute-name attr)))

; attribute-alias -> attribute
(define (attribute-alias-attribute alias)
  (entity-attribute (entity-alias-entity (attribute-alias-entity alias))
                    (attribute-alias-attribute-name alias)))

; (struct type symbol expression)
(define-serializable-struct (expression-alias column) (value) #:transparent) 

; symbol expression -> value
(define (create-expression-alias name value)
  (make-expression-alias (expression-type value) name value))

; (struct type symbol (listof expression)
(define-serializable-struct (function expression) (op args) #:transparent)

; (struct type symbol (listof (U entity-alias attribute-alias)))
(define-serializable-struct (aggregate function) () #:transparent)

; (struct type any)
(define-serializable-struct (literal expression) (value) #:transparent)

; literal-value -> literal
(define (create-literal val)
  (cond [(boolean? val)       (make-literal type:boolean  val)]
        [(integer? val)       (make-literal type:integer  val)]
        [(real? val)          (make-literal type:real     val)]
        [(string? val)        (make-literal type:string   val)]
        [(symbol? val)        (make-literal type:symbol   val)]
        [(time-utc? val)      (make-literal type:time-utc val)]
        [(time-tai? val)      (make-literal type:time-tai val)]
        [(guid? val)          (make-literal (entity-make-guid-type (guid-entity val) #f) val)]
        [(snooze-struct? val) (create-literal (snooze-struct-guid val))]
        [else                 (raise-type-error 'create-literal
                                                "(U boolean integer real string symbol time-tai time-utc)"
                                                val)]))

; type -> literal
(define (create-null type)
  (make-literal type (type-null type)))

; Ordering ---------------------------------------

; (struct expression (U 'asc 'desc))
(define-serializable-struct order (expression direction) #:transparent)

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
;         (U symbol #f (listof (U symbol #f))))
(define-serializable-struct query
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
  #:transparent)

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
 [struct (source-alias source)         ([name symbol?])]
 [struct (entity-alias source-alias)   ([name symbol?] [entity-name symbol?])]
 [entity-alias-entity                  (-> entity-alias? entity?)]
 [struct (query-alias source-alias)    ([name symbol?] [query query?])]
 [struct (join source)                 ([op symbol?] [left source?] [right source?] [on (or/c expression? #f)])]
 [struct expression                    ([type type?])]
 [struct (column expression)           ([type type?] [name symbol?])]
 [create-attribute-alias               (-> entity-alias? attribute? attribute-alias?)]
 [attribute-alias-attribute            (-> attribute-alias? attribute?)]
 [struct (function expression)         ([type type?] [op symbol?] [args (listof function-arg/c)])]
 [struct (aggregate function)          ([type type?] [op symbol?] [args (listof function-arg/c)])]
 [struct order                         ([expression expression?] [direction (symbols 'asc 'desc)])]
 [struct query                         ([what             (listof column?)]
                                        [distinct         (or/c (listof expression?) #f)]
                                        [from             source?]
                                        [where            (or/c expression? #f)]
                                        [group            (listof expression?)]
                                        [order            (listof order?)]
                                        [having           (or/c expression? #f)]
                                        [limit            (or/c integer? #f)]
                                        [offset           (or/c integer? #f)]
                                        [local-columns    (listof column?)]
                                        [imported-columns (listof column?)]
                                        [extract-info     (or/c symbol? #f (listof (or/c symbol? #f)))])]
 [source-alias-columns                 (-> source-alias? (listof column?))])
