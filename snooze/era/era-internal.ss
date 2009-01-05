#lang scheme/base

(require "../base.ss"
         "era-struct.ss")

; Properties -------------------------------------

; property
; any -> boolean
; any -> (U entity #f)
(define-values (prop:entity struct-has-entity? struct-entity)
  (make-struct-type-property 'entity))

; Constants --------------------------------------

; symbol
;
; This is defined for use in the type-null procedure.
; DO NOT PROVIDE THIS OUTSIDE THIS FILE!
;
; Allowing people to set boolean fields to NULL
; will cause all sorts of problems.
(define boolean-null (gensym 'boolean-null))

; type
(define type:id       (make-integer-type  #t #f))
(define type:revision (make-integer-type  #f #f))
(define type:boolean  (make-boolean-type #t #f))
(define type:integer  (make-integer-type #t #f))
(define type:real     (make-real-type #t #f))
(define type:string   (make-string-type #t #f #f))
(define type:symbol   (make-symbol-type #t #f #f))
(define type:time-tai (make-time-tai-type #t #f))
(define type:time-utc (make-time-utc-type #t #f))

; Accessors --------------------------------------

; entity (U symbol attribute) -> boolean
(define (entity-has-attribute? entity name+attr)
  (if (attribute? name+attr)
      (eq? (attribute-entity name+attr) entity)
      (ormap (lambda (attr)
               (eq? (attribute-name attr) name+attr))
             (entity-attributes entity))))

; entity (U symbol attribute) -> attribute
(define (entity-attribute entity name+attr)
  (or (if (attribute? name+attr)
          (and (eq? (attribute-entity name+attr) entity)
               name+attr)
          (ormap (lambda (attr)
                   (and (eq? (attribute-name attr) name+attr)
                        attr))
                 (entity-attributes entity)))
      (raise-exn exn:fail:contract
        (format "Attribute not found: ~s ~s" entity name+attr))))

; type -> any
(define (type-null type)
  (if (boolean-type? type)
      boolean-null
      #f))

; type any -> boolean
(define (type-valid? type value)
  (cond [(equal? value (type-null type)) (type-allows-null? type)]
        [(boolean-type? type)            (boolean? value)]
        [(integer-type? type)            (integer? value)]
        [(real-type? type)               (real? value)]
        [(string-type? type)             (let ([max-length (string-type-max-length type)])
                                           (and (string? value)
                                                (or (not max-length)
                                                    (<= (string-length value) max-length))))]
        [(symbol-type? type)             (let ([max-length (string-type-max-length type)])
                                           (and (symbol? value)
                                                (or (not max-length)
                                                    (<= (string-length (symbol->string value)) max-length))))]
        [(time-tai-type? type)           (time-tai? value)]
        [(time-utc-type? type)           (time-utc? value)]))

; type -> symbol
(define (type-name type)
  (cond [(boolean-type? type)  'boolean]
        [(integer-type? type)  'integer]
        [(real-type? type)     'real]
        [(string-type? type)   'string]
        [(symbol-type? type)   'symbol]
        [(time-utc-type? type) 'time-utc]
        [(time-tai-type? type) 'time-tai]))

; type type -> boolean
;
; Returns #t if the arguments are of the same class of type (boolean types, integer types, etc).
(define (type-compatible? type1 type2)
  (cond [(boolean-type? type1)  (boolean-type? type2)]
        [(integer-type? type1)  (integer-type? type2)]
        [(real-type? type1)     (real-type? type2)]
        [(string-type? type1)   (string-type? type2)]
        [(symbol-type? type1)   (symbol-type? type2)]
        [(time-utc-type? type1) (time-utc-type? type2)]
        [(time-tai-type? type1) (time-tai-type? type2)]))

; Provide statements -----------------------------

(provide/contract
 [type:id                     integer-type?]
 [type:revision               integer-type?]
 [type:boolean                boolean-type?]
 [type:integer                integer-type?]
 [type:real                   real-type?]
 [type:string                 string-type?]
 [type:symbol                 symbol-type?]
 [type:time-tai               time-tai-type?]
 [type:time-utc               time-utc-type?]
 [type-null                   (-> type? any)]
 [type-valid?                 (-> type? any/c boolean?)]
 [type-name                   (-> type? symbol?)]
 [type-compatible?            (-> type? type? boolean?)]
 [prop:entity                 struct-type-property?]
 [struct-has-entity?          procedure?]
 [struct-entity               procedure?]
 [entity-has-attribute?       (-> entity? (or/c symbol? attribute?) boolean?)]
 [entity-attribute            (-> entity? (or/c symbol? attribute?) attribute?)])
