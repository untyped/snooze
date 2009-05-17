#lang scheme/base

(require "../base.ss")

(require (unlib-in list parameter)
         "attribute-keyword.ss"
         "core.ss"
         (prefix-in real: "snooze-struct.ss"))

; Snooze struct wrappers -------------------------

; entity symbol (any ... -> snooze-struct) natural -> ([#:snooze snooze-cache<%>] any ... -> guid)
(define (make-cached-constructor
         entity
         procedure-name
         struct-constructor
         expected-arity)
  (lambda (#:snooze [snooze (current-snooze)] . args)
    (unless (= (length args) expected-arity)
      (raise-exn exn:fail:contract
        (format "~a: expected ~a non-keyword argument(s), received ~s"
                procedure-name
                expected-arity
                args)))
    (send snooze cache-add! (apply struct-constructor #f #f args))))

; (any -> boolean) -> (any -> boolean)
(define (make-cached-predicate struct-predicate)
  (lambda (guid)
    (struct-predicate (guid-ref guid))))

; (struct -> any) -> (guid -> any)
(define (make-cached-accessor struct-accessor)
  (lambda (guid)
    (struct-accessor (guid-ref guid))))

; (struct any -> void) -> (guid any -> void)
(define (make-cached-mutator struct-mutator)
  (lambda (guid val)
    (struct-mutator (guid-ref guid) val)))

; Standard wrappers ------------------------------

; any -> boolean
(define snooze-struct? guid?)

; (U snooze-struct guid) -> entity
(define (struct-entity struct+guid)
  (if (guid? struct+guid)
      (guid-entity struct+guid)
      (real:struct-entity struct+guid)))

; (U snooze-struct guid) -> guid
(define (struct-guid struct+guid)
  (if (guid? struct+guid)
      struct+guid
      (real:struct-guid struct+guid)))

; guid -> (U natural #f)
(define struct-id guid-id)

; guid -> boolean
(define struct-local? guid-local?)

; guid -> boolean
(define (struct-saved? guid)
  (and (guid-id guid) #t))

; guid -> (U natural #f)
(define (struct-revision guid)
  (real:struct-revision (guid-ref guid)))

; guid -> (U natural #f)
(define (set-struct-revision! guid val)
  (real:set-struct-revision! (guid-ref guid) val))

; guid (U symbol attribute) -> any
(define (snooze-struct-ref guid name+attr)
  (real:snooze-struct-ref (guid-ref guid) name+attr))

; guid -> any
(define (snooze-struct-ref* guid)
  (real:snooze-struct-ref* (guid-ref guid)))

; guid <attr any> ... -> guid
(define (snooze-struct-set original . args)
  (let*-values ([(entity)             (struct-entity original)]
                 [(arg-attrs arg-vals) (check-attribute-keywords entity args)]
                 [(attrs)              (entity-attributes entity)]
                 [(existing)           (snooze-struct-ref* original)])
    (send (guid-snooze original)
          cache-add!
          (apply (entity-private-constructor entity)
                 (for/list ([attr     (in-list attrs)]
                            [existing (in-list existing)])
                   (attribute-keyword-get
                    entity
                    attr
                    arg-attrs
                    arg-vals
                    (lambda (attr) existing)))))))

; guid (U symbol attribute) any -> void
(define (snooze-struct-set! guid name+attr val)
  (real:snooze-struct-set! (guid-ref guid) name+attr val))

; guid (listof any) -> void
(define (snooze-struct-set*! guid vals)
  (real:snooze-struct-set*! (guid-ref guid) vals))

; entity any ... -> guid
(define (make-snooze-struct #:snooze [snooze (current-snooze)] entity . args)
  (send (current-snooze)
        cache-add!
        (apply real:make-snooze-struct entity args)))

; entity <attr any> ... -> guid
(define (make-snooze-struct/defaults #:snooze [snooze (current-snooze)] entity . args)
  (send (current-snooze)
        cache-add!
        (apply real:make-snooze-struct/defaults entity args)))

; guid -> guid
(define (copy-snooze-struct original)
  (let ([entity (struct-entity original)])
    (send (guid-snooze original)
          cache-add!
          (apply (entity-private-constructor entity)
                 (snooze-struct-ref* original)))))

; guid any ... -> string
(define (snooze-struct-format guid . rest)
  (apply (entity-pretty-formatter (struct-entity guid)) guid rest))

; Provide statements -----------------------------

(provide/contract
 [make-cached-constructor         (-> entity? symbol? procedure? natural-number/c procedure?)]
 [make-cached-predicate           (-> procedure? procedure?)]
 [make-cached-accessor            (-> procedure? procedure?)]
 [make-cached-mutator             (-> procedure? procedure?)]
 [snooze-struct?                  (-> any/c boolean?)]
 [struct-entity                   (-> (or/c guid? prop:entity-set?) entity?)]
 [struct-guid                     (-> (or/c guid? prop:entity-set?) guid?)]
 [struct-id                       (-> guid? (or/c natural-number/c #f))]
 [struct-local?                   (-> guid? boolean?)]
 [struct-saved?                   (-> guid? boolean?)]
 [struct-revision                 (-> guid? (or/c natural-number/c #f))]
 [set-struct-revision!            (-> guid? (or/c natural-number/c #f) void?)]
 [snooze-struct-ref               (-> guid? (or/c attribute? symbol?) any)]
 [snooze-struct-ref*              (-> guid? list?)]
 [snooze-struct-set               (->* (guid?) () #:rest attr/value-list? guid?)]
 [snooze-struct-set!              (-> guid? (or/c attribute? symbol?) any/c void?)]
 [snooze-struct-set*!             (-> guid? list? void?)]
 [make-snooze-struct              (->* (entity?) () #:rest any/c guid?)]
 [make-snooze-struct/defaults     (->* (entity?) () #:rest attr/value-list? guid?)]
 [copy-snooze-struct              (-> guid? guid?)]
 [snooze-struct-format            (->* (guid?) () #:rest any/c string?)])
