#lang scheme/base

(require "../base.ss")

(require (unlib-in list parameter)
         "core.ss"
         (prefix-in real: "snooze-struct.ss"))

; Snooze struct wrappers -------------------------

; (any ... -> snooze-struct) natural attribute attribute -> ([#:snooze snooze-cache<%>] any ... -> guid)
(define (make-cached-constructor
         procedure-name
         struct-constructor
         expected-arity
         guid-attribute
         revision-attribute)
  (lambda (#:snooze [snooze (current-snooze)] . args)
    (unless (= (length args) expected-arity)
      (raise-exn exn:fail:contract
        (format "~a: expected ~a argument(s), received ~s"
                procedure-name
                expected-arity
                args)))
    (send snooze cache-add!
          (apply struct-constructor
                 (type-default (attribute-type guid-attribute))
                 (type-default (attribute-type revision-attribute))
                 args))))

; (any -> boolean) -> (any [#:snooze snooze-cache<%>] -> boolean)
(define (make-cached-predicate struct-predicate)
  (lambda (guid #:snooze [snooze (current-snooze)])
    (struct-predicate (send snooze cache-ref guid))))

; (struct -> any) -> (guid [#:snooze snooze-cache<%>] -> any)
(define (make-cached-accessor struct-accessor)
  (lambda (guid #:snooze [snooze (current-snooze)])
    (struct-accessor (send snooze cache-ref guid))))

; (struct any -> void) -> (guid any [#:snooze snooze-cache<%>] -> void)
(define (make-cached-mutator struct-mutator)
  (lambda (guid val #:snooze [snooze (current-snooze)])
    (struct-mutator (send snooze cache-ref guid) val)))

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

; guid (U natural #f) -> void
(define set-struct-id! set-guid-id!)

; guid -> boolean
(define (struct-saved? guid)
  (and (guid-id guid) #t))

; guid [#:snooze snooze-cache<%>] -> (U natural #f)
(define (struct-revision guid #:snooze [snooze (current-snooze)])
  (real:struct-revision (send snooze cache-ref guid)))

; guid [#:snooze snooze-cache<%>] -> (U natural #f)
(define (set-struct-revision! guid val #:snooze [snooze (current-snooze)])
  (real:set-struct-revision! (send snooze cache-ref guid) val))

; guid [#:snooze snooze-cache<%>] (U symbol attribute) -> any
(define (snooze-struct-ref guid name+attr #:snooze [snooze (current-snooze)])
  (real:snooze-struct-ref (send snooze cache-ref guid) name+attr))

; guid [#:snooze snooze-cache<%>] [#:copy-guid? boolean] -> any
(define (snooze-struct-ref* guid #:snooze [snooze (current-snooze)] #:copy-guid? [copy-guid? #f])
  (real:snooze-struct-ref* (send snooze cache-ref guid) #:copy-guid? copy-guid?))

; guid (U symbol attribute) any [#:snooze snooze-cache<%>] -> void
(define (snooze-struct-set! guid name+attr val #:snooze [snooze (current-snooze)])
  (real:snooze-struct-set! (send snooze cache-ref guid) name+attr val))

; guid (listof any) [#:snooze snooze-cache<%>] -> void
(define (snooze-struct-set*! guid vals #:snooze [snooze (current-snooze)])
  (real:snooze-struct-set*! (send snooze cache-ref guid) vals))

; entity [#:snooze snooze-cache<%>] <attr any> ... -> guid
(define (make-snooze-struct/defaults entity #:snooze [snooze (current-snooze)] . args)
  (send snooze cache-add! (apply real:make-snooze-struct/defaults entity args)))

; guid [#:snooze snooze-cache<%>] <attr any> ... -> guid
(define (copy-snooze-struct original #:snooze [snooze (current-snooze)] . args)
  (send snooze cache-add! (apply real:copy-snooze-struct (send snooze cache-ref original) args)))

; guid guid [#:snooze snooze-cache<%>] -> void
(define (update-snooze-struct-from-copy! guid copy #:snooze [snooze (current-snooze)])
  (real:update-snooze-struct-from-copy! (send snooze cache-ref guid) (send snooze cache-ref copy)))

; Provide statements -----------------------------

(provide/contract
 [make-cached-constructor         (-> symbol?
                                      procedure?
                                      natural-number/c
                                      attribute?
                                      attribute?
                                      procedure?)]
 [make-cached-predicate           (-> procedure? procedure?)]
 [make-cached-accessor            (-> procedure? procedure?)]
 [make-cached-mutator             (-> procedure? procedure?)]
 [snooze-struct?                  (-> any/c boolean?)]
 [struct-entity                   (-> (or/c guid? prop:entity-set?) entity?)]
 [struct-guid                     (-> (or/c guid? prop:entity-set?) guid?)]
 [struct-id                       (-> guid? (or/c natural-number/c #f))]
 [set-struct-id!                  (-> guid? (or/c natural-number/c #f) void?)]
 [struct-saved?                   (-> guid? boolean?)]
 [struct-revision                 (->* (guid?)
                                       (#:snooze (is-a?/c snooze-cache<%>))
                                       (or/c natural-number/c #f))]
 [set-struct-revision!            (->* (guid? (or/c natural-number/c #f))
                                       (#:snooze (is-a?/c snooze-cache<%>))
                                       void?)]
 [snooze-struct-ref               (->* (guid? (or/c attribute? symbol?))
                                       (#:snooze (is-a?/c snooze-cache<%>))
                                       any)]
 [snooze-struct-ref*              (->* (guid?)
                                       (#:snooze (is-a?/c snooze-cache<%>) #:copy-guid? boolean?)
                                       list?)]
 [snooze-struct-set!              (->* (guid? (or/c attribute? symbol?) any/c)
                                       (#:snooze (is-a?/c snooze-cache<%>))
                                       void?)]
 [snooze-struct-set*!             (->* (guid? list?)
                                       (#:snooze (is-a?/c snooze-cache<%>))
                                       void?)]
 [make-snooze-struct/defaults     (->* (entity?)
                                       (#:snooze (is-a?/c snooze-cache<%>))
                                       #:rest real:attr/value-list?
                                       guid?)]
 [copy-snooze-struct              (->* (guid?)
                                       (#:snooze (is-a?/c snooze-cache<%>))
                                       #:rest real:attr/value-list?
                                       guid?)]
 [update-snooze-struct-from-copy! (->* (guid? guid?)
                                       (#:snooze (is-a?/c snooze-cache<%>))
                                       void?)])
