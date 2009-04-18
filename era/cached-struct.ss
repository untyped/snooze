#lang scheme/base

(require "../base.ss")

(require (unlib-in list parameter)
         "core.ss"
         (prefix-in real: "snooze-struct.ss"))

; Snooze struct wrappers -------------------------

; entity symbol (any ... -> snooze-struct) natural -> ([#:snooze snooze-cache<%>] any ... -> guid)
(define (make-cached-constructor
         entity
         procedure-name
         struct-constructor
         expected-arity)
  (lambda (#:snooze [snooze (current-snooze)]
                    #:guid     [guid     (entity-make-guid entity #:snooze snooze #f)]
                    #:revision [revision #f] . args)
    (unless (= (length args) expected-arity)
      (raise-exn exn:fail:contract
        (format "~a: expected ~a non-keyword argument(s), received ~s"
                procedure-name
                expected-arity
                args)))
    (send snooze cache-add! (apply struct-constructor guid revision args))))

; (any -> boolean) -> (any -> boolean)
(define (make-cached-predicate struct-predicate)
  (lambda (guid)
    (struct-predicate (guid-cache-ref guid))))

; (struct -> any) -> (guid -> any)
(define (make-cached-accessor struct-accessor)
  (lambda (guid)
    (struct-accessor (guid-cache-ref guid))))

; (struct any -> void) -> (guid any -> void)
(define (make-cached-mutator struct-mutator)
  (lambda (guid val)
    (struct-mutator (guid-cache-ref guid) val)))

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

; guid -> (U natural #f)
(define (struct-revision guid)
  (real:struct-revision (guid-cache-ref guid)))

; guid -> (U natural #f)
(define (set-struct-revision! guid val)
  (real:set-struct-revision! (guid-cache-ref guid) val))

; guid (U symbol attribute) -> any
(define (snooze-struct-ref guid name+attr)
  (real:snooze-struct-ref (guid-cache-ref guid) name+attr))

; guid [#:copy-guid? boolean] -> any
(define (snooze-struct-ref* guid #:copy-guid? [copy-guid? #f])
  (real:snooze-struct-ref* (guid-cache-ref guid) #:copy-guid? copy-guid?))

; guid (U symbol attribute) any -> void
(define (snooze-struct-set! guid name+attr val)
  (real:snooze-struct-set! (guid-cache-ref guid) name+attr val))

; guid (listof any) -> void
(define (snooze-struct-set*! guid vals)
  (real:snooze-struct-set*! (guid-cache-ref guid) vals))

; entity <attr any> ... -> guid
(define (make-snooze-struct/defaults entity . args)
  (struct-cache-add! (apply real:make-snooze-struct/defaults entity args)))

; guid <attr any> ... -> guid
(define (copy-snooze-struct original . args)
  (struct-cache-add! (apply real:copy-snooze-struct (guid-cache-ref original) args)))

; guid guid -> void
(define (update-snooze-struct-from-copy! guid copy)
  (real:update-snooze-struct-from-copy! (guid-cache-ref guid) (guid-cache-ref copy)))

; Helpers ----------------------------------------

; snooze-struct -> guid
(define (struct-cache-add! struct)
  (send (guid-snooze (real:struct-guid struct)) cache-add! struct))

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
 [set-struct-id!                  (-> guid? (or/c natural-number/c #f) void?)]
 [struct-saved?                   (-> guid? boolean?)]
 [struct-revision                 (-> guid? (or/c natural-number/c #f))]
 [set-struct-revision!            (-> guid? (or/c natural-number/c #f) void?)]
 [snooze-struct-ref               (-> guid? (or/c attribute? symbol?) any)]
 [snooze-struct-ref*              (->* (guid?) (#:copy-guid? boolean?) list?)]
 [snooze-struct-set!              (-> guid? (or/c attribute? symbol?) any/c void?)]
 [snooze-struct-set*!             (-> guid? list? void?)]
 [make-snooze-struct/defaults     (->* (entity?) () #:rest real:attr/value-list? guid?)]
 [copy-snooze-struct              (->* (guid?) () #:rest real:attr/value-list? guid?)]
 [update-snooze-struct-from-copy! (-> guid? guid? void?)])
