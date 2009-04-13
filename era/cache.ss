#lang scheme/base

(require "../base.ss")

(require (unlib-in list parameter)
         "cache-internal.ss"
         "core.ss"
         (prefix-in real: "snooze-struct.ss"))

; Cache wrappers ---------------------------------

; [cache] -> cache
(define (create-cache [parent (current-cache)])
  (make-cache parent (make-weak-hasheq)))

; guid [cache] -> snooze-struct
(define (cache-ref guid [cache (current-cache)])
  (cond [(hash-ref (cache-data cache) guid #f)
         => (lambda (local) local)]
        [(cache-parent cache)
         => (lambda (parent)
              (let* ([remote (cache-ref guid parent)]
                     [local  (copy-snooze-struct remote)])
                (cache-set! guid local cache)
                local))]
        [else (error "struct not cached" guid)]))

; guid snooze-struct [cache] -> guid
(define (cache-set! guid struct [cache (current-cache)])
  (hash-set! (cache-data cache) guid struct)
  guid)

; snooze-struct [cache] -> guid
(define (cache-add! struct [cache (current-cache)])
  (if (hash-ref (cache-data cache) (real:struct-guid struct) #f)
      (error "struct already cached" struct)
      (cache-set! (struct-guid struct) struct cache)))

; Current cache ----------------------------------

; (parameter snooze-cache)
(define current-cache
  (make-parameter (create-cache #f)))

; (-> any) -> any
(define (call-with-cache thunk)
  (parameterize ([current-cache (create-cache)])
    (thunk)))

; Snooze struct wrappers -------------------------

; (any ... -> snooze-struct) natural attribute attribute -> (any ... -> guid)
(define (make-cached-constructor
         procedure-name
         struct-constructor
         expected-arity
         guid-attribute
         revision-attribute)
  (lambda args
    (unless (= (length args) expected-arity)
      (raise-exn exn:fail:contract
        (format "~a: expected ~a argument(s), received ~s"
                procedure-name
                expected-arity
                args)))
    (cache-add! (apply struct-constructor
                       (type-default (attribute-type guid-attribute))
                       (type-default (attribute-type revision-attribute))
                       args))))

; (any -> boolean) -> (any -> boolean)
(define (make-cached-predicate struct-predicate)
  (lambda (guid)
    (struct-predicate (cache-ref guid))))

; (struct -> any) -> (guid -> any)
(define (make-cached-accessor struct-accessor)
  (lambda (guid)
    (struct-accessor (cache-ref guid))))

; (struct any -> void) -> (guid any -> void)
(define (make-cached-mutator struct-mutator)
  (lambda (guid val)
    (struct-mutator (cache-ref guid) val)))

; Standard wrappers ------------------------------

; any -> boolean
(define (snooze-struct? guid)
  (real:snooze-struct? (cache-ref guid)))

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

; snooze-struct -> boolean
(define (struct-saved? guid)
  (and (guid-id guid) #t))

; guid -> (U natural #f)
(define (struct-revision guid)
  (real:struct-revision (cache-ref guid)))

; guid -> (U natural #f)
(define (set-struct-revision! guid val)
  (real:set-struct-revision! (cache-ref guid) val))

; guid (U symbol attribute) -> any
(define (snooze-struct-ref guid name+attr)
  (real:snooze-struct-ref (cache-ref guid) name+attr))

; guid -> any
(define (snooze-struct-ref* guid #:copy-guid? [copy-guid? #f])
  (real:snooze-struct-ref* (cache-ref guid) #:copy-guid? copy-guid?))

; guid (U symbol attribute) any -> void
(define (snooze-struct-set! guid name+attr val)
  (real:snooze-struct-set! (cache-ref guid) name+attr val))

; guid (listof any) -> void
(define (snooze-struct-set*! guid vals)
  (real:snooze-struct-set*! (cache-ref guid) vals))

; entity <attr any> ... -> guid
(define (make-snooze-struct/defaults entity . args)
  (cache-add! (apply real:make-snooze-struct/defaults entity args)))

; guid <attr any> ... -> guid
(define (copy-snooze-struct original . args)
  (cache-add! (apply real:copy-snooze-struct (cache-ref original) args)))

; guid guid -> void
(define (update-snooze-struct-from-copy! guid copy)
  (real:update-snooze-struct-from-copy! (cache-ref guid) (cache-ref copy)))

; Provide statements -----------------------------

(provide (except-out (all-from-out "cache-internal.ss") make-cache))

(provide/contract
 [rename create-cache make-cache  (->* () ((or/c cache? #f)) cache?)]
 [cache-ref                       (->* (guid?) (cache?) prop:entity-set?)]
 [cache-set!                      (->* (guid? real:snooze-struct?) (cache?) void?)]
 [current-cache                   (parameter/c cache?)]
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
 [struct-revision                 (-> guid? (or/c natural-number/c #f))]
 [set-struct-revision!            (-> guid? (or/c natural-number/c #f) void?)]
 [snooze-struct-ref               (-> guid? (or/c attribute? symbol?) any)]
 [snooze-struct-ref*              (->* (guid?) (#:copy-guid? boolean?) list?)]
 [snooze-struct-set!              (-> guid? (or/c attribute? symbol?) any/c void?)]
 [snooze-struct-set*!             (-> guid? list? void?)]
 [make-snooze-struct/defaults     (->* (entity?) () #:rest real:attr/value-list? guid?)]
 [copy-snooze-struct              (->* (guid?) () #:rest real:attr/value-list? guid?)]
 [update-snooze-struct-from-copy! (-> guid? guid? void?)])
