#lang scheme/base

(require "../base.ss")

(require (unlib-in list parameter)
         "cache-internal.ss"
         "core.ss")

; Cache wrappers ---------------------------------

; [snooze-cache] -> snooze-cache
(define (create-cache [parent #f])
  (make-cache parent (make-weak-hasheq)))

; snooze-cache guid -> snooze-struct
(define (cache-ref cache guid)
  (cond [(hash-ref (cache-data cache) guid #f)
         => (lambda (local) local)]
        [(cache-parent cache)
         => (lambda (parent)
              (let* ([remote (cache-ref parent guid)]
                     [local  (copy-snooze-struct remote)])
                (cache-set! cache guid local)
                local))]
        [else (error "struct not cached" guid)]))

; cache guid snooze-struct -> guid
(define (cache-set! cache guid struct)
  (hash-set! (cache-data cache) guid struct)
  guid)

; Current cache ----------------------------------

; (parameter snooze-cache)
(define current-cache
  (make-parameter (create-cache #f)))

; (-> any) -> any
(define (call-with-cache thunk)
  (parameterize ([current-cache (create-cache (current-cache))])
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
      (raise-type-error procedure-name
                        (format "expected ~a arguments" expected-arity)
                        args))
    (let* ([struct (apply struct-constructor
                          (type-default (attribute-type guid-attribute))
                          (type-default (attribute-type revision-attribute))
                          args)]
           [guid   ((entity-private-accessor (prop:entity-ref struct)) struct 0)])
      (cache-set! (current-cache) guid struct)
      guid)))

; (any -> boolean) -> (any -> boolean)
(define (make-cached-predicate struct-predicate)
  (lambda (guid)
    (struct-predicate (cache-ref (current-cache) guid))))

; (struct -> any) -> (guid -> any)
(define (make-cached-accessor struct-accessor)
  (lambda (guid)
    (struct-accessor (cache-ref (current-cache) guid))))

; (struct any -> void) -> (guid any -> void)
(define (make-cached-mutator struct-mutator)
  (lambda (guid val)
    (struct-mutator (cache-ref (current-cache) guid) val)))

; Standard wrappers ------------------------------

; any -> boolean
(define (snooze-struct? guid)
  (guid? guid))

; (U snooze-struct guid) -> entity
(define (struct-entity struct+guid)
  (if (guid? struct+guid)
      (guid-entity struct+guid)
      (prop:entity-ref struct+guid)))

; (U snooze-struct guid) -> guid
(define (struct-guid struct+guid)
  (if (guid? struct+guid)
      struct+guid
      ((entity-private-accessor (prop:entity-ref struct+guid)) struct+guid 0)))

; guid -> (U natural #f)
(define struct-id guid-id)

; guid (U natural #f) -> void
(define set-struct-id! set-guid-id!)

; snooze-struct -> boolean
(define (struct-saved? guid)
  (and (guid-id guid) #t))

; guid -> (U natural #f)
(define (struct-revision guid)
  (let ([struct (cache-ref (current-cache) guid)])
    ((entity-private-accessor (guid-entity guid)) struct 1)))

; guid -> (U natural #f)
(define (set-struct-revision! guid val)
  (let ([struct (cache-ref (current-cache) guid)])
    ((entity-private-mutator (guid-entity guid)) struct 1 val)))

; guid (U symbol attribute) -> any
(define (snooze-struct-ref guid name+attr)
  (let* ([struct (cache-ref (current-cache) guid)]
         [entity (guid-entity guid)]
         [attr   (if (attribute? name+attr)
                     name+attr
                     (entity-attribute entity name+attr))])
    ((attribute-private-accessor attr) struct)))

; guid -> any
(define (snooze-struct-ref* guid)
  (cdr (vector->list (struct->vector (cache-ref (current-cache) guid)))))

; guid (U symbol attribute) any -> void
(define (snooze-struct-set! guid name+attr val)
  (let* ([struct  (cache-ref (current-cache) guid)]
         [entity  (guid-entity guid)]
         [attr    (if (attribute? name+attr)
                      name+attr
                      (entity-attribute entity name+attr))]
         [mutator (attribute-private-mutator attr)])
    (if mutator
        (mutator struct val)
        (error "immutable attribute" attr))))

; guid (listof any) -> void
(define (snooze-struct-set*! guid vals)
  (let* ([struct (cache-ref (current-cache) guid)]
         [entity (guid-entity guid)]
         [attrs  (entity-attributes entity)])
    (unless (= (length vals) (length attrs))
      (raise-type-error 'snooze-struct-set!
                        (format "list of ~a values" (length attrs))
                        vals))
    (set-guid-id! guid (guid-id (car vals)))
    (for ([attr (in-list (cdr attrs))]
          [val  (in-list (cdr values))])
      (snooze-struct-set! struct attr val))))

; entity <attr any> ... -> guid
(define (make-snooze-struct/defaults entity . args)
  (let-values ([(arg-attrs arg-vals) (check-attribute-keywords entity args)])
    (apply (entity-cached-constructor entity)
           (for/list ([attr (in-list (cddr (entity-attributes entity)))])
             (attribute-keyword-get
              attr
              arg-attrs
              arg-vals
              (type-default (attribute-type attr)))))))

; guid <attr any> ... -> guid
(define (copy-snooze-struct original . args)
  (let*-values ([(entity)             (guid-entity original)]
                [(arg-attrs arg-vals) (check-attribute-keywords entity args)]
                [(attrs)              (entity-attributes entity)]
                [(existing)           (snooze-struct-ref* original)])
    (let ([struct (apply (entity-private-constructor entity)
                         ((entity-guid-constructor entity) (guid-id original))
                         (struct-revision original)
                         (for/list ([attr     (in-list (cddr attrs))]
                                    [existing (in-list (cddr existing))])
                           (attribute-keyword-get
                            attr
                            arg-attrs
                            arg-vals
                            existing)))])
      (cache-set! (current-cache) struct))))

; guid guid -> void
(define (update-snooze-struct-from-copy! guid copy)
  (let ([entity (guid-entity guid)])
    (unless (equal? entity (guid-entity copy))
      (raise-type-error 'update-snooze-struct-from-copy!
                        "arguments are of different types"
                        (list guid copy)))
    (snooze-struct-set*! guid (snooze-struct-ref* copy))))

; Helpers --------------------------------------

; attribute attribute -> boolean
(define (attribute-name-equal? attr1 attr2)
  (equal? (attribute-name attr1)
          (attribute-name attr2)))

; entity (alternating-listof (U symbol attribute) any) -> (listof attribute) (listof any)
(define (check-attribute-keywords entity args)
  ; boolean (listof (U attribute any)) (listof attribute) (listof any) -> (listof attribute) (listof any)
  (let loop ([even? #f] [args args] [attrs-accum null] [vals-accum null])
    (if even?
        ; Attribute value:
        (if (null? args)
            (raise-type-error 'check-attribute-keywords "no value for attribute" (car attrs-accum))
            (let ([attr (car attrs-accum)]
                  [val  (car args)])
              (if (attribute? val)
                  (raise-type-error 'check-attribute-keywords "no value for attribute" (car attrs-accum))
                  (loop (not even?) (cdr args) attrs-accum (cons val vals-accum)))))
        ; Attribute:
        (if (null? args)
            (values (reverse attrs-accum)
                    (reverse vals-accum))
            (let* ([attr+name (car args)]
                   [attr      (cond [(attribute? attr+name) (entity-attribute entity attr+name)]
                                    [(symbol? attr+name)    (entity-attribute entity attr+name)]
                                    [else                   (raise-type-error
                                                             'check-attribute-keywords
                                                             (if (null? attrs-accum)
                                                                 "no attribute for value"
                                                                 "multiple values for attribute")
                                                             (if (null? attrs-accum)
                                                                 attr+name
                                                                 (car attrs-accum)))])])
              (if (for/or ([attr* (in-list attrs-accum)])
                    (and (attribute-name-equal? attr attr*) attr*))
                  (raise-type-error 'check-attribute-keywords "attribute specified more than once" attr)
                  (loop (not even?) (cdr args) (cons attr attrs-accum) vals-accum)))))))


; attribute (listof attribute) (listof any) any -> any
;
; We search for attributes by name so we don't have to worry that, for example,
; attr:struct-guid and attr:person-guid are not equal. Note that we are reliant on
; check-attribute-keywords to chuck out attributes from other entities, otherwise
; we might get confused between, for example, attr:person-name and attr:pet-name.
(define (attribute-keyword-get needle attrs vals default)
  (let/ec return
    (for ([attr attrs] [val vals])
      (when (eq? needle attr)
        (return val)))
    default))

; any -> boolean
(define (attr/value-list? item)
  (or (null? item)
      (and (pair? item)
           (or (attribute? (car item))
               (symbol? (car item)))
           (value/attr-list? (cdr item)))))

; any -> boolean
(define (value/attr-list? item)
  (and (pair? item)
       (attr/value-list? (cdr item))))

; Provide statements -----------------------------

(provide (except-out (all-from-out "cache-internal.ss") make-cache))

(provide/contract
 [rename create-cache make-cache  (-> (or/c cache? #f) cache?)]
 [cache-ref                       (-> cache? guid? snooze-struct?)]
 [cache-set!                      (-> cache? guid? snooze-struct? void?)]
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
 [snooze-struct-ref*              (-> guid? list?)]
 [snooze-struct-set!              (-> guid? (or/c attribute? symbol?) any/c void?)]
 [snooze-struct-set*!             (-> guid? list? void?)]
 [make-snooze-struct/defaults     (->* (entity?) () #:rest attr/value-list? guid?)]
 [copy-snooze-struct              (->* (guid?) () #:rest attr/value-list? guid?)]
 [update-snooze-struct-from-copy! (-> guid? guid? void?)])
