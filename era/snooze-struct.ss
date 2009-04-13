#lang scheme/base

(require "../base.ss")

(require (unlib-in list)
         "core.ss")

; any -> boolean
(define (snooze-struct? struct)
  (prop:entity-set? struct))

; snooze-struct -> entity
(define struct-entity prop:entity-ref)

; snooze-struct -> guid
(define (struct-guid struct)
  ((entity-private-accessor (struct-entity struct)) struct 0))

; snooze-struct -> (U natural #f)
(define (struct-id struct)
  (guid-id (struct-guid struct)))

; snooze-struct (U natural #f) -> void
(define (set-struct-id! struct id)
  (set-guid-id! (struct-guid struct) id))

; snooze-struct -> boolean
(define (struct-saved? struct)
  (and (guid-id (struct-guid struct)) #t))

; snooze-struct -> (U natural #f)
(define (struct-revision struct)
  ((entity-private-accessor (struct-entity struct)) struct 1))

; snooze-struct -> (U natural #f)
(define (set-struct-revision! struct val)
  ((entity-private-mutator (struct-entity struct)) struct 1 val))

; snooze-struct (U symbol attribute) -> any
(define (snooze-struct-ref struct name+attr)
  (let* ([entity (struct-entity struct)]
         [attr   (if (attribute? name+attr)
                     name+attr
                     (entity-attribute entity name+attr))])
    ((attribute-private-accessor attr) struct)))

; snooze-struct [#:copy-guid? boolean] -> any
(define (snooze-struct-ref* struct #:copy-guid? [copy-guid? #f])
  (let ([ans (cdr (vector->list (struct->vector struct)))])
    (if copy-guid?
        (cons (entity-make-guid (struct-entity struct) (guid-id (car ans)))
              (cdr ans))
        ans)))

; snooze-struct (U symbol attribute) any -> void
(define (snooze-struct-set! struct name+attr val)
  (let* ([entity  (struct-entity struct)]
         [attr    (if (attribute? name+attr)
                      name+attr
                      (entity-attribute entity name+attr))])
    (if (entity-guid-attribute? entity attr)
        (set-guid-id! (struct-guid struct) (guid-id val))
        (let ([mutator (attribute-private-mutator attr)])
          (if mutator
              (mutator struct val)
              (error "immutable attribute" attr))))))

; snooze-struct (listof any) -> void
(define (snooze-struct-set*! struct vals)
  (let* ([entity (struct-entity struct)]
         [attrs  (entity-attributes entity)])
    (unless (= (length vals) (length attrs))
      (raise-type-error 'snooze-struct-set!
                        (format "list of ~a values" (length attrs))
                        vals))
    (for ([attr (in-list attrs)]
          [val  (in-list values)])
      (snooze-struct-set! struct attr val))))

; entity <attr any> ... -> struct
(define (make-snooze-struct/defaults entity . args)
  (let-values ([(arg-attrs arg-vals) (check-attribute-keywords entity args)])
    (apply (entity-private-constructor entity)
           (for/list ([attr (in-list (entity-attributes entity))])
             (attribute-keyword-get
              attr
              arg-attrs
              arg-vals
              (type-default (attribute-type attr)))))))

; snooze-struct <attr any> ... -> guid
(define (copy-snooze-struct original . args)
  (let*-values ([(entity)             (struct-entity original)]
                [(arg-attrs arg-vals) (check-attribute-keywords entity args)]
                [(attrs)              (entity-attributes entity)]
                [(existing)           (snooze-struct-ref* original #:copy-guid? #t)])
    (apply (entity-private-constructor entity)
           (for/list ([attr     (in-list attrs)]
                      [existing (in-list existing)])
             (attribute-keyword-get
              attr
              arg-attrs
              arg-vals
              existing)))))

; snooze-struct snooze-struct -> void
(define (update-snooze-struct-from-copy! struct copy)
  (let ([entity (struct-entity struct)])
    (unless (equal? entity (struct-entity copy))
      (raise-type-error 'update-snooze-struct-from-copy!
                        "arguments are of different types"
                        (list struct copy)))
    (snooze-struct-set*! struct (snooze-struct-ref* copy))))

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

(provide attr/value-list?)

(provide/contract
 [snooze-struct?                  (-> any/c boolean?)]
 [struct-entity                   (-> (or/c snooze-struct? prop:entity-set?) entity?)]
 [struct-guid                     (-> (or/c snooze-struct? prop:entity-set?) guid?)]
 [struct-id                       (-> snooze-struct? (or/c natural-number/c #f))]
 [set-struct-id!                  (-> snooze-struct? (or/c natural-number/c #f) void?)]
 [struct-saved?                   (-> snooze-struct? boolean?)]
 [struct-revision                 (-> snooze-struct? (or/c natural-number/c #f))]
 [set-struct-revision!            (-> snooze-struct? (or/c natural-number/c #f) void?)]
 [snooze-struct-ref               (-> snooze-struct? (or/c attribute? symbol?) any)]
 [snooze-struct-ref*              (->* (snooze-struct?) (#:copy-guid? boolean?) list?)]
 [snooze-struct-set!              (-> snooze-struct? (or/c attribute? symbol?) any/c void?)]
 [snooze-struct-set*!             (-> snooze-struct? list? void?)]
 [make-snooze-struct/defaults     (->* (entity?) () #:rest attr/value-list? snooze-struct?)]
 [copy-snooze-struct              (->* (snooze-struct?) () #:rest attr/value-list? snooze-struct?)]
 [update-snooze-struct-from-copy! (-> snooze-struct? snooze-struct? void?)])
