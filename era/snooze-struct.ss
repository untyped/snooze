#lang scheme/base

(require "../base.ss")

(require (unlib-in list)
         "attribute-keyword.ss"
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
  (and (struct-guid struct)
       (guid-id (struct-guid struct))))

; snooze-struct -> boolean
(define (struct-saved? struct)
  (and (struct-guid struct) #t))

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

; snooze-struct boolean -> any
(define (snooze-struct-ref* struct)
  (cdr (vector->list (struct->vector struct))))

; snooze-struct <attr any> ... -> snooze-struct
(define (snooze-struct-set original . args)
  (let*-values ([(entity)             (struct-entity original)]
                [(arg-attrs arg-vals) (check-attribute-keywords entity args)]
                [(attrs)              (entity-attributes entity)]
                [(existing)           (snooze-struct-ref* original)])
    (apply (entity-private-constructor entity)
           (for/list ([attr     (in-list attrs)]
                      [existing (in-list existing)])
             (attribute-keyword-get
              entity
              attr
              arg-attrs
              arg-vals
              (lambda (attr) existing))))))

; snooze-struct (U symbol attribute) any -> void
(define (snooze-struct-set! struct name+attr val)
  (let* ([entity  (struct-entity struct)]
         [attr    (if (attribute? name+attr)
                      name+attr
                      (entity-attribute entity name+attr))])
    ((attribute-private-mutator attr) struct val)))

; snooze-struct (listof any) -> void
(define (snooze-struct-set*! struct vals)
  (let* ([entity (struct-entity struct)]
         [attrs  (entity-attributes entity)])
    (unless (= (length vals) (length attrs))
      (raise-type-error 'snooze-struct-set!
                        (format "list of ~a values" (length attrs))
                        vals))
    (for ([attr (in-list attrs)]
          [val  (in-list vals)])
      (snooze-struct-set! struct attr val))))

; snooze-struct -> snooze-struct
(define (make-snooze-struct entity . args)
  (apply (entity-private-constructor entity) args))

; entity <attr any> ... -> struct
(define (make-snooze-struct/defaults #:snooze [snooze (current-snooze)] entity . args)
  (let-values ([(arg-attrs arg-vals) (check-attribute-keywords entity args)])
    (apply (entity-private-constructor entity)
           (for/list ([attr (in-list (entity-attributes entity))])
             (attribute-keyword-get
              entity
              attr
              arg-attrs
              arg-vals
              (lambda (attr)
                (attribute-default #:snooze snooze attr)))))))

; snooze-struct -> snooze-struct
(define (copy-snooze-struct original)
  (apply (entity-private-constructor (struct-entity original))
         (snooze-struct-ref* original)))

; Provide statements -----------------------------

(provide/contract
 [snooze-struct?              (-> any/c boolean?)]
 [struct-entity               (-> (or/c snooze-struct? prop:entity-set?) entity?)]
 [struct-guid                 (-> (or/c snooze-struct? prop:entity-set?) (or/c guid? #f))]
 [struct-id                   (-> snooze-struct? (or/c natural-number/c #f))]
 [struct-saved?               (-> snooze-struct? boolean?)]
 [struct-revision             (-> snooze-struct? (or/c natural-number/c #f))]
 [set-struct-revision!        (-> snooze-struct? (or/c natural-number/c #f) void?)]
 [snooze-struct-ref           (-> snooze-struct? (or/c attribute? symbol?) any)]
 [snooze-struct-ref*          (-> snooze-struct? list?)]
 [snooze-struct-set           (->* (snooze-struct?) () #:rest attr/value-list? snooze-struct?)]
 [snooze-struct-set!          (-> snooze-struct? (or/c attribute? symbol?) any/c void?)]
 [snooze-struct-set*!         (-> snooze-struct? list? void?)]
 [make-snooze-struct          (->* (entity?) () #:rest any/c snooze-struct?)]
 [make-snooze-struct/defaults (->* (entity?)
                                   (#:snooze (is-a?/c snooze<%>))
                                   #:rest attr/value-list?
                                   snooze-struct?)]
 [copy-snooze-struct          (-> snooze-struct? snooze-struct?)])
