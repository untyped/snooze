#lang scheme/base

(require "../base.ss")

(require (unlib-in list)
         "attribute-keyword.ss"
         "core.ss")

; any -> boolean
(define (snooze-struct? struct)
  (prop:entity-set? struct))

; snooze-struct -> entity
(define snooze-struct-entity
  prop:entity-ref)

; snooze-struct -> guid
(define (snooze-struct-guid struct)
  ((entity-private-accessor (snooze-struct-entity struct)) struct 0))

; snooze-struct -> (U natural #f)
(define (snooze-struct-id struct)
  (let ([guid (snooze-struct-guid struct)])
    (and guid (guid-id guid))))

; snooze-struct -> boolean
(define (snooze-struct-saved? struct)
  (and (snooze-struct-guid struct) #t))

; snooze-struct -> (U natural #f)
(define (snooze-struct-revision struct)
  ((entity-private-accessor (snooze-struct-entity struct)) struct 1))

; snooze-struct (U symbol attribute) -> any
(define (snooze-struct-ref struct name+attr)
  (let* ([entity (snooze-struct-entity struct)]
         [attr   (if (attribute? name+attr)
                     name+attr
                     (entity-attribute entity name+attr))])
    ((attribute-private-accessor attr) struct)))

; snooze-struct boolean -> any
(define (snooze-struct-ref* struct)
  (cdr (vector->list (struct->vector struct))))

; snooze-struct <attr any> ... -> snooze-struct
(define (snooze-struct-set original . args)
  (let*-values ([(entity)             (snooze-struct-entity original)]
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
  (apply (entity-private-constructor (snooze-struct-entity original))
         (snooze-struct-ref* original)))

; Provide statements -----------------------------

(define attr-value-list/c
  (cons/c (or/c vanilla-guid? #f) any/c))

(provide/contract
 [snooze-struct?              (-> any/c boolean?)]
 [snooze-struct-entity        (-> (or/c snooze-struct? prop:entity-set?) entity?)]
 [snooze-struct-guid          (-> (or/c snooze-struct? prop:entity-set?) (or/c vanilla-guid? #f))]
 [snooze-struct-id            (-> snooze-struct? (or/c natural-number/c #f))]
 [snooze-struct-saved?        (-> snooze-struct? boolean?)]
 [snooze-struct-revision      (-> snooze-struct? (or/c natural-number/c #f))]
 [snooze-struct-ref           (-> snooze-struct? (or/c attribute? symbol?) any)]
 [snooze-struct-ref*          (-> snooze-struct? list?)]
 [snooze-struct-set           (->* (snooze-struct?) () #:rest attr/value-list? snooze-struct?)]
 [make-snooze-struct          (->* (entity?) () #:rest attr-value-list/c snooze-struct?)]
 [make-snooze-struct/defaults (->* (entity?)
                                   (#:snooze (is-a?/c snooze<%>))
                                   #:rest attr/value-list?
                                   snooze-struct?)]
 [copy-snooze-struct          (-> snooze-struct? snooze-struct?)])
