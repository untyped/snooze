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
        (cons (entity-make-guid #:snooze (guid-snooze (car ans)) (struct-entity struct) (guid-id (car ans)))
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
          [val  (in-list vals)])
      (snooze-struct-set! struct attr val))))

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
                (if (entity-guid-attribute? entity attr)
                    (entity-make-guid #:snooze snooze entity #f)
                    (attribute-default #:snooze snooze attr))))))))

; [#:snooze snooze] snooze-struct <attr any> ... -> guid
(define (copy-snooze-struct original . args)
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

; snooze-struct snooze-struct -> void
(define (update-snooze-struct-from-copy! struct copy)
  (let ([entity (struct-entity struct)])
    (unless (equal? entity (struct-entity copy))
      (raise-type-error 'update-snooze-struct-from-copy!
                        "arguments are of different types"
                        (list struct copy)))
    (snooze-struct-set*! struct (snooze-struct-ref* copy))))

; Provide statements -----------------------------

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
 [make-snooze-struct/defaults     (->* (entity?)        (#:snooze (is-a?/c snooze-cache<%>)) #:rest attr/value-list? snooze-struct?)]
 [copy-snooze-struct              (->* (snooze-struct?) () #:rest attr/value-list? snooze-struct?)]
 [update-snooze-struct-from-copy! (-> snooze-struct? snooze-struct? void?)])
