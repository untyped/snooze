#lang scheme/base

(require "../base.ss")

(require (unlib-in list)
         "attribute-keyword.ss"
         "struct.ss")

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
  (database-guid? (snooze-struct-guid struct)))

; snooze-struct -> (U natural #f)
(define (snooze-struct-revision struct)
  ((entity-private-accessor (snooze-struct-entity struct)) struct 1))

; snooze-struct (U symbol attribute) -> any
(define (snooze-struct-ref struct name+attr)
  (if (or (eq? name+attr 'guid)
          (eq? name+attr (car (entity-attributes (snooze-struct-entity struct)))))
      (snooze-struct-guid struct)
      (let* ([entity (snooze-struct-entity struct)]
             [attr   (if (attribute? name+attr)
                         name+attr
                         (entity-attribute entity name+attr))]
             [val    ((attribute-private-accessor attr) struct)])
        (if (database-guid? val)
            (send (current-snooze) find-by-guid val)
            val))))

; snooze-struct boolean -> any
(define (snooze-struct-ref* struct)
  (let ([data (struct->vector struct)])
    (list* (vector-ref data 1)
           (vector-ref data 2)
           (for/list ([val (in-vector data 3)])
             (if (database-guid? val)
                 (send (current-snooze) find-by-guid val)
                 val)))))

; guid -> list
(define (snooze-struct-data-ref* struct)
  (for/list ([val (in-vector (struct->vector struct) 3)])
    (if (database-guid? val)
        (send (current-snooze) find-by-guid val)
        val)))

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
(define (make-snooze-struct/defaults entity . args)
  (let-values ([(arg-attrs arg-vals) (check-attribute-keywords entity args)])
    (apply (entity-private-constructor entity)
           (for/list ([attr (in-list (entity-attributes entity))])
             (attribute-keyword-get
              entity
              attr
              arg-attrs
              arg-vals
              (lambda (attr)
                (attribute-default attr)))))))

; snooze-struct -> snooze-struct
(define (snooze-struct-copy original)
  (let ([entity (snooze-struct-entity original)])
    (apply (entity-private-constructor entity)
           (entity-make-temporary-guid entity) 
           #f
           (snooze-struct-data-ref* original))))

; snooze-struct any ... -> (listof check-result)
(define (check-snooze-struct struct . rest)
  (let* ([entity (snooze-struct-entity struct)]
         [ans    (apply (entity-save-check entity) struct rest)])
    (if (and (list? ans) (andmap check-result? ans))
        ans
        (raise-exn exn:fail:contract
          (format "check-snooze-struct: validation for ~a returned ~a, expected (listof check-result)"
                  (entity-name entity)
                  ans)))))

; snooze-struct any ... -> (listof check-result)
(define (check-old-snooze-struct struct . rest)
  (let* ([entity (snooze-struct-entity struct)]
         [ans    (apply (entity-delete-check entity) struct rest)])
    (if (and (list? ans) (andmap check-result? ans))
        ans
        (raise-exn exn:fail:contract
          (format "check-snooze-struct: validation for ~a returned ~a, expected (listof check-result)"
                  (entity-name struct)
                  ans)))))

; Provide statements -----------------------------

(define revision/c
  (or/c natural-number/c #f))

(define attr-value-list/c
  (cons/c guid? (cons/c revision/c any/c)))

(provide/contract
 [snooze-struct?              (-> any/c boolean?)]
 [snooze-struct-entity        (-> (or/c snooze-struct? prop:entity-set?) entity?)]
 [snooze-struct-guid          (-> (or/c snooze-struct? prop:entity-set?) guid?)]
 [snooze-struct-id            (-> snooze-struct? (or/c natural-number/c symbol?))]
 [snooze-struct-saved?        (-> snooze-struct? boolean?)]
 [snooze-struct-revision      (-> snooze-struct? revision/c)]
 [snooze-struct-ref           (-> snooze-struct? (or/c attribute? symbol?) any)]
 [snooze-struct-ref*          (-> snooze-struct? list?)]
 [snooze-struct-data-ref*     (-> snooze-struct? list?)]
 [snooze-struct-set           (->* (snooze-struct?) () #:rest attr/value-list? snooze-struct?)]
 [make-snooze-struct          (->* (entity?) () #:rest attr-value-list/c snooze-struct?)]
 [make-snooze-struct/defaults (->* (entity?) () #:rest attr/value-list? snooze-struct?)]
 [snooze-struct-copy          (-> snooze-struct? snooze-struct?)]
 [check-snooze-struct         (->* (snooze-struct?) () #:rest any/c (listof check-result?))]
 [check-old-snooze-struct     (->* (snooze-struct?) () #:rest any/c (listof check-result?))])
