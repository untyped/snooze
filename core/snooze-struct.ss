#lang scheme/base

(require "../base.ss")

(require (unlib-in list)
         "attribute-keyword.ss"
         "struct.ss")

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

; snooze-struct -> boolean
(define (snooze-struct-has-revision? struct)
  (and (snooze-struct-revision struct) #t))

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
        (cond [(database-guid?  val) (send (current-snooze) find-by-guid val)]
              [(temporary-guid? val) (error "temporary guid found in foreign key" val)]
              [else                  val]))))

; snooze-struct boolean -> any
(define (snooze-struct-ref* struct)
  (let ([data (struct->vector struct)])
    (list* (vector-ref data 1)
           (vector-ref data 2)
           (for/list ([val (in-vector data 3)])
             (cond [(database-guid?  val) (send (current-snooze) find-by-guid val)]
                   [(temporary-guid? val) (error "temporary guid found in foreign key" val)]
                   [else                  val])))))

; guid -> list
(define (snooze-struct-data-ref* struct)
  (for/list ([val (in-vector (struct->vector struct) 3)])
    (cond [(database-guid?  val) (send (current-snooze) find-by-guid val)]
          [(temporary-guid? val) (error "temporary guid found in foreign key" val)]
          [else                  val])))

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

; (U snooze-struct guid) (U snooze-struct guid) -> boolean
(define (snooze-struct-guid-equal? val1 val2 [equal? equal?])
  (equal? (if (snooze-struct? val1)
              (snooze-struct-guid val1)
              val1)
          (if (snooze-struct? val2)
              (snooze-struct-guid val2)
              val2)))

; snooze-struct snooze-struct -> boolean
(define (snooze-struct-data-equal? struct1 struct2 [equal? equal?])
  (and (eq? (snooze-struct-entity struct1)
            (snooze-struct-entity struct2))
       (for/and ([val1 (in-list (snooze-struct-data-ref* struct1))]
                 [val2 (in-list (snooze-struct-data-ref* struct2))])
         (equal? (if (snooze-struct? val1)
                     (snooze-struct-guid val1)
                     val1)
                 (if (snooze-struct? val2)
                     (snooze-struct-guid val2)
                     val2)))))

; snooze-struct snooze-struct -> boolean
(define (snooze-struct-equal? struct1 struct2 [equal? equal?])
  (and (snooze-struct-guid-equal? struct1 struct2 equal?)
       (snooze-struct-data-equal? struct1 struct2 equal?)))

; guid any ... -> string
(define (format-snooze-struct struct . rest)
  (let* ([entity (snooze-struct-entity struct)]
         [ans    (apply (entity-pretty-formatter entity) struct rest)])
    (if (string? ans)
        ans
        (raise-exn exn:fail:contract
          (format "format-snooze-struct: pretty-formatter for ~a returned ~a, expected a string"
                  (entity-name entity)
                  ans)))))

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

(define snooze-struct-equal+hash
  (list (lambda (a b equal?)
          (and (equal? (snooze-struct-revision a)
                       (snooze-struct-revision b))
               (snooze-struct-equal? a b equal?)))
        (lambda (a hash-code)
          (let ([vec (struct->vector a)])
            (for ([i (in-range 3 (vector-length vec))])
              (when (snooze-struct? (vector-ref vec i))
                (vector-set! vec i (snooze-struct-guid (vector-ref vec i)))))
            (hash-code vec)))
        (lambda (a hash-code)
          (let ([vec (struct->vector a)])
            (for ([i (in-range 3 (vector-length vec))])
              (when (snooze-struct? (vector-ref vec i))
                (vector-set! vec i (snooze-struct-guid (vector-ref vec i)))))
            (hash-code vec)))))

; Provide statements -----------------------------

(define revision/c
  (or/c natural-number/c #f))

(define attr-value-list/c
  (cons/c guid? (cons/c revision/c any/c)))

(provide/contract
 [snooze-struct-guid            (-> snooze-struct? guid?)]
 [snooze-struct-id              (-> snooze-struct? (or/c natural-number/c symbol?))]
 [snooze-struct-saved?          (-> snooze-struct? boolean?)]
 [snooze-struct-has-revision?   (-> snooze-struct? boolean?)]
 [snooze-struct-revision        (-> snooze-struct? revision/c)]
 [snooze-struct-ref             (-> snooze-struct? (or/c attribute? symbol?) any)]
 [snooze-struct-ref*            (-> snooze-struct? list?)]
 [snooze-struct-data-ref*       (-> snooze-struct? list?)]
 [snooze-struct-set             (->* (snooze-struct?) () #:rest attr/value-list? snooze-struct?)]
 [make-snooze-struct            (->* (entity?) () #:rest attr-value-list/c snooze-struct?)]
 [make-snooze-struct/defaults   (->* (entity?) () #:rest attr/value-list? snooze-struct?)]
 [snooze-struct-copy            (-> snooze-struct? snooze-struct?)]
 [snooze-struct-guid-equal?     (-> (or/c snooze-struct? guid?) (or/c snooze-struct? guid?) boolean?)]
 [snooze-struct-data-equal?     (-> snooze-struct? snooze-struct? boolean?)]
 [snooze-struct-equal?          (-> snooze-struct? snooze-struct? boolean?)]
 [format-snooze-struct          (->* (snooze-struct?) () #:rest any/c string?)]
 [check-snooze-struct           (->* (snooze-struct?) () #:rest any/c (listof check-result?))]
 [check-old-snooze-struct       (->* (snooze-struct?) () #:rest any/c (listof check-result?))]
 [snooze-struct-equal+hash      (list/c procedure? procedure? procedure?)])
