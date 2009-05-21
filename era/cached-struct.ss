#lang scheme/base

(require "../base.ss")

(require (unlib-in list parameter)
         "attribute-keyword.ss"
         "core.ss"
         (prefix-in real: "snooze-struct.ss"))

; Standard wrappers ------------------------------

; any -> boolean
(define snooze-struct? guid?)

; (U snooze-struct guid) -> entity
(define (snooze-struct-entity struct+guid)
  (guid-entity struct+guid))

; guid -> (U natural #f)
(define (snooze-struct-id guid)
  (real:snooze-struct-id (guid-ref guid)))

; guid guid -> boolean
(define (snooze-struct-eq? guid1 guid2)
  (eq? (guid-ref guid1)
       (guid-ref guid2)))

; guid -> boolean
(define (snooze-struct-saved? guid)
  (and (real:snooze-struct-guid (guid-ref guid)) #t))

; guid -> (U natural #f)
(define (snooze-struct-revision guid)
  (real:snooze-struct-revision (guid-ref guid)))

; guid (U symbol attribute) -> any
(define (snooze-struct-ref guid name+attr)
  (let ([ans (if (or (eq? name+attr 'guid)
                     (and (attribute? name+attr)
                          (eq? (attribute-name name+attr) 'guid)))
                 guid
                 (real:snooze-struct-ref (guid-ref guid) name+attr))])
    (if (guid? ans)
        (send (guid-snooze ans) find-by-guid ans)
        ans)))

; guid -> any
(define (snooze-struct-ref* guid)
  (cons guid
        (for/list ([ans (in-list (cdr (real:snooze-struct-ref* (guid-ref guid))))])
          (if (guid? ans)
              (send (guid-snooze ans) find-by-guid ans)
              ans))))

; guid <attr any> ... -> guid
(define (snooze-struct-set original . args)
  (let*-values ([(cache)              (send (guid-snooze original) get-current-cache)]
                [(entity)             (snooze-struct-entity original)]
                [(arg-attrs arg-vals) (check-attribute-keywords entity args)]
                [(attrs)              (entity-attributes entity)]
                [(existing)           (real:snooze-struct-ref* (guid-ref original))])
    (send cache add-copied-struct!
          (apply (entity-private-constructor entity)
                 (for/list ([attr     (in-list attrs)]
                            [existing (in-list existing)])
                   (attribute-keyword-get
                    entity
                    attr
                    arg-attrs
                    arg-vals
                    (lambda (attr) existing)))))))

; entity any ... -> guid
(define (make-snooze-struct #:snooze [snooze (current-snooze)] entity . args)
  (let ([cache (send (current-snooze) get-current-cache)])
    (send cache add-copied-struct! (apply real:make-snooze-struct entity args))))

; entity <attr any> ... -> guid
(define (make-snooze-struct/defaults #:snooze [snooze (current-snooze)] entity . args)
  (let ([cache (send (current-snooze) get-current-cache)])
    (send cache add-copied-struct! (apply real:make-snooze-struct/defaults entity args))))

; guid -> guid
(define (copy-snooze-struct original)
  (let ([cache  (send (guid-snooze original) get-current-cache)]
        [entity (snooze-struct-entity original)])
    (send cache add-copied-struct! (apply (entity-private-constructor entity)
                                          (real:snooze-struct-ref* (guid-ref original))))))

; guid any ... -> string
(define (format-snooze-struct guid . rest)
  (apply (entity-pretty-formatter (snooze-struct-entity guid)) guid rest))

; Provide statements -----------------------------

(provide/contract
 [snooze-struct?                  (-> any/c boolean?)]
 [snooze-struct-eq?               (-> guid? guid? boolean?)]
 [snooze-struct-entity            (-> guid? entity?)]
 [snooze-struct-id                (-> guid? (or/c natural-number/c #f))]
 [snooze-struct-saved?            (-> guid? boolean?)]
 [snooze-struct-revision          (-> guid? (or/c natural-number/c #f))]
 [snooze-struct-ref               (-> guid? (or/c attribute? symbol?) any)]
 [snooze-struct-ref*              (-> guid? list?)]
 [snooze-struct-set               (->* (guid?) () #:rest attr/value-list? guid?)]
 [make-snooze-struct              (->* (entity?) () #:rest any/c guid?)]
 [make-snooze-struct/defaults     (->* (entity?) () #:rest attr/value-list? guid?)]
 [copy-snooze-struct              (-> guid? guid?)]
 [format-snooze-struct            (->* (guid?) () #:rest any/c string?)])
