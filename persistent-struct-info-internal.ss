#lang scheme/base

(require scheme/contract
         scheme/struct-info)

; Structure types --------------------------------

(define persistent-struct-info-field-names
  '(id
    struct-type-id
    entity-id
    constructor-id
    constructor/defaults-id
    copy-struct-id
    predicate-id
    default-alias-id
    attribute-ids
    accessor-ids
    mutator-ids
    attribute-names))

(define-values (struct:persistent-struct-info
                make-persistent-struct-info
                persistent-struct-info?
                persistent-struct-info-ref
                persistent-struct-info-set!)
  (let* ([entity-id-index
          (for/or ([index (in-naturals)]
                   [id    (in-list persistent-struct-info-field-names)])
                  (and (eq? id 'entity-id) index))]
         [attribute-ids-index
          (for/or ([index (in-naturals)]
                   [id    (in-list persistent-struct-info-field-names)])
                  (and (eq? id 'attribute-ids) index))]
         [attribute-names-index
          (for/or ([index (in-naturals)]
                   [id    (in-list persistent-struct-info-field-names)])
                  (and (eq? id 'attribute-names) index))]
         [proc
          (lambda (struct stx)
            (syntax-case stx ()
              [entity-id
               (identifier? #'entity-id)
               (persistent-struct-info-ref struct entity-id-index)]))])
    (make-struct-type 'persistent-struct-info
                      struct:struct-info
                      (length persistent-struct-info-field-names)
                      0
                      #f
                      null
                      #f
                      proc)))

(define-values (persistent-struct-info-id
                persistent-struct-info-struct-type-id
                persistent-struct-info-entity-id
                persistent-struct-info-constructor-id
                persistent-struct-info-constructor/defaults-id
                persistent-struct-info-copy-struct-id
                persistent-struct-info-predicate-id
                persistent-struct-info-default-alias-id
                persistent-struct-info-attribute-ids
                persistent-struct-info-accessor-ids
                persistent-struct-info-mutator-ids
                persistent-struct-info-attribute-names)
  (apply values (for/list ([index (in-naturals)]
                           [id    (in-list persistent-struct-info-field-names)])
                  (make-struct-field-accessor persistent-struct-info-ref index id))))

; Provide statements -----------------------------

(provide/contract
 [struct:persistent-struct-info                  struct-type?]
 [make-persistent-struct-info                    (-> procedure? 
                                                     identifier?
                                                     identifier?
                                                     identifier?
                                                     identifier?
                                                     identifier?
                                                     identifier?
                                                     identifier?
                                                     identifier?
                                                     (listof identifier?)
                                                     (listof identifier?)
                                                     (listof identifier?)
                                                     (listof symbol?)
                                                     persistent-struct-info?)]
 [persistent-struct-info?                        (-> any/c boolean?)]
 [persistent-struct-info-id                      (-> persistent-struct-info? identifier?)]
 [persistent-struct-info-struct-type-id          (-> persistent-struct-info? identifier?)]
 [persistent-struct-info-entity-id               (-> persistent-struct-info? identifier?)]
 [persistent-struct-info-constructor-id          (-> persistent-struct-info? identifier?)]
 [persistent-struct-info-constructor/defaults-id (-> persistent-struct-info? identifier?)]
 [persistent-struct-info-copy-struct-id          (-> persistent-struct-info? identifier?)]
 [persistent-struct-info-predicate-id            (-> persistent-struct-info? identifier?)]
 [persistent-struct-info-default-alias-id        (-> persistent-struct-info? identifier?)]
 [persistent-struct-info-attribute-ids           (-> persistent-struct-info? (listof identifier?))]
 [persistent-struct-info-accessor-ids            (-> persistent-struct-info? (listof identifier?))]
 [persistent-struct-info-mutator-ids             (-> persistent-struct-info? (listof identifier?))]
 [persistent-struct-info-attribute-names         (-> persistent-struct-info? (listof symbol?))])
