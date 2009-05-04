#lang scheme/base

(require scheme/class
         scheme/contract
         srfi/26/cut)

(require "../snooze.ss"
         "cache.ss"
         "entity.ss")

; audit-attributes map attributes to globally unique integers,
; reducing the size-on-disk of records in the audit-delta table.
;
; Attributes are cached in memory for speed and in the database
; for persistence. The attribute-cache% class makes sure the caches
; stay in sync.

; Persistent struct types ------------------------

(define-snooze-struct audit-attribute
  ([entity-id (make-integer-type #f #f) #:column-name 'entityID]
   [name      (make-symbol-type #f #f 32)])
  #:table-name 'auditattributes)

; Cache ------------------------------------------

(define attribute-cache%
  (class* object% (attribute-cache<%>)
    
    ; Constructor --------------------------------
    
    ; snooze<%>
    (init-field snooze)
    
    ; entity-cache<%>
    (init-field entity-cache)
    
    (super-new)
    
    ; Fields -------------------------------------
    
    ; (hashof (cons symbol symbol) integer)
    (define forward-cache (make-hash))
    
    ; (hashof integer attribute)
    (define reverse-cache (make-hasheq))
    
    (define-alias ENTITY audit-entity)
    (define-alias ATTR   audit-attribute)
    
    ; Public methods -----------------------------
    
    ; attribute -> integer
    (define/public (attribute->id attr)
      (cond [(memory-forward-lookup attr)
             => (lambda (id)
                  id)]
            [(database-forward-lookup snooze attr)
             => (lambda (id)
                  (memory-store! id attr)
                  id)]
            [else (let ([id (database-store! attr)])
                    (memory-store! id attr)
                    id)]))
    
    ; integer -> attribute
    (define/public (id->attribute id)
      (cond [(memory-reverse-lookup id)
             => (lambda (attr)
                  attr)]
            [(database-reverse-lookup id)
             => (lambda (attr)
                  (memory-store! id attr)
                  attr)]
            [else (raise-exn exn:fail:snooze
                    "Attribute not found in audit metadata: ~s" id)]))
    
    ; -> void
    (define/public (clear-cache!)
      (set! forward-cache (make-hash))
      (set! reverse-cache (make-hash)))
    
    ; Helpers ------------------------------------
    
    ; attribute -> (cons symbol symbol)
    (define (attr->forward-key attr)
      (cons (entity-table-name (attribute-entity attr))
            (attribute-column-name attr)))
    
    ; attribute -> (U integer #f)
    (define (memory-forward-lookup attr)
      (hash-ref forward-cache (attr->forward-key attr) #f))
    
    ; attribute -> (U integer #f)
    (define (database-forward-lookup snooze attr)
      (send snooze find-one 
            (sql:select #:what  ATTR-id
                        #:from  (sql:inner ENTITY ATTR (sql:= ENTITY-id ATTR-entity-id))
                        #:where (sql:and (sql:= ENTITY-name (entity-table-name (attribute-entity attr)))
                                         (sql:= ATTR-name (attribute-column-name attr))))))
    
    ; integer -> (U attribute #f)
    (define (memory-reverse-lookup id)
      (hash-ref reverse-cache id #f))
    
    ; integer -> attribute
    (define (database-reverse-lookup id)
      ; (list symbol symbol)
      (define names
        (send snooze find-one 
              (sql:select #:what  (list ENTITY-name ATTR-name)
                          #:from  (sql:inner ENTITY ATTR (sql:= ENTITY-id ATTR-entity-id))
                          #:where (sql:= ATTR-id id))))
      (entity-attribute (schema-entity (car names)) (cadr names)))
    
    ; integer attribute -> void
    (define (memory-store! id attr)
      (hash-set! forward-cache attr id)
      (hash-set! reverse-cache id attr))
    
    ; attribute -> void
    (define (database-store! attr)
      (define entity-id
        (send entity-cache entity->id (attribute-entity attr)))
      (define audit-attr
        (send snooze save! (make-audit-attribute entity-id (attribute-column-name attr))))
      (struct-id audit-attr))
    
    (inspect #f)))

; Provide statements -----------------------------

(provide (snooze-struct-out audit-attribute)
         attribute-cache<%>
         attribute-cache%)
