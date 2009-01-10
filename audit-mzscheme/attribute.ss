#lang mzscheme

(require scheme/class
         scheme/contract
         srfi/26/cut)

(require (file "../snooze-mzscheme.ss")
         (file "cache.ss")
         (file "entity.ss"))

; audit-attributes map attributes to globally unique integers,
; reducing the size-on-disk of records in the audit-delta table.
;
; Attributes are cached in memory for speed and in the database
; for persistence. The attribute-cache% class makes sure the caches
; stay in sync.

; Persistent struct types ------------------------

(define/provide-persistent-struct audit-attribute
  ([entity-id (make-integer-type #f #f)]
   [name      (make-symbol-type #f #f 32)]))

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
    
    ; (hash-table-of (cons symbol symbol) integer)
    (define forward-cache (make-hash-table 'equal))
    
    ; (hash-table-of integer attribute)
    (define reverse-cache (make-hash-table))
    
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
      (set! forward-cache (make-hash-table 'equal))
      (set! reverse-cache (make-hash-table 'equal)))
    
    ; Helpers ------------------------------------
    
    ; attribute -> (cons symbol symbol)
    (define (attr->forward-key attr)
      (cons (entity-table-name (attribute-entity attr))
            (attribute-column-name attr)))
    
    ; attribute -> (U integer #f)
    (define (memory-forward-lookup attr)
      (hash-table-get forward-cache (attr->forward-key attr) #f))
    
    ; attribute -> (U integer #f)
    (define (database-forward-lookup snooze attr)
      (send snooze find-one 
            (q:select #:what  ATTR-id
                      #:from  (q:inner ENTITY ATTR (q:= ENTITY-id ATTR-entity-id))
                      #:where (q:and (q:= ENTITY-name (entity-table-name (attribute-entity attr)))
                                     (q:= ATTR-name (attribute-column-name attr))))))
    
    ; integer -> (U attribute #f)
    (define (memory-reverse-lookup id)
      (hash-table-get reverse-cache id #f))
    
    ; integer -> attribute
    (define (database-reverse-lookup id)
      ; (list symbol symbol)
      (define names
        (send snooze find-one 
              (q:select #:what  (list ENTITY-name ATTR-name)
                        #:from  (q:inner ENTITY ATTR (q:= ENTITY-id ATTR-entity-id))
                        #:where (q:= ATTR-id id))))
      (entity-attribute (schema-entity (car names)) (cadr names)))
    
    ; integer attribute -> void
    (define (memory-store! id attr)
      (hash-table-put! forward-cache attr id)
      (hash-table-put! reverse-cache id attr))
    
    ; attribute -> void
    (define (database-store! attr)
      (define entity-id
        (send entity-cache entity->id (attribute-entity attr)))
      (define audit-attr
        (send snooze save! (make-audit-attribute entity-id (attribute-column-name attr))))
      (struct-id audit-attr))
    
    (inspect #f)))

; Provide statements -----------------------------

(provide-persistent-struct audit-attribute (entity-id name))

(provide attribute-cache<%>
         attribute-cache%)
