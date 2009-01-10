#lang mzscheme

(require scheme/class
         scheme/contract
         srfi/26/cut)

(require (file "../snooze-mzscheme.ss")
         (file "../snooze-interface.ss")
         (file "cache.ss"))

; audit-entities map entities to globally unique integers,
; reducing the size-on-disk of records in the audit-delta table.
;
; Entities are cached in memory for speed and in the database
; for persistence. The entity-cache% class makes sure the two
; caches stay in sync.

; Persistent struct types ------------------------

(define/provide-persistent-struct audit-entity
  ([name (make-symbol-type #f #f 32)]))

; Cache ------------------------------------------

(define entity-cache%
  (class* object% (entity-cache<%>)
    
    ; Constructor --------------------------------
    
    ; snooze<%>
    (init-field snooze)
    
    (super-new)
    
    ; Fields -------------------------------------
    
    ; (hash-table-of entity integer)
    (define forward-cache (make-hash-table))
    
    ; (hash-table-of integer entity)
    (define reverse-cache (make-hash-table))
    
    ; q:entity
    (define-alias ENTITY audit-entity)
    
    ; Public rocedures -----------------------------
    
    ; entity -> integer
    (define/public (entity->id entity)
      (cond [(memory-forward-lookup entity)
             => (lambda (id) 
                  id)]
            [(database-forward-lookup entity)
             => (lambda (id)
                  (memory-store! id entity)
                  id)]
            [else (let ([id (database-store! entity)])
                    (memory-store! id entity)
                    id)]))
    
    ; integer -> entity
    (define/public (id->entity id)
      (cond [(memory-reverse-lookup id)
             => (lambda (entity)
                  entity)]
            [(database-reverse-lookup id)
             => (lambda (entity)
                  (memory-store! id entity)
                  entity)]
            [else (raise-exn exn:fail:snooze
                    (format "Entity not found in audit metadata: ~s" id))]))
    
    ; -> void
    (define/public (clear-cache!)
      (set! forward-cache (make-hash-table))
      (set! reverse-cache (make-hash-table)))
    
    ; Helpers --------------------------------------
    
    ; entity -> (U integer #f)
    (define (memory-forward-lookup entity)
      (hash-table-get forward-cache entity #f))
    
    ; entity -> (U integer #f)
    (define (database-forward-lookup entity)
      (send snooze find-one 
            (q:select #:what  ENTITY-id
                      #:from  ENTITY
                      #:where (q:= ENTITY-name (entity-table-name entity)))))
    
    ; integer -> (U entity #f)
    (define (memory-reverse-lookup id)
      (hash-table-get reverse-cache id #f))
    
    ; integer -> entity
    (define (database-reverse-lookup id)
      (schema-entity 
       (send snooze find-one 
             (q:select #:what  ENTITY-name
                       #:from  ENTITY
                       #:where (q:= ENTITY-id id)))))
    
    ; integer entity -> void
    (define (memory-store! id entity)
      (hash-table-put! forward-cache entity id)
      (hash-table-put! reverse-cache id entity))
    
    ; entity -> void
    (define (database-store! entity)
      (define audit-entity
        (send snooze save! (make-audit-entity (entity-table-name entity))))
      (struct-id audit-entity))
    
    (inspect #f)))

; Provide statements ---------------------------

(provide-persistent-struct audit-entity (name))

(provide entity-cache<%>
         entity-cache%)
