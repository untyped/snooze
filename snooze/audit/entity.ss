#lang scheme/base

(require scheme/class
         scheme/contract
         srfi/26/cut)

(require (file "../snooze.ss")
         (file "../snooze-interface.ss")
         (file "cache.ss"))

; audit-entities map entities to globally unique integers,
; reducing the size-on-disk of records in the audit-delta table.
;
; Entities are cached in memory for speed and in the database
; for persistence. The entity-cache% class makes sure the two
; caches stay in sync.

; Persistent struct types ------------------------

(define-persistent-struct audit-entity
  ([name (make-symbol-type #f #f 32)]))

; Cache ------------------------------------------

(define entity-cache%
  (class* object% (entity-cache<%>)
    
    ; Constructor --------------------------------
    
    ; snooze<%>
    (init-field snooze)
    
    (super-new)
    
    ; Fields -------------------------------------
    
    ; (hashof entity integer)
    (define forward-cache (make-hasheq))
    
    ; (hashof integer entity)
    (define reverse-cache (make-hasheq))
    
    ; sql:entity
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
      (set! forward-cache (make-hasheq))
      (set! reverse-cache (make-hasheq)))
    
    ; Helpers --------------------------------------
    
    ; entity -> (U integer #f)
    (define (memory-forward-lookup entity)
      (hash-ref forward-cache entity #f))
    
    ; entity -> (U integer #f)
    (define (database-forward-lookup entity)
      (send snooze find-one 
            (sql:select #:what  ENTITY-id
                        #:from  ENTITY
                        #:where (sql:= ENTITY-name (entity-table-name entity)))))
    
    ; integer -> (U entity #f)
    (define (memory-reverse-lookup id)
      (hash-ref reverse-cache id #f))
    
    ; integer -> entity
    (define (database-reverse-lookup id)
      (schema-entity 
       (send snooze find-one 
             (sql:select #:what  ENTITY-name
                         #:from  ENTITY
                         #:where (sql:= ENTITY-id id)))))
    
    ; integer entity -> void
    (define (memory-store! id entity)
      (hash-set! forward-cache entity id)
      (hash-set! reverse-cache id entity))
    
    ; entity -> void
    (define (database-store! entity)
      (define audit-entity
        (send snooze save! (make-audit-entity (entity-table-name entity))))
      (struct-id audit-entity))
    
    (inspect #f)))

; Provide statements ---------------------------

(provide (persistent-struct-out audit-entity)
         entity-cache<%>
         entity-cache%)
