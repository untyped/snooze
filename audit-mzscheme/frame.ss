#lang mzscheme

(require mzlib/etc
         scheme/class
         scheme/contract
         scheme/match
         srfi/19/time
         (planet untyped/unlib:3/list)
         "../snooze-mzscheme.ss"
         "attribute.ss"
         "cache.ss"
         "delta.ss"
         "entity.ss")

; Parameters -----------------------------------

(define audit-frame%
  (class delta-api%
    
    (inherit id->entity
             entity->id
             id->attribute
             attribute->id
             make-insert-delta
             make-update-delta
             make-delete-delta)
    
    (inherit-field snooze)
    
    ; Fields -------------------------------------
    
    ; entity
    (init-field entity:audit-transaction)
    
    ; audit-transaction
    (field [transaction #f])
    
    ; boolean
    (field [changes-made? #f])
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Accessors ----------------------------------
    
    ; -> (U audit-transaction #f)
    (define/public (get-transaction)
      transaction)
    
    ; Caching changes ----------------------------
    
    ; any ... -> void
    (define/public (init! . log-values)
      (define attributes (entity-attributes entity:audit-transaction))
      (define make-transaction (entity-constructor entity:audit-transaction))
      (define args (list-pad-right (list* #f #f (current-time time-tai) log-values) (length attributes)))
      (set! transaction (send snooze save! (apply make-transaction args))))
    
    ; -> void
    ;
    ; Deletes the audit-transaction if no changes were made in this frame.
    (define/public (clean-up!)
      (unless changes-made?
        (send snooze delete! transaction)))

    ; persistent-struct -> void
    (define/public (audit-insert! struct)
      (define delta (make-insert-delta transaction (struct-guid struct)))
      (send snooze save! delta)
      (set! changes-made? #t))
    
    ; persistent-struct -> void
    (define/public (audit-update! new)
      ; integer
      (define id (struct-id new))
      ; entity
      (define entity (struct-entity new))
      ; persistent-struct
      (define old (send snooze find-by-id entity id))
      ; integer
      (define revision (struct-revision old))
      ; void
      (for-each (lambda (attr old-value new-value)
                  (unless (equal? old-value new-value)
                    (let* (; integer
                           [attr-id (attribute->id attr)]
                           ; (U type #f)
                           [attr-type (attribute-type attr)]
                           ; transaction-id gets patched in later 
                           [delta (make-update-delta transaction (struct-guid new) revision attr old-value)])
                      (send snooze save! delta)
                      (set! changes-made? #t))))
                (cddr (entity-attributes entity))
                (cddr (struct-attributes old))
                (cddr (struct-attributes new))))
    
    ; persistent-struct -> void
    (define/public (audit-delete! struct)
      ; integer
      (define id (struct-id struct))
      ; integer
      (define revision (struct-revision struct))
      ; entity
      (define entity (struct-entity struct))
      ; void
      (for-each (lambda (attr value)
                  ; integer
                  (define attr-id (attribute->id attr))
                  ; (U type #f)
                  (define attr-type (attribute-type attr))
                  ; audit-delta ; transaction-id gets patched in later 
                  (define delta (make-delete-delta transaction (struct-guid struct) revision attr value))
                  (send snooze save! delta)
                  (set! changes-made? #t))
                (cddr (entity-attributes entity))
                (cddr (struct-attributes struct))))
    
    (inspect #f)))

; Provide statements -----------------------------

(provide audit-frame%)