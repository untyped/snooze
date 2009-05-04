#lang scheme/base

(require "../base.ss")

(require "core.ss"
         "snooze-struct.ss")

; Cache frame ------------------------------------

; (struct (U snooze-cache #f) (hashof guid snooze-struct))
(define-struct frame (parent data) #:transparent)

; (U frame #f) -> frame
(define (create-frame parent)
  (make-frame parent (make-weak-hasheq)))

; object% -> snooze-cache<%>
(define snooze-cache-mixin
  (mixin () (snooze-cache<%>)
    
    ; Fields -------------------------------------
    
    ; (parameter frame)
    (field [current-frame (make-parameter (create-frame #f))])
    
    ; (hashof (cons symbol natural) guid)
    (field [guid-cache (make-hash)])
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; (-> any) -> any
    (define/public (call-with-cache-frame thunk)
      (parameterize ([current-frame (create-frame (current-frame))])
        (thunk)))
    
    ; guid [frame] [(U any (-> any))] -> snooze-struct
    (define/public (cache-ref guid [default (cut error "cache-ref: struct not cached" guid)] [frame (current-frame)])
      (cond [(hash-ref (frame-data frame) guid #f)
             => (lambda (local) local)]
            [(frame-parent frame)
             => (lambda (parent)
                  (let* ([remote (cache-ref guid default parent)]
                         [local  (copy-snooze-struct remote)])
                    (cache-set! guid local frame)
                    local))]
            [else (if (procedure? default) (default) default)]))

    ; guid snooze-struct [frame] -> guid
    (define/public (cache-set! guid struct [frame (current-frame)])
      (hash-set! (frame-data frame) guid struct)
      guid)

    ; snooze-struct [frame] -> guid
    (define/public (cache-add! struct [frame (current-frame)])
      (if (cache-ref (struct-guid struct) #f frame)
          (error "cache-add!: struct already cached" struct)
          (cache-set! (struct-guid struct) struct frame)))
    
    ; entity natural -> guid
    (define/public (get-interned-guid entity id)
      (unless (entity? entity) (raise-type-error 'get-interned-guid "entity" entity))
      (unless (number? id)     (raise-type-error 'get-interned-guid "number" id))
      (hash-ref guid-cache
                (cons (entity-name entity) id)
                (lambda () 
                  (let ([guid (entity-make-guid #:snooze this entity id)])
                    (hash-set! guid-cache (cons (entity-name entity) id) guid)
                    guid))))
    
    ; guid -> void
    (define/public (intern-guid! guid)
      (hash-set! guid-cache
                 (cons (entity-name (guid-entity guid)) (guid-id guid))
                 guid))
    
        ; -> #s(snooze ...)
    (define/public (get-serializable-cache-address)
      '#s(snooze no-database))))

; Provide statements -----------------------------

(provide (except-out (struct-out frame) make-frame)
         (rename-out [create-frame make-frame])
         snooze-cache-mixin)
