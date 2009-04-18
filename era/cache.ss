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
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; (-> any) -> any
    (define/public (call-with-cache-frame thunk)
      (parameterize ([current-frame (create-frame (current-frame))])
        (thunk)))
    
    ; guid [frame] [(U any (-> any))] -> snooze-struct
    (define/public (cache-ref guid [frame (current-frame)] [default (cut error "cache-ref: struct not cached" guid)])
      (cond [(hash-ref (frame-data frame) guid #f)
             => (lambda (local) local)]
            [(frame-parent frame)
             => (lambda (parent)
                  (let* ([remote (cache-ref guid parent)]
                         [local  (copy-snooze-struct remote)])
                    (cache-set! guid local frame)
                    local))]
            [else (if (procedure? default)
                      (default)
                      default)]))

    ; guid snooze-struct [frame] -> guid
    (define/public (cache-set! guid struct [frame (current-frame)])
      (hash-set! (frame-data frame) guid struct)
      guid)

    ; snooze-struct [frame] -> guid
    (define/public (cache-add! struct [frame (current-frame)])
      (if (cache-ref (struct-guid struct) frame #f)
          (error "cache-add!: struct already cached" struct)
          (cache-set! (struct-guid struct) struct frame)))
    
        ; -> #s(snooze ...)
    (define/public (get-serializable-cache-address)
      '#s(snooze no-database))))

; Provide statements -----------------------------

(provide (except-out (struct-out frame) make-frame)
         (rename-out [create-frame make-frame])
         snooze-cache-mixin)
