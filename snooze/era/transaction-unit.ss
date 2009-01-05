#lang scheme/unit

(require (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/parameter)
         (file "../base.ss")
         (file "era-sig.ss")
         (file "transaction-sig.ss"))

(import era^)

(export transaction^)

; Here's the idea:
;
; - Every time you start a transaction, you create a frame in which you record
;   deltas to persistent-structs.
;
; - There is a parameter called current-frame that points to the frame you are
;   currently using to store roll-back information.
;
; - When you start a sub-transaction, you create a "child" frame of the current
;   frame, and set that to be the current-frame.
;
; - Rolling back a given transaction involves rolling back its frame and any
;   child frames that haven't already been rolled back.
;
; - Even if a sub-transaction completes successfully, its parent still keeps a
;   link to its frame. If the parent is rolled back, it can roll back the child.
;
; That's about it.

; Structure types ------------------------------

; (struct frame (hashof symbol persistent-struct) boolean)
;
; The "deltas" field maps persistent structs to the old values 
; of their fields.
(define-struct frame (parent [copies #:mutable] [rolled-back? #:mutable]) #:transparent)

; (parameter (U frame #f))
(define current-frame (make-parameter #f (make-guard frame? "frame")))

; (parameter boolean)
(define enable-transaction-backups?
  (make-parameter #t (make-guard boolean? "boolean")))

; Public interface -----------------------------

; -> boolean
;
; This has been replaced by the in-transaction field in the connection structure.
;(define (in-transaction?)
;  (if (current-frame)
;      #t
;      #f))

; (-> any) -> any
(define (call-with-transaction-frame thunk)
  (define frame (create-frame))
  (define complete? #f)
  (call-with-continuation-barrier
   (lambda ()
     (dynamic-wind 
      ; Entry
      void
      ; Body
      (lambda ()
        (parameterize ([current-frame frame])
          (begin0 (thunk)
                  (set! complete? #t))))
      ; Exit
      (lambda ()
        (unless complete?
          (roll-back-transaction-frame! frame)))))))

; persistent-struct -> void
(define (store-transaction-backup! struct)
  (when (enable-transaction-backups?)
    (let ([frame (current-frame)])
      (when (and frame (not (copy-stored? frame struct)))
        (hash-set! (frame-copies frame) struct (copy-persistent-struct struct))))))

; Helpers --------------------------------------

; frame -> integer
(define (frame-depth frame)
  (if (frame-parent frame)
      (add1 (frame-depth (frame-parent frame)))
      0))

; -> frame
(define (create-frame)
  (make-frame (current-frame) (make-hasheq) #f))

; frame persistent-struct -> boolean
(define (copy-stored? frame struct)
  (and (hash-ref (frame-copies frame) struct (lambda () #f)) #t))

; frame -> void
(define (roll-back-transaction-frame! frame)
  (if (frame-rolled-back? frame)
      (raise-exn exn:fail:snooze
        (format "Transaction frame already rolled back: ~s" frame))
      (let ([copies (frame-copies frame)]) ; (hashof persistent-struct persistent-struct)
        ; Mark the frame as rolled back so we don't try it again:
        (set-frame-rolled-back?! frame #t)
        ; Roll back the frame:
        (hash-for-each (frame-copies frame) update-persistent-struct-from-copy!))))
