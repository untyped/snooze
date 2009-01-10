#lang scheme/base
  
(require (for-syntax scheme/base
                     "../base.ss")
         "../base.ss")

; Syntax -----------------------------------------

(define-syntax (with-snooze-reraise stx)
  (syntax-case stx ()
    [(_ (exn-pred message) expr ...)
     #'(with-handlers
           ([exn-pred (lambda (exn)
                        (reraise-exn exn exn:fail:snooze message))])
         expr ...)]))

; Provide statements -----------------------------

(provide with-snooze-reraise)
