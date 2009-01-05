#lang scheme/base
  
(require (for-syntax scheme/base
                     (file "../base.ss"))
         (file "../base.ss"))

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
