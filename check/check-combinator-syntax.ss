#lang scheme/base

(require (for-syntax scheme/base)
         "../base.ss"
         "check-combinator.ss")

; Syntax ---------------------------------------

; (_ ([annotation any] ...) expr ...) -> (listof check-result)
(define-syntax check/annotate
  (syntax-rules ()
    [(_ ([ann val] ...) expr ...)
     (check-with-annotations
      (list (cons ann val) ...)
      (lambda ()
        (check-with-handlers
         (lambda ()
           expr ...))))]))

; Provide statements -----------------------------

(provide check/annotate)
