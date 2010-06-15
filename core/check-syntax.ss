#lang scheme/base

(require (for-syntax scheme/base)
         "../base.ss")

(require "check.ss")

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
           (check-all expr ...)))))]))

; Provide statements -----------------------------

(provide check/annotate)
