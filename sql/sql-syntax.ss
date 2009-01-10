#lang scheme/base

(require (for-syntax scheme/base
                     "sql-syntax-internal.ss"))

; Syntax -----------------------------------------

(define-syntax (sql stx)
  (syntax-case stx ()
    [(_ expr) (expand-top-level (syntax expr))]))

; Provide statements -----------------------------

(provide sql)
