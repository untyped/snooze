#lang scheme/base

(require (for-syntax scheme/base
                     "sql-syntax-internal.ss"))

; Syntax -----------------------------------------

(define-syntax (sql stx)
  (syntax-case stx ()
    [(_ expr) (expand-top-level (syntax expr))]))

(define-syntax (sql-list stx)
  (syntax-case stx ()
    [(_ expr ...) (syntax/loc stx (list (sql expr) ...))]))

; Provide statements -----------------------------

(provide sql
         sql-list)
