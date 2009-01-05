#lang scheme/base
  
(require (for-syntax scheme/base
                     (planet untyped/unlib:3/syntax))
         (file "../snooze.ss"))

; (alistof symbol type) -> entity
(define-syntax (define-audit-transaction stx)
  (syntax-case stx ()
    [(_ id ([attr-id attr-type] ...))
     (with-syntax ([audit-transaction (make-id stx 'audit-transaction)])
       #'(begin (define-persistent-struct id
                  ([timestamp (make-time-utc-type #f #f)]
                   [attr-id attr-type] ...))))]))

; Provide statements -----------------------------

(provide define-audit-transaction)
