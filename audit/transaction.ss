#lang scheme/base
  
(require "../snooze.ss")

(define-snooze-struct audit-transaction
  ([timestamp (make-time-utc-type #f #f)])
  #:table-name 'audittransactions)

; Provide statements -----------------------------

(provide (snooze-struct-out audit-transaction))
