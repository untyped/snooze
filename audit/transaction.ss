#lang scheme/base
  
(require "../snooze.ss")

(define-persistent-struct audit-transaction
  ([timestamp (make-time-utc-type #f #f)])
  #:table-name 'audittransactions)

; Provide statements -----------------------------

(provide (persistent-struct-out audit-transaction))
