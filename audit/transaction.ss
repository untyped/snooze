#lang scheme/base
  
(require "../base.ss")

(require "../core/core.ss")

; Entities ---------------------------------------

(define-entity audit-transaction
  ([timestamp time-utc #:allow-null? #f]))

; Provide statements -----------------------------

(provide/contract/entities
 [entity audit-transaction])
