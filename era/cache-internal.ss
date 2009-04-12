#lang scheme/base

(require "../base.ss")

; Structure types --------------------------------

; (struct (U snooze-cache #f) (hashof guid snooze-struct))
(define-struct cache (parent data) #:transparent)

; Provide statements -----------------------------

(provide/contract
 [struct cache ([parent (or/c cache? #f)]
                [data   hash?])])
