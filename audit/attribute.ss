#lang scheme/base

(require "../base.ss")

(require "../snooze-api.ss"
         "../core/core.ss"
         "../sql/sql.ss")

; audit-attributes map attributes to globally unique integers,
; reducing the size-on-disk of records in the audit-delta table.
;
; Attributes are cached in memory for speed and in the database
; for persistence. The attribute-cache% class makes sure the caches
; stay in sync.

; Persistent struct types ------------------------

(define-entity audit-attribute
  ([table  symbol #:max-length 128]
   [column symbol #:max-length 128]))

; Cache ------------------------------------------

; (hashof attribute audit-attribute)
(define forward-cache (make-hash))

; (hashof audit-attribute attribute)
(define reverse-cache (make-hash))

; Procedures -------------------------------------

; attribute [#:snooze snooze] -> audit-attribute
(define (forward-attribute-lookup attr #:snooze [snooze (current-snooze)])
  (cond [(memory-forward-lookup snooze attr)
         => (lambda (aattr) aattr)]
        [(database-forward-lookup snooze attr)
         => (lambda (aattr)
              (memory-store! snooze aattr attr)
              aattr)]
        [else (let ([aattr (database-store! snooze attr)])
                (memory-store! snooze aattr attr)
                aattr)]))

; audit-attribute [#:snooze snooze] -> attribute
(define (reverse-attribute-lookup aattr #:snooze [snooze (current-snooze)])
  (or (memory-reverse-lookup snooze aattr)
      (error "attribute not found in audit metadata" aattr)))

; [#:snooze snooze] -> void
(define (clear-attribute-cache! #:snooze [snooze (current-snooze)])
  (set! forward-cache (make-hash))
  (set! reverse-cache (make-hash)))

; Helpers ----------------------------------------

; snooze attribute -> (U audit-attribute #f)
(define (memory-forward-lookup snooze attr)
  (hash-ref forward-cache attr #f))

; snooze attribute -> (U integer #f)
(define (database-forward-lookup snooze attr)
  (find-audit-attribute
   #:snooze snooze
   #:table  (entity-table-name (attribute-entity attr))
   #:column (attribute-column-name attr)))

; snooze audit-attribute -> (U attribute #f)
(define (memory-reverse-lookup snooze aattr)
  (hash-ref reverse-cache aattr #f))

; snooze audit-attribute attribute -> void
(define (memory-store! snooze aattr attr)
  (hash-set! forward-cache attr aattr)
  (hash-set! reverse-cache aattr attr))

; snooze attribute -> void
(define (database-store! snooze attr)
  (save! #:snooze snooze
         (make-audit-attribute/defaults
          #:table  (entity-table-name (attribute-entity attr))
          #:column (attribute-column-name attr))))

; Provide statements -----------------------------

(provide (entity-out audit-attribute))

(provide/contract
 [forward-attribute-lookup (->* (attribute?) (#:snooze (is-a?/c snooze<%>)) audit-attribute?)]
 [reverse-attribute-lookup (->* (audit-attribute?) (#:snooze (is-a?/c snooze<%>)) attribute?)]
 [clear-attribute-cache!   (->* () (#:snooze (is-a?/c snooze<%>)) void?)])
