#lang scheme/base

(require "../base.ss")

(require "../snooze-api.ss"
         "../core/core.ss"
         "../sql/sql.ss")

; The audit-attribute entity defined below maps attributes to globally unique integers,
; reducing the size-on-disk of records in the audit-delta table.
;
; Attributes are cached in memory for speed, and in the database for persistence.
; This module manages both caches.

; Persistent struct types ------------------------

(define-entity audit-attribute
  ([table  symbol #:max-length 128]
   [column symbol #:max-length 128]))

; Cache ------------------------------------------

; (hashof attribute natural)
(define forward-cache (make-hash))

; (hashof natural attribute)
(define reverse-cache (make-hash))

; Procedures -------------------------------------

; attribute [#:snooze snooze] -> natural
(define (attribute->id attr #:snooze [snooze (current-snooze)])
  (cond [(memory-forward-lookup snooze attr)   => (lambda (id) id)]
        [(database-forward-lookup snooze attr) => (lambda (id) (memory-store! snooze id attr))]
        [else (memory-store! snooze (database-store! snooze attr) attr)]))

; natural [#:snooze snooze] -> attribute
(define (id->attribute id #:snooze [snooze (current-snooze)])
  (or (memory-reverse-lookup snooze id)
      (error "reverse-attribute-lookup: id not found in audit metadata" id)))

; [#:snooze snooze] -> void
(define (clear-attribute-cache! #:snooze [snooze (current-snooze)])
  (set! forward-cache (make-hash))
  (set! reverse-cache (make-hash)))

; Helpers ----------------------------------------

; snooze attribute -> (U audit-attribute #f)
(define (memory-forward-lookup snooze attr)
  (hash-ref forward-cache attr #f))

; snooze attribute -> (U natural #f)
(define (database-forward-lookup snooze attr)
  (snooze-struct-id
   (find-audit-attribute
    #:snooze snooze
    #:table  (entity-table-name (attribute-entity attr))
    #:column (attribute-column-name attr))))

; snooze audit-attribute -> (U attribute #f)
(define (memory-reverse-lookup snooze aattr)
  (hash-ref reverse-cache aattr #f))

; snooze natural attribute -> natural
(define (memory-store! snooze id attr)
  (hash-set! forward-cache attr id)
  (hash-set! reverse-cache id attr)
  id)

; snooze attribute -> natural
(define (database-store! snooze attr)
  (snooze-struct-id (save! #:snooze snooze
                           (make-audit-attribute/defaults
                            #:table  (entity-table-name (attribute-entity attr))
                            #:column (attribute-column-name attr)))))

; Provide statements -----------------------------

(provide (entity-out audit-attribute))

(provide/contract
 [attribute->id (->* (attribute?) (#:snooze (is-a?/c snooze<%>)) natural-number/c)]
 [id->attribute (->* (natural-number/c) (#:snooze (is-a?/c snooze<%>)) attribute?)]
 [clear-attribute-cache! (->* () (#:snooze (is-a?/c snooze<%>)) void?)])
