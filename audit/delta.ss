#lang scheme/base

(require "../base.ss")

(require srfi/19
         (planet untyped/unlib:3/enumeration)
         (planet untyped/unlib:3/time)
         "../core/core.ss"
         "../sql/sql.ss"
         "attribute.ss"
         "transaction.ss")

(define-enum delta-types
  ([insert 'I]
   [update 'U]
   [delete 'D]))

(define-entity audit-delta
  ([transaction     audit-transaction #:allow-null? #f]
   [type            enum              #:allow-null? #f #:values delta-types]
   [guid-attribute  audit-attribute   #:allow-null? #f]
   [struct-id       integer           #:allow-null? #f]
   [struct-revision integer]
   [value-attribute audit-attribute   #:allow-null? #f]
   [boolean-value   boolean]
   [integer-value   integer]
   [real-value      real]
   [string-value    string]
   [time-utc-value  time-utc]
   [binary-value    binary]))

; Procedures -------------------------------------

; audit-transaction guid -> audit-delta
(define (make-insert-delta txn guid)
  (let ([attr (forward-attribute-lookup (entity-guid-attribute (guid-entity guid)))]
        [id   (guid-id guid)])
    (make-audit-delta txn                  ; transaction
                      (delta-types insert) ; type
                      attr                 ; guid-attribute
                      id                   ; struct-id
                      #f                   ; struct-revision
                      #f                   ; attribute-id
                      #f                   ; boolean-value
                      #f                   ; integer-value
                      #f                   ; real-value
                      #f                   ; string-value
                      #f                   ; time-utc-value
                      #f)))                ; binary-value

; audit-transaction guid integer attribute any -> audit-delta
(define (make-update-delta txn guid revision attr value)
  (make-update/delete-delta txn (delta-types update) guid revision attr value))

; audit-transaction guid integer attribute any -> audit-delta
(define (make-delete-delta txn guid revision attr value)
  (make-update/delete-delta txn (delta-types delete) guid revision attr value))

; audit-delta -> entity
(define/public (audit-delta-entity delta)
  (attribute-entity (attribute-reverse-lookup (audit-delta-guid-attribute delta))))

; audit-delta -> guid
(define (audit-delta-guid delta)
  (entity-make-database-guid (audit-delta-entity delta) (audit-delta-struct-id delta)))

; audit-delta -> (U attribute #f)
(define (audit-delta-attribute delta)
  (attribute-reverse-lookup (audit-delta-value-attribute delta)))

; audit-delta type -> any
(define (audit-delta-value delta type)
  (cond [(boolean-type?  type) (audit-delta-boolean-value delta)]
        [(integer-type?  type) (audit-delta-integer-value delta)]
        [(real-type?     type) (audit-delta-real-value delta)]
        [(string-type?   type) (audit-delta-string-value delta)]
        [(symbol-type?   type) (if (audit-delta-string-value delta)
                                   (string->symbol (audit-delta-string-value delta))
                                   #f)]
        [(time-tai-type? type) (if (audit-delta-time-utc-value delta)
                                   (time-utc->time-tai (audit-delta-time-utc-value delta))
                                   #f)]
        [(time-utc-type? type) (audit-delta-time-utc-value delta)]
        [(binary-type? type)   (audit-delta-binary-value delta)]))

; guid audit-delta (U snooze-struct #f) -> (U snooze-struct #f)
(define (revert-delta! guid delta struct)
  (unless (equal? guid (audit-delta-guid delta))
    (error "delta does not apply to the correct guid" (list guid delta)))
  (enum-case delta-types (audit-delta-type delta)
    [(insert) #f]
    [(update delete)
     (let ([struct (or struct (make-snooze-struct/defaults (guid-entity guid)))]
           [attr   (audit-delta-attribute delta)])
       (unless (struct-id struct)
         (set-struct-id! struct (audit-delta-struct-id delta)))
       (unless (struct-revision struct)
         (set-struct-revision! struct (audit-delta-struct-revision delta)))
       (set-struct-attribute! struct (attribute-name attr) (audit-delta-value delta (attribute-type attr)))
       struct)]))

; Helpers --------------------------------------

; audit-transaction (U 'U 'D) guid integer attribute any -> audit-delta
(define (make-update/delete-delta txn type guid revision attr value)
  (define entity-id (entity->id (guid-entity guid)))
  (define id        (guid-id guid))
  (define attr-id   (attribute->id attr))
  (define attr-type (attribute-type attr))
  (apply make-audit-delta
         (struct-id txn) ; transaction-id
         type            ; type
         entity-id       ; entity-id
         id              ; struct-id
         revision        ; struct-revision
         attr-id         ; attribute-id
         (expand-value attr-type value)))

;  ( type
;    (U boolean integer real string symbol time-tai time-utc)
; -> 
;    (list boolean (U integer #f) (U real #f) (U string #f) (U time-utc #f)) )
(define (expand-value type value)
  (if type
      (list (if (boolean-type? type) value #f)
            (if (integer-type? type) value #f)
            (if (real-type? type) value #f)
            (cond [(string-type? type) value]
                  [(symbol-type? type) (if value (symbol->string value) value)]
                  [else #f])
            (cond [(time-tai-type? type) (if value (time-tai->time-utc value) value)]
                  [(time-utc-type? type) value]
                  [else #f]))
      (list #f #f #f #f #f)))

; Provide statements -----------------------------

(provide (entity-out audit-delta)
         delta-types
         delta-api<%>
         delta-api%)
