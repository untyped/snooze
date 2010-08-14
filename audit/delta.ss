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
   [struct-id       integer           #:allow-null? #f]
   [attribute-id    integer           #:allow-null? #f]
   [boolean-value   boolean]
   [integer-value   integer]
   [real-value      real]
   [string-value    string]
   [time-utc-value  time-utc]
   [binary-value    binary]))

; Procedures -------------------------------------

; audit-transaction snooze-struct -> audit-delta
(define (make-insert-delta txn struct)
  (let ([attr-id (attribute->id (entity-guid-attribute (snooze-struct-entity struct)))])
    (make-audit-delta/defaults
     #:transaction  txn
     #:type         (delta-types insert)
     #:struct-id    (snooze-struct-id struct)
     #:attribute-id attr-id)))

; audit-transaction snooze-struct attribute -> audit-delta
(define (make-update-delta txn struct attr)
  (let* ([id        (snooze-struct-id struct)]
         [attr-id   (attribute->id attr)]
         [attr-type (attribute-type attr)]
         [attr-val  (snooze-struct-ref struct attr)]
         [rest      (expand-value attr-type attr-val)])
    (apply make-audit-delta
           txn                  ; transaction-id
           (delta-types update) ; type
           id                   ; struct-id
           attr-id              ; attribute-id
           rest)))              ; etc...

; audit-transaction snooze-struct -> audit-delta
(define (make-delete-delta txn struct)
  (let ([attr-id (attribute->id (entity-guid-attribute (snooze-struct-entity struct)))])
    (make-audit-delta/defaults
     #:transaction  txn
     #:type         (delta-types delete)
     #:struct-id    (snooze-struct-id struct)
     #:attribute-id attr-id)))

; audit-delta -> entity
(define (audit-delta-entity delta)
  (attribute-entity (audit-delta-attribute delta)))

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

;  type
;  (U boolean integer real string symbol time-tai time-utc)
; ->
;  (list boolean (U integer #f) (U real #f) (U string #f) (U time-utc #f))
(define (expand-value type value)
  (list (and (boolean-type? type) value)
        (and (integer-type? type) value)
        (and (real-type? type) value)
        (cond [(string-type? type) value]
              [(symbol-type? type) (if value (symbol->string value) value)]
              [else #f])
        (cond [(time-tai-type? type) (if value (time-tai->time-utc value) value)]
              [(time-utc-type? type) value]
              [else #f])
        (and (binary-type? type) value)))

; Provides ---------------------------------------

(provide delta-types)

(provide/contract/entities
 [entity audit-delta]
 [make-insert-delta     (-> audit-transaction? snooze-struct? audit-delta?)]
 [make-update-delta     (-> audit-transaction? snooze-struct? attribute? audit-delta?)]
 [make-delete-delta     (-> audit-transaction? snooze-struct? audit-delta?)]
 [audit-delta-entity    (-> audit-delta? entity?)]
 [audit-delta-attribute (-> audit-delta? attribute?)]
 [audit-delta-value     (-> audit-delta? any)]
 #;[revert-delta          (-> audit-delta? (or/c snooze-struct? #f) (or/c snooze-struct? #f))])
