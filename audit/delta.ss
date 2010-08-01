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
    (make-audit-delta
     txn                       ; transaction
     (delta-types insert)      ; type
     (snooze-struct-id struct) ; struct-id
     attr-id                   ; attribute-id
     #f                        ; boolean-value
     #f                        ; integer-value
     #f                        ; real-value
     #f                        ; string-value
     #f                        ; time-utc-value
     #f)))                     ; binary-value

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
    (make-audit-delta
     txn                       ; transaction
     (delta-types delete)      ; type
     (snooze-struct-id struct) ; struct-id
     attr-id                   ; attribute-id
     #f                        ; boolean-value
     #f                        ; integer-value
     #f                        ; real-value
     #f                        ; string-value
     #f                        ; time-utc-value
     #f)))                     ; binary-value

; audit-delta -> entity
(define (audit-delta-entity delta)
  (attribute-entity (audit-delta-attribute delta)))

; audit-delta -> (U attribute #f)
(define (audit-delta-attribute delta)
  (id->attribute (audit-delta-attribute-id delta)))

; audit-delta -> any
(define (audit-delta-value delta)
  (let ([type (attribute-type (audit-delta-attribute delta))])
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
          [(binary-type? type)   (audit-delta-binary-value delta)])))

; Reverts the ID in guid to a temporary (symbolic) value.
; 
; audit-delta (U snooze-struct #f) -> (U snooze-struct #f)
#;(define (revert-delta delta struct)
    (enum-case delta-types (audit-delta-type delta)
      [(insert)        (if (equal? (snooze-struct-id struct)
                                   (audit-delta-struct-id delta))
                           ; Put a temporary ID in to the guid:
                           (begin (set-guid-id! (snooze-struct-guid struct) (entity-make-temporary-id (snooze-struct-entity struct)))
                                  #f)
                           (error "delta does not apply to the correct struct" (list delta struct)))]
      [(update)
       (if (equal? (snooze-struct-id struct) (audit-delta-struct-id delta))
           (snooze-struct-set struct (audit-delta-attribute delta) (audit-delta-value delta))
           (error "delta does not apply to the correct guid" (list delta struct)))]
      [(delete)
       (if struct
           (let* ([entity (audit-delta-entity delta)]
                  [guid   (entity-make-guid entity (audit-delta-struct-id struct))]
                  [struct (apply (entity-private-constructor entity)
                                 guid
                                 1000000000
                                 (map attribute-default (entity-data-attributes entity)))])
             ; Put a dummy database ID into the guid - should be set to a meaningful value by the next accompanying update delta:
             (set-guid-id! guid (audit-delta-struct-id delta))
             struct))]))

; Helpers --------------------------------------

;  type
;  (U boolean integer real string symbol time-tai time-utc)
; ->
;  (list boolean (U integer #f) (U real #f) (U string #f) (U time-utc #f))
(define (expand-value type value)
  (list (if (boolean-type? type) value #f)
        (if (integer-type? type) value #f)
        (if (real-type? type) value #f)
        (cond [(string-type? type) value]
              [(symbol-type? type) (if value (symbol->string value) value)]
              [else #f])
        (cond [(time-tai-type? type) (if value (time-tai->time-utc value) value)]
              [(time-utc-type? type) value]
              [else #f])))

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
