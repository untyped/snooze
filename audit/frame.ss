#lang scheme/base

(require "../base.ss")

(require srfi/19
         (planet untyped/unlib:3/list)
         "../core/core.ss"
         "attribute.ss"
         "delta.ss"
         "transaction.ss")

; Parameters -----------------------------------

; (struct audit-transaction boolean)
(define-struct audit-frame
  (transaction [changes-made? #:mutable])
  #:transparent)

; [#:snooze snooze] -> audit-frame
(define (start-transaction #:snooze [snooze (current-snooze)])
  (make-audit-frame (send snooze save! (make-audit-transaction (current-time time-utc))) #f))

; audit-frame snooze-struct [#:snooze snooze] -> void
(define (end-transaction frame metadata #:snooze [snooze (current-snooze)])
  (if (audit-frame-changes-made? frame)
      (send snooze save! metadata)
      (send snooze delete! (audit-frame-transaction frame)))
  (void))

; audit-frame snooze-struct [#:snooze snooze] -> snooze-struct
(define (audit-insert! frame new-struct #:snooze [snooze (current-snooze)])
  (send snooze save! (make-insert-delta (audit-frame-transaction frame) new-struct))
  (set-audit-frame-changes-made?! frame #t)
  new-struct)

; audit-frame snooze-struct [#:snooze snooze] -> snooze-struct
(define (audit-update! frame new-struct #:snooze [snooze (current-snooze)])
  (let* ([txn        (audit-frame-transaction frame)]
         [entity     (snooze-struct-entity new-struct)]
         [old-struct (send snooze find-by-guid (snooze-struct-guid new-struct))])
    ; Save update deltas for each attribute that has changed:
    (for ([attr      (in-list (cons (entity-revision-attribute entity)
                                    (entity-data-attributes entity)))]
          [old-value (in-list (cons (snooze-struct-revision old-struct)
                                    (snooze-struct-data-ref* old-struct)))]
          [new-value (in-list (cons (snooze-struct-revision new-struct)
                                    (snooze-struct-data-ref* new-struct)))])
      (unless (equal? old-value new-value)
        (let* ([attr-id   (attribute->id attr)]
               [attr-type (attribute-type attr)])
          (send snooze save! (make-update-delta txn old-struct attr)))))
    (set-audit-frame-changes-made?! frame #t)
    new-struct))

; audit-frame snooze-struct [#:snooze snooze] -> snooze-struct
(define (audit-delete! frame new-struct #:snooze [snooze (current-snooze)])
  (let* ([txn        (audit-frame-transaction frame)]
         [entity     (snooze-struct-entity new-struct)]
         [old-struct (send snooze find-by-guid (snooze-struct-guid new-struct))])
    ; Save update deltas for each attribute:
    (for ([attr      (in-list (cons (entity-revision-attribute entity)
                                    (entity-data-attributes entity)))]
          [old-value (in-list (cons (snooze-struct-revision old-struct)
                                    (snooze-struct-data-ref* old-struct)))])
      (let* ([attr-id   (attribute->id attr)]
             [attr-type (attribute-type attr)])
        (send snooze save! (make-update-delta txn old-struct attr))))
    ; Save a delete delta:
    (send snooze save! (make-delete-delta txn old-struct))
    (set-audit-frame-changes-made?! frame #t)
    new-struct))

; Provides ---------------------------------------

(provide/contract
 [struct audit-frame ([transaction   (and/c audit-transaction? snooze-struct-saved?)]
                      [changes-made? boolean?])]
 [start-transaction (->* () (#:snooze (is-a?/c snooze<%>)) audit-frame?)]
 [end-transaction   (->* (audit-frame? (and/c snooze-struct? (not/c snooze-struct-saved?))) (#:snooze (is-a?/c snooze<%>)) void?)]
 [audit-insert!     (->* (audit-frame? snooze-struct?) (#:snooze (is-a?/c snooze<%>)) snooze-struct?)]
 [audit-update!     (->* (audit-frame? snooze-struct?) (#:snooze (is-a?/c snooze<%>)) snooze-struct?)]
 [audit-delete!     (->* (audit-frame? snooze-struct?) (#:snooze (is-a?/c snooze<%>)) snooze-struct?)])
