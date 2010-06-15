#lang scheme/base

(require "../base.ss")

(require scheme/match
         "snooze-struct.ss"
         "struct.ss")

; Struct types -----------------------------------

; In a transaction frame:
;   - the loaded table interns structures loaded from the database;
;   - the saved table records changes to the IDs in guids to they can be rolled back.

; (struct (U transaction-frame #f)
;         (hashof guid (U symbol natural))
;         (hashof guid snooze-struct))
(define-struct transaction-frame
  (parent
   [data  #:mutable]
   [cache #:mutable])
  #:transparent)

; Procedures -------------------------------------

; (U transaction-frame #f) -> transaction-frame
(define (transaction-frame-push frame)
  (make-transaction-frame frame (make-hasheq) (make-hash)))

; transaction-frame guid -> void
(define (transaction-frame-data-add! frame guid)
  (let ([parent (transaction-frame-parent frame)]
        [data   (transaction-frame-data   frame)])
    (hash-set! data guid (guid-id guid))))

; transaction-frame guid -> snooze-struct
(define (transaction-frame-cache-ref frame guid)
  (let ([parent (transaction-frame-parent frame)]
        [cache  (transaction-frame-cache  frame)])
    (hash-ref cache guid (cut and parent (transaction-frame-cache-ref parent guid)))))

; transaction-frame snooze-struct -> snooze-struct
(define (transaction-frame-cache-add! frame struct)
  (let ([parent (transaction-frame-parent frame)]
        [cache  (transaction-frame-cache  frame)])
    (hash-set! cache (snooze-struct-guid struct) struct)
    struct))

; transaction-frame snooze-struct -> snooze-struct
(define (transaction-frame-cache-remove! frame struct)
  (let ([parent (transaction-frame-parent frame)]
        [cache  (transaction-frame-cache  frame)])
    (hash-remove! cache (snooze-struct-guid struct))
    (if parent
        (transaction-frame-cache-remove! parent struct)
        struct)))

; transaction-frame -> void
(define (transaction-frame-commit! frame)
  (let ([parent (transaction-frame-parent frame)]
        [data   (transaction-frame-data   frame)]
        [cache  (transaction-frame-cache  frame)])
    (set-transaction-frame-data!  frame (make-hasheq))
    (set-transaction-frame-cache! frame (make-hash))
    (when parent
      (let ([parent-data  (transaction-frame-data  parent)]
            [parent-cache (transaction-frame-cache parent)])
        (for ([(key val) (in-hash data)])
          (hash-set! parent-data key val))
        (for ([(key val) (in-hash cache)])
          (hash-set! parent-cache key val))))))

; transaction-frame -> void
(define (transaction-frame-rollback! frame)
  (let ([data (transaction-frame-data frame)])
    (set-transaction-frame-data! frame (make-hasheq))
    (set-transaction-frame-cache! frame (make-hash))
    (for ([(guid id) (in-hash data)])
      (set-guid-id! guid id))))

; Provides ---------------------------------------

(provide/contract
 [struct transaction-frame        ([parent (or/c transaction-frame? #f)]
                                   [data   (and/c hash? hash-eq? (hash/c guid? (or/c natural-number/c symbol?)))]
                                   [cache  (and/c hash? hash-eq? (hash/c guid? snooze-struct?))])]
 [transaction-frame-push          (-> (or/c transaction-frame? #f) transaction-frame?)]
 [transaction-frame-data-add!     (-> transaction-frame? guid? void?)]
 [transaction-frame-cache-ref     (-> transaction-frame? guid? (or/c snooze-struct? #f))]
 [transaction-frame-cache-add!    (-> transaction-frame? snooze-struct? snooze-struct?)]
 [transaction-frame-cache-remove! (-> transaction-frame? snooze-struct? snooze-struct?)]
 [transaction-frame-commit!       (-> transaction-frame? void?)]
 [transaction-frame-rollback!     (-> transaction-frame? void?)])