#lang scheme/base

(require "../base.ss")

(require scheme/match
         "struct.ss")

; Struct types -----------------------------------

; In a transaction frame:
;   - the loaded table interns structures loaded from the database;
;   - the saved table records changes to the IDs in guids to they can be rolled back.

; (struct (U transaction-frame #f)
;         (hashof guid (U symbol natural)))
(define-struct transaction-frame (parent [data #:mutable]) #:transparent)

; Procedures -------------------------------------

; (U transaction-frame #f) -> transaction-frame
(define (transaction-frame-push frame)
  (make-transaction-frame frame (make-hasheq)))

; transaction-frame guid -> void
(define (transaction-frame-add! frame guid)
  (match frame
    [(struct transaction-frame (parent data))
     (hash-set! data guid (guid-id guid))
     (when parent
       (transaction-frame-add! parent guid))]))

; transaction-frame -> void
(define (transaction-frame-rollback! frame)
  (let ([data (transaction-frame-data frame)])
    (set-transaction-frame-data! frame (make-hasheq))
    (for ([(guid id) (in-hash data)])
      (set-guid-id! guid id))))

; Provides ---------------------------------------

(provide/contract
 [struct transaction-frame    ([parent (or/c transaction-frame? #f)]
                               [data   (and/c hash? hash-eq? (hash/c guid? (or/c natural-number/c symbol?)))])]
 [transaction-frame-push      (-> (or/c transaction-frame? #f) transaction-frame?)]
 [transaction-frame-add!      (-> transaction-frame? guid? void?)]
 [transaction-frame-rollback! (-> transaction-frame? void?)])