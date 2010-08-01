#lang scheme/base

(require "../base.ss")

(require (only-in srfi/1 drop delete-duplicates unzip2)
         srfi/19
         (planet untyped/unlib:3/hash)
         (planet untyped/unlib:3/parameter)
         "../main.ss"
         "attribute.ss"
         "delta.ss"
         "frame.ss"
         "transaction.ss")

(define-struct audit-hook (original procedure) #:property prop:procedure 1)

(define audit-trail<%>
  (interface ()
    current-audit-transaction
    #;audit-transaction-deltas
    #;audit-deltas->guids
    #;audit-struct-history
    #;audit-struct-transaction-history
    #;audit-struct-snapshot
    #;audit-transaction-affected
    #;audit-roll-back!))

(define audit-trail%
  (class* object% (audit-trail<%>)
    
    ; Fields ---------------------------------------
    
    ; snooze
    (field [snooze (current-snooze)])
    
    ; (parameter boolean)
    (field [in-audit? (make-parameter #f)])
    
    ; (parameter (U audit-frame #f))
    (field [current-frame (make-parameter #f)])
    
    ; Constructor ----------------------------------
    
    (init-field metadata-entity)
    
    (init-field entities)
    
    (super-new)
    
    ; Check that the metadata entity has an audit-transaction attribute:
    (match (entity-data-attributes metadata-entity)
      [(list-rest (app attribute-type type) _)
       (unless (and (guid-type? type) (eq? (guid-type-entity type) audit-transaction))
         (error "the first attribute in the audit metadata entity must be a foreign key to audit-transaction" metadata-entity))]
      [_ (error "the first attribute in the audit metadata entity must be a foreign key to audit-transaction" metadata-entity)])
    
    ; Check we're not trying to audit parts of the audit trail itself:
    (for ([entity entities])
      (when (memq entity (list audit-attribute
                               audit-delta
                               audit-transaction
                               metadata-entity))
        (error (format "~a cannot be audited" entity))))
    
    ; Initialize the database and transaction hooks:
    (parameterize ([in-audit? #t])
      
      (unless (table-exists? audit-attribute)   (create-table audit-attribute))
      (unless (table-exists? audit-transaction) (create-table audit-transaction))
      (unless (table-exists? audit-delta)       (create-table audit-delta))
      (unless (table-exists? metadata-entity)   (create-table metadata-entity))
      
      ; Install the transaction pipeline stage:
      (send snooze set-transaction-hook! (make-transaction-hook snooze))
      
      ; Install the insert, update and delete pipeline stages:
      (for ([entity (in-list entities)])
        (set-entity-on-save!   entity (make-save-hook entity))
        (set-entity-on-delete! entity (make-delete-hook entity))))
    
    ; Cache the attributes we're going to be auditing:
    (for ([entity (in-list entities)])
      (for ([attr (in-list (entity-attributes entity))])
        ; This adds the attribute to the in-memory and in-database caches:
        (attribute->id attr)))
    
    ; Methods --------------------------------------
    
    ; -> (U audit-transaction #f)
    (define/public (current-audit-transaction)
      (and (current-frame)
           (send (current-frame) get-transaction)))
    
    ; audit-frame (listof any) -> (U snooze-struct #f)
    (define/public (make-metadata frame metadata-values)
      (parameterize ([current-snooze snooze])
        (let* ([txn   (audit-frame-transaction frame)]
               [attrs (entity-attributes metadata-entity)]
               [args0 (list* (entity-make-temporary-guid metadata-entity)
                              #f
                              txn
                              metadata-values)]
               [args1 (if (= (length args0) (length attrs))
                          args0
                          (append args0 (map attribute-default (drop attrs (length args0)))))])
          (apply (entity-constructor metadata-entity) args1))))
    
    ; snooze -> transaction-hook
    (define/public (make-transaction-hook snooze)
      (let ([old-hook (send snooze get-transaction-hook)])
        (make-audit-hook
         old-hook
         (lambda (continue conn metadata-values)
           (if (in-audit?)
               (old-hook continue conn metadata-values)
               (parameterize ([in-audit? #t])
                 (let ([frame (start-transaction)])
                   (parameterize ([current-frame frame])
                     (begin0 (old-hook continue conn metadata-values)
                             (end-transaction frame (make-metadata frame metadata-values)))))))))))
    
    ; entity -> save-hook
    (define/public (make-save-hook entity)
      (let ([old-hook (entity-on-save entity)])
        (make-audit-hook
         old-hook
         (lambda (continue conn struct)
           (parameterize ([in-audit? #t])
             (if (snooze-struct-saved? struct)
                 (old-hook continue conn (audit-update! (current-frame) struct #:snooze snooze))
                 (audit-insert! (current-frame) (old-hook continue conn struct) #:snooze snooze)))))))
    
    ; entity -> delete-hook
    (define/public (make-delete-hook entity)
      (let ([old-hook (entity-on-save entity)])
        (make-audit-hook
         old-hook
         (lambda (continue conn struct)
           (parameterize ([in-audit? #t])
             (old-hook continue conn (audit-delete! (current-frame) struct)))))))
    
    ; -> void
    (define/public (clear!)
      (parameterize ([current-snooze snooze]
                     [in-audit?      #t])
        (for-each delete! (append (find-audit-attributes)
                                  (find-audit-transactions)
                                  (find-audit-deltas)))
        (clear-attribute-cache!)))
    
    ; -> void
    (define/public (drop!)
      (parameterize ([current-snooze snooze])
        (drop-table audit-attribute)
        (drop-table audit-transaction)
        (drop-table audit-delta)))
    
    ; audit-transaction -> (listof audit-delta)
    #;(define/public (audit-transaction-deltas txn)
        (select-all #:from  audit-delta
                    #:where (= audit-delta.transaction ,txn)
                    #:order ((asc audit-delta.guid))))
    
    ; (listof audit-delta) -> (listof guid)
    #;(define/public (audit-deltas->guids deltas)
        (for/list ([delta (in-list deltas)])
          (entity-make-guid (audit-delta-entity delta)
                            (audit-delta-struct-id delta))))
    
    ; guid txn -> (listof delta)
    ;
    ; Find all deltas involving this guid, from the supplied transaction to the present (inclusive).
    ; Deltas are returned in reverse chronological order, from the present back to txn.
    #;(define/public (audit-struct-history guid txn [inclusive? #t])
        (let ([entity (guid-entity guid)])
          (select-all #:from  audit-delta
                      #:where (and (= audit-delta.struct-id ,(guid-id guid))
                                   (in audit-delta.attribute ,(map attribute->id (entity-attributes entity)))
                                   ,(if inclusive?
                                        (sql (>= audit-delta.transaction ,txn))
                                        (sql (>  audit-delta.transaction ,txn))))
                      #:order ((desc audit-delta.guid)))))
    
    ; guid txn -> (listof transaction)
    ;
    ; Returns the list of transactions that affect guid, from txn to the present.
    ; Transactions are returned in chronological order. The optional third argument
    ; determines whether txn itself should be included in the list (#t by default).
    #;(define/public (audit-struct-transaction-history guid txn [inclusive? #t])
        (let ([entity (guid-entity guid)])
          (select-all #:what  audit-transaction
                      #:from  (inner audit-transaction audit-delta (= audit-transaction.guid audit-delta.transaction))
                      #:where (and (=  audit-delta.struct-id ,(guid-id guid))
                                   (in audit-delta.attribute ,(map attribute->id (entity-attributes entity)))
                                   ,(if inclusive?
                                        (sql (>= audit-transaction.guid ,txn))
                                        (sql (>  audit-transaction.guid ,txn))))
                      #:order ((asc audit-delta.guid)))))
    
    ; (listof audit-delta) (U snooze-struct #f) -> (U snooze-struct #f)
    #;(define/public (audit-struct-snapshot history start)
        (let ([struct (find-by-guid guid)])
          (foldl (cut revert-delta guid <> <> #:snooze snooze)
                 struct
                 history)))
    
    ; audit-transaction -> (hashof guid txn)
    ;
    ; Used to calculate the set of structures that would need to be rolled back
    ; if txn0 were rolled back. Searches through the GUIDs affected by txn0 and
    ; later txns, and returns a hash of GUIDs to the earliest txns that
    ; affected them (in the time from txn0 to the present).
    ;
    ; Implemented as a breadth first search over affected transactions and guids.
    ;
    ; For example, consider the following transactions (indexed in chronological order):
    ;
    ;   - T0 affects GUIDs 1, 2 and 3
    ;   - T1 affects GUIDs 2, 3 and 4
    ;   - T2 affects GUIDs 6 and 7
    ;   - T3 affects GUIDs 4 and 5
    ;
    ; To roll back T0, we would need to revert GUIDs 1, 2 and 3 to their states
    ; just before T0. However, there is more to it than that: rolling back these GUIDs
    ; would involve rolling back T1, which would require GUID 4 to be rolled back. T3 
    ; also affects GUID 4, so GUID 5 would need to be rolled back to there.
    ;
    ; The complete list of changes required to roll back T0 are:
    ;
    ;   - revert GUID 1 to its state before T0
    ;   - revert GUID 2 to its state before T0
    ;   - revert GUID 3 to its state before T0
    ;   - revert GUID 4 to its state before T1
    ;   - revert GUID 5 to its state before T3
    ; 
    ; Note that T2 is unaffected because GUIDs 6 and 7 are not affected by T0 or any
    ; related successors.
    ;
    ; In the example, this procedure would return:
    ;
    ;    (make-hash/alist (list (cons guid1 txn0)
    ;                           (cons guid2 txn0)
    ;                           (cons guid3 txn0)
    ;                           (cons guid4 txn1)
    ;                           (cons guid5 txn3)))
    #;(define/public (audit-transaction-affected txn0)
        ; (hashof txn txn)
        (define closed-txns (make-hash))
        ; (hashof guid guid)
        (define closed-guids (make-hash))
        ; txn -> void
        (define (close-txn! txn)
          (hash-set! closed-txns txn txn))
        ; guid txn -> void
        (define (close-guid! guid txn)
          (define old-txn (hash-ref closed-guids guid #f))
          (cond [(not old-txn)
                 (hash-set! closed-guids guid txn)]
                [(< (snooze-struct-id txn) (snooze-struct-id old-txn))
                 (hash-set! closed-guids guid txn)]
                [else (void)]))
        ; (listof a) (hashof a a) -> (listof a)
        (define (filter-open elts closed)
          (filter (lambda (elt) 
                    (not (hash-ref closed elt #f)))
                  elts))
        ; (listof txn)
        (let loop ([open (list txn0)])
          (match open
            [(list-rest (? audit-transaction? txn) tail)
             (define deltas (audit-transaction-deltas txn))
             (define guids (audit-deltas->guids deltas))
             (close-txn! txn)
             (loop (append tail (filter-open guids closed-guids)))]
            [(list-rest (? guid? guid) tail)
             (define txns (audit-struct-transaction-history guid txn0 #t))
             (close-guid! guid (car txns))
             (loop (append tail (filter-open txns closed-txns)))]
            [(list) closed-guids])))
    
    ; audit-transaction (hashof guid transaction) any ... -> void
    #;(define/public (audit-roll-back! affected . log-values)
        ; (listof (listof delta))
        ;
        ; A list of groups of deltas, where each group represents the changes to
        ; a particular struct in a particular transaction.
        ;
        ; Deltas are retrieved from the database in chronological order and consed into
        ; a list. This means the list is in reverse chronological order and can be folded
        ; over to revert the structs in the correct sequence.
        (define delta-groups
          (let ([gen (g:select #:what  audit-delta
                               #:from  (inner audit-transaction audit-delta (= audit-transaction.guid audit-delta.transaction))
                               #:where (in audit-transaction.guid ,affected)
                               #:order ((asc audit-delta.guid)))])
            (let loop ([k-group null] [k-all null])
              (let ([next (gen)])
                (cond [(g:end? next) 
                       (if (null? k-group)
                           k-all
                           (cons k-group k-all))]
                      [(null? k-group)
                       (loop (cons next k-group) k-all)]
                      [(and (equal? (audit-delta-type      next) (audit-delta-type      (car k-group)))
                            (eq?    (audit-delta-entity    next) (audit-delta-entity    (car k-group)))
                            (equal? (audit-delta-struct-id next) (audit-delta-struct-id (car k-group))))
                       (loop (cons next k-group) k-all)]
                      [else (loop (list next) (cons k-group k-all))])))))
        ; (hashof guid (U snooze-struct #f))
        ;
        ; A hash table of guids to current working versions of structs.
        (define working 
          (let ([ans (make-hash)])
            (hash-for-each 
             affected
             (lambda (guid txn)
               (hash-set! ans guid (find-by-guid guid))))
            ans))
        ; void
        ;
        ; Work through the list of groups of deltas. For each group, retrieve the
        ; current working struct, revert it, and save it back to the database.
        ; Do this for all groups to end up with the original state.
        (apply call-with-transaction
               (lambda () 
                 (for-each (lambda (deltas)
                             (define id (audit-delta-struct-id (car deltas)))
                             (define old-struct (hash-ref working guid))
                             (define new-struct 
                               (foldl (cut revert-delta! guid <> <>)
                                      old-struct
                                      deltas))
                             ;(pretty-print (list (cons "FROM" old-struct) (cons "TO" new-struct) (cons "audit-deltaS" deltas)))
                             (enum-case delta-types (audit-delta-type (car deltas))
                               [(insert) (delete/id+revision! old-struct (list delete-stage))]
                               [(update) (update/id+revision! new-struct (list update-stage))]
                               [(delete) (insert/id+revision! new-struct (list insert-stage))])
                             (hash-set! working guid new-struct))
                           delta-groups))
               log-values))
    
    (inspect #f)))

; Provides ---------------------------------------

(provide (all-from-out "attribute.ss"
                       "transaction.ss"
                       "delta.ss")
         audit-trail%)
