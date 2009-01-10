#lang scheme/base

(require mzlib/etc
         scheme/class
         scheme/contract
         scheme/match
         (only-in srfi/1/list 
                  delete-duplicates
                  unzip2)
         srfi/19/time
         srfi/26/cut
         (planet untyped/unlib:3/hash)
         (planet untyped/unlib:3/parameter)
         (planet untyped/unlib:3/pipeline)
         "../snooze.ss"
         "attribute.ss"
         "delta.ss"
         "entity.ss"
         "frame.ss"
         "transaction.ss")

(define-struct audit-stage (proc)
  #:property prop:procedure 0)

(define audit-trail<%>
  (interface (delta-api<%> entity-cache<%> attribute-cache<%>)
    current-audit-transaction
    audit-transaction-deltas
    audit-deltas->guids
    audit-struct-history
    audit-struct-transaction-history
    audit-struct-snapshot
    audit-transaction-affected
    audit-roll-back!))

(define audit-trail%
  (class* delta-api% (audit-trail<%>)
    
    (inherit id->entity
             entity->id
             id->attribute
             attribute->id
             audit-delta-guid
             revert-delta!)
    
    (inherit-field snooze
                   entity-cache
                   attribute-cache)
    
    ; Fields -------------------------------------
    
    ; (listof entity)
    (init-field entities)
    
    ; (parameter boolean)
    (field [in-audit? (make-parameter #f)])
    
    ; (parameter (U audit-frame #f))
    (field [current-audit-frame (make-parameter #f)])
    
    (define-alias ENTITY audit-entity)
    (define-alias ATTR   audit-attribute)
    (define-alias DELTA  audit-delta)
    (define-alias TXN    audit-transaction)
    
    ; stage
    ; stage
    ; stage
    ; stage
    (field [transaction-stage #f])
    (field [insert-stage #f])
    (field [update-stage #f])
    (field [delete-stage #f])
    
    ; Constructor --------------------------------
    
    (init [(init-snooze snooze)])
    
    ; entity-cache%
    (init [(init-entity-cache entity-cache)
           (new entity-cache%
                [snooze         init-snooze])])
    
    ; attribute-cache%
    (init [(init-attribute-cache attribute-cache)
           (new attribute-cache%
                [snooze         init-snooze]
                [entity-cache   init-entity-cache])])
    
    (super-new [snooze init-snooze]
               [entity-cache    init-entity-cache]
               [attribute-cache init-attribute-cache])
    
    (unless (is-a? snooze snooze<%>)
      (raise-type-error 'snooze "snooze<%>" snooze))
    
    ; Check we're not trying to audit parts of the audit trail itself
    ; (we can't do audit-metadata but that's less important):
    (for ([entity entities])
      (when (memq entity (list entity:audit-attribute
                               entity:audit-entity
                               entity:audit-delta
                               entity:audit-transaction))
        (error (format "~a cannot be audited" entity))))
    
    ; Helpers ------------------------------------
    
    (define-snooze-interface snooze)
    
    ; Public methods -----------------------------
    
    ; -> void
    (define/public (init!)
      
      ; (stage (connection any ... -> any))
      (set! transaction-stage
            (make-audit-stage
             (lambda (continue conn . metadata-args)
               (if (in-audit?)
                   (apply continue conn metadata-args)
                   (parameterize ([in-audit? #t])
                     (let* ([frame (new audit-frame% 
                                        [trail           this]
                                        [snooze          snooze]
                                        [entity-cache    entity-cache]
                                        [attribute-cache attribute-cache])])
                       (send frame on-transaction-start)
                       (parameterize ([current-audit-frame frame])
                         (begin0 (apply continue conn metadata-args)
                                 (send frame on-transaction-end
                                       (and (pair? metadata-args)
                                            (send/apply this make-metadata
                                                        (send frame get-transaction)
                                                        metadata-args)))))))))))
      
      ; (stage (connection persistent-struct -> persistent-struct))
      ;
      ; Audit after the insert to make sure we have an ID.
      (set! insert-stage
            (make-audit-stage
             (lambda (continue conn struct)
               (begin0
                 (continue conn struct)
                 (parameterize ([in-audit? #t])
                   (send (current-audit-frame) audit-insert! struct))))))
      
      ; (stage (connection persistent-struct -> persistent-struct))
      ;
      ; Audit before the update to make sure the original information is in the database.
      (set! update-stage
            (make-audit-stage
             (lambda (continue conn struct)
               (parameterize ([in-audit? #t])
                 (send (current-audit-frame) audit-update! struct))
               (continue conn struct))))
      
      ; (stage (connection persistent-struct -> persistent-struct))
      ;
      ; Audit before the delete to make sure the original information is in the database.
      (set! delete-stage
            (make-audit-stage
             (lambda (continue conn struct)
               (parameterize ([in-audit? #t])
                 (send (current-audit-frame) audit-delete! struct))
               (continue conn struct))))
      
      ; Initialize the database and pipelines:
      (parameterize ([in-audit? #t])
        
        ; Ensure the delta table exists:
        (unless (table-exists? entity:audit-entity)
          (create-table entity:audit-entity))
        
        ; Ensure the delta table exists:
        (unless (table-exists? entity:audit-attribute)
          (create-table entity:audit-attribute))
        
        ; Ensure the transaction table exists:
        (unless (table-exists? entity:audit-transaction)
          (create-table entity:audit-transaction))
        
        ; Ensure the delta table exists:
        (unless (table-exists? entity:audit-delta)
          (create-table entity:audit-delta))
        
        ; Install the transaction pipeline stage:
        (send snooze set-transaction-pipeline! (cons transaction-stage (send snooze get-transaction-pipeline)))
        
        ; Install the insert, update and delete pipeline stages:
        (for-each (lambda (entity)
                    (set-entity-insert-pipeline! entity (append (entity-insert-pipeline entity) (list insert-stage)))
                    (set-entity-update-pipeline! entity (append (entity-update-pipeline entity) (list update-stage)))
                    (set-entity-delete-pipeline! entity (append (entity-delete-pipeline entity) (list delete-stage))))
                  entities)))
    
    ; -> (U audit-transaction #f)
    (define/public (current-audit-transaction)
      (and (current-audit-frame)
           (send (current-audit-frame) get-transaction)))
    
    ; audit-transaction any ... -> (U persistent-struct #f)
    (define/public (make-metadata txn . metadata-args)
      #f)
    
    ; -> void
    (define/public (clear!)
      (parameterize ([in-audit? #t])
        (for-each delete! (append (find-all (sql:select #:from ENTITY))
                                  (find-all (sql:select #:from ATTR))
                                  (find-all (sql:select #:from TXN))
                                  (find-all (sql:select #:from DELTA))))
        (send this clear-cache!)))
    
    ; -> void
    (define/public (drop!)
      (drop-table entity:audit-entity)
      (drop-table entity:audit-attribute)
      (drop-table entity:audit-transaction)
      (drop-table entity:audit-delta))
    
    ; (listof any) -> (listof any)
    (define/public (format-log-values log-values)
      log-values)
    
    ; audit-transaction -> (listof audit-delta)
    (define/public (audit-transaction-deltas txn)
      (find-all (sql:select #:what  DELTA
                            #:from  (sql:inner TXN DELTA (sql:= TXN-id DELTA-transaction-id))
                            #:where (sql:= TXN-id (struct-id txn)))))
    
    ; (listof audit-delta) -> (listof guid)
    (define/public (audit-deltas->guids deltas)
      (map (lambda (delta)
             (define entity (id->entity (audit-delta-entity-id delta)))
             (make-guid entity (audit-delta-struct-id delta)))
           (sort (delete-duplicates
                  deltas
                  (lambda (a b)
                    (= (audit-delta-struct-id a)
                       (audit-delta-struct-id b))))
                 (lambda (a b)
                   (if (= (audit-delta-entity-id a) (audit-delta-entity-id b))
                       (< (audit-delta-struct-id a) (audit-delta-struct-id b))
                       (< (audit-delta-entity-id a) (audit-delta-entity-id b)))))))
    
    ; guid txn -> (listof delta)
    ;
    ; Find all deltas involving this guid, from the supplied transaction to the present (inclusive).
    ; Deltas are returned in reverse chronological order, from the present back to txn.
    (define/public audit-struct-history
      (opt-lambda (guid txn [inclusive? #t])
        ; entity
        (define entity (guid-entity guid))
        ; integer
        (define entity-id (entity->id entity))
        ; sql:expr
        (define sql:greater? (if inclusive? sql:>= sql:>))
        ; (listof delta)
        (find-all (sql:select #:what  DELTA
                              #:from  DELTA
                              #:where (sql:and (sql:= DELTA-entity-id entity-id)
                                               (sql:= DELTA-struct-id (guid-id guid))
                                               (sql:greater? DELTA-transaction-id (struct-id txn)))
                              #:order (list (sql:desc DELTA-id))))))
    
    ; guid txn -> (listof transaction)
    ;
    ; Returns the list of transactions that affect guid, from txn to the present.
    ; Transactions are returned in chronological order. The optional third argument
    ; determines whether txn itself should be included in the list (#t by default).
    (define/public audit-struct-transaction-history
      (opt-lambda (guid txn [inclusive? #t])
        ; entity
        (define entity (guid-entity guid))
        ; integer
        (define entity-id (entity->id entity))
        ; sql:expr
        (define sql:greater? (if inclusive? sql:>= sql:>))
        ; (listof transaction)
        (find-all (sql:select #:what  TXN
                              #:from  (sql:inner TXN DELTA (sql:= TXN-id DELTA-transaction-id))
                              #:where (sql:and (sql:= DELTA-entity-id entity-id)
                                               (sql:= DELTA-struct-id (guid-id guid))
                                               (sql:greater? TXN-id (struct-id txn)))
                              #:order (list (sql:asc DELTA-id))))))
    
    ; guid audit-transaction -> persistent-struct
    (define/public (audit-struct-snapshot guid history)
      (define struct (find-by-guid guid))
      (foldl (cut revert-delta! guid <> <>)
             struct
             history))
    
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
    (define/public (audit-transaction-affected txn0)
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
              [(< (struct-id txn) (struct-id old-txn))
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
    (define/public (audit-roll-back! affected . log-values)
      ; (listof integer)
      (define transaction-ids 
        (map struct-id (hash-values affected)))
      ; (listof (listof delta))
      ;
      ; A list of groups of deltas, where each group represents the changes to
      ; a particular struct in a particular transaction.
      ;
      ; Deltas are retrieved from the database in chronological order and consed into
      ; a list. This means the list is in reverse chronological order and can be folded
      ; over to revert the structs in the correct sequence.
      (define delta-groups
        (let ([gen (g:find (sql:select #:what  DELTA
                                       #:from  (sql:inner TXN DELTA (sql:= TXN-id DELTA-transaction-id))
                                       #:where (sql:in TXN-id transaction-ids)
                                       #:order (list (sql:asc (sql:attr DELTA 'id)))))])
          (let loop ([k-group null] [k-all null])
            (let ([next (gen)])
              (cond [(g:end? next) 
                     (if (null? k-group)
                         k-all
                         (cons k-group k-all))]
                    [(null? k-group)
                     (loop (cons next k-group) k-all)]
                    [(and (equal? (audit-delta-type next) (audit-delta-type (car k-group)))
                          (equal? (audit-delta-guid next) (audit-delta-guid (car k-group))))
                     (loop (cons next k-group) k-all)]
                    [else (loop (list next) (cons k-group k-all))])))))
      ; (hashof guid (U persistent-struct #f))
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
                           (define guid (audit-delta-guid (car deltas)))
                           (define old-struct (hash-ref working guid))
                           (define new-struct 
                             (foldl (cut revert-delta! guid <> <>)
                                    old-struct
                                    deltas))
                           ;(pretty-print (list (cons "FROM" old-struct) (cons "TO" new-struct) (cons "DELTAS" deltas)))
                           (case (audit-delta-type (car deltas))
                             [(I) (delete/id+revision! old-struct (list delete-stage))]
                             [(U) (update/id+revision! new-struct (list update-stage))]
                             [(D) (insert/id+revision! new-struct (list insert-stage))])
                           (hash-set! working guid new-struct))
                         delta-groups))
             log-values))
    
    (inspect #f)))

; Provide statements -----------------------------

(provide (all-from-out "attribute.ss")
         (all-from-out "entity.ss")
         (all-from-out "transaction.ss")
         (all-from-out "delta.ss"))

(provide audit-trail<%>
         audit-trail%)
