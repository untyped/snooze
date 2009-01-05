#lang mzscheme

(require mzlib/etc
         (only mzlib/list
               sort)
         scheme/class
         scheme/contract
         (only scheme/private/list
               filter
               foldl)
         scheme/match
         (only srfi/1/list 
               delete-duplicates
               unzip2)
         srfi/19/time
         srfi/26/cut
         (planet untyped/unlib:3/hash-table)
         (planet untyped/unlib:3/parameter)
         (planet untyped/unlib:3/pipeline)
         "../snooze-mzscheme.ss"
         "attribute.ss"
         "delta.ss"
         "entity.ss"
         "frame.ss"
         "transaction.ss")

(define audit-trail<%>
  (interface (delta-api<%> entity-cache<%> attribute-cache<%>)
    init!
    current-audit-transaction
    format-log-values
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
    
    ; entity
    (init-field entity:audit-transaction)
    
    ; (listof entity)
    (init-field entities)
    
    ; (parameter boolean)
    (field [in-audit? (make-parameter #f)])
    
    ; (parameter (U audit-frame #f))
    (field [current-audit-frame (make-parameter #f)])
    
    (define-alias ENTITY audit-entity)
    (define-alias ATTR   audit-attribute)
    (define-alias DELTA  audit-delta)
    
    (define-sql TXN    (q:entity 'TXN entity:audit-transaction))
    (define-sql TXN-id (q:attr TXN 'id))
    
    ; stage
    ; stage
    ; stage
    ; stage
    (field [transaction-stage #f])
    (field [insert-stage #f])
    (field [update-stage #f])
    (field [delete-stage #f])
    
    (super-new)
    
    ; Helpers ------------------------------------
    
    ; any -> boolean
    (define audit-transaction?
      (entity-predicate entity:audit-transaction))
    
    (define-snooze-interface snooze)
    
    ; Public methods -----------------------------
    
    ; -> void
    (define/public (init!)
      
      ; (stage (connection any ... -> any))
      (set! transaction-stage
            (make-stage
             'transaction-stage
             (lambda (continue conn . log-values)
               (if (in-audit?)
                   (apply continue conn log-values)
                   (parameterize ([in-audit? #t])
                     (let ([frame (new audit-frame% 
                                       [snooze                   snooze]
                                       [entity:audit-transaction entity:audit-transaction]
                                       [entity-cache             entity-cache]
                                       [attribute-cache          attribute-cache])])
                       (send/apply frame init! (format-log-values log-values))
                       (parameterize ([current-audit-frame frame])
                         (begin0 (apply continue conn log-values)
                                 (send frame clean-up!)))))))))
      
      ; (stage (connection persistent-struct -> persistent-struct))
      ;
      ; Audit after the insert to make sure we have an ID.
      (set! insert-stage
            (make-stage
             'insert-stage
             (lambda (continue conn struct)
               (begin0
                 (continue conn struct)
                 (parameterize ([in-audit? #t])
                   (send (current-audit-frame) audit-insert! struct))))))
      
      ; (stage (connection persistent-struct -> persistent-struct))
      ;
      ; Audit before the update to make sure the original information is in the database.
      (set! update-stage
            (make-stage
             'update-stage
             (lambda (continue conn struct)
               (parameterize ([in-audit? #t])
                 (send (current-audit-frame) audit-update! struct))
               (continue conn struct))))
      
      ; (stage (connection persistent-struct -> persistent-struct))
      ;
      ; Audit before the delete to make sure the original information is in the database.
      (set! delete-stage
            (make-stage
             'delete-stage
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
        (unless (find-stage (send snooze get-transaction-pipeline) (stage-name transaction-stage))
          (send snooze set-transaction-pipeline! (cons transaction-stage (send snooze get-transaction-pipeline))))
        
        ; Install the insert, update and delete pipeline stages:
        (for-each (lambda (entity)
                    (unless (find-stage (entity-insert-pipeline entity) (stage-name insert-stage))
                      (set-entity-insert-pipeline! entity (append (entity-insert-pipeline entity) (list insert-stage))))
                    (unless (find-stage (entity-update-pipeline entity) (stage-name update-stage))
                      (set-entity-update-pipeline! entity (append (entity-update-pipeline entity) (list update-stage))))
                    (unless (find-stage (entity-delete-pipeline entity) (stage-name delete-stage))
                      (set-entity-delete-pipeline! entity (append (entity-delete-pipeline entity) (list delete-stage)))))
                  entities)))
    
    ; -> (U audit-transaction #f)
    (define/public (current-audit-transaction)
      (and (current-audit-frame)
           (send (current-audit-frame) get-transaction)))
    
    ; -> void
    (define/public (clear!)
      (parameterize ([in-audit? #t])
        (for-each delete! (append (find-all (q:select #:from ENTITY))
                                  (find-all (q:select #:from ATTR))
                                  (find-all (q:select #:from TXN))
                                  (find-all (q:select #:from DELTA))))
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
      (find-all (q:select #:what  DELTA
                          #:from  (q:inner TXN DELTA (q:= TXN-id DELTA-transaction-id))
                          #:where (q:= TXN-id (struct-id txn)))))
    
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
        ; q:expr
        (define q:greater? (if inclusive? q:>= q:>))
        ; (listof delta)
        (find-all (q:select #:what  DELTA
                            #:from  DELTA
                            #:where (q:and (q:= DELTA-entity-id entity-id)
                                           (q:= DELTA-struct-id (guid-id guid))
                                           (q:greater? DELTA-transaction-id (struct-id txn)))
                            #:order (list (q:desc DELTA-id))))))
    
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
        ; q:expr
        (define q:greater? (if inclusive? q:>= q:>))
        ; (listof transaction)
        (find-all (q:select #:what  TXN
                            #:from  (q:inner TXN DELTA (q:= TXN-id DELTA-transaction-id))
                            #:where (q:and (q:= DELTA-entity-id entity-id)
                                           (q:= DELTA-struct-id (guid-id guid))
                                           (q:greater? TXN-id (struct-id txn)))
                            #:order (list (q:asc DELTA-id))))))
    
    ; guid audit-transaction -> persistent-struct
    (define/public (audit-struct-snapshot guid history)
      (define struct (find-by-guid guid))
      (foldl (cut revert-delta! guid <> <>)
             struct
             history))
    
    ; audit-transaction -> (hash-table-of guid txn)
    ;
    ; Used to calculate the set of structures that would need to be rolled back
    ; if txn0 were rolled back. Searches through the GUIDs affected by txn0 and
    ; later txns, and returns a hash-table of GUIDs to the earliest txns that
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
    ;    (make-hash-table/pairs (cons guid1 txn0)
    ;                           (cons guid2 txn0)
    ;                           (cons guid3 txn0)
    ;                           (cons guid4 txn1)
    ;                           (cons guid5 txn3))
    (define/public (audit-transaction-affected txn0)
      ; (hash-table-of txn txn)
      (define closed-txns (make-hash-table 'equal))
      ; (hash-table-of guid guid)
      (define closed-guids (make-hash-table 'equal))
      ; txn -> void
      (define (close-txn! txn)
        (hash-table-put! closed-txns txn txn))
      ; guid txn -> void
      (define (close-guid! guid txn)
        (define old-txn (hash-table-get closed-guids guid #f))
        (cond [(not old-txn)
               (hash-table-put! closed-guids guid txn)]
              [(< (struct-id txn) (struct-id old-txn))
               (hash-table-put! closed-guids guid txn)]
              [else (void)]))
      ; (listof a) (hash-table-of a a) -> (listof a)
      (define (filter-open elts closed)
        (filter (lambda (elt) 
                  (not (hash-table-get closed elt #f)))
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
    
    ; audit-transaction (hash-table-of guid transaction) any ... -> void
    (define/public (audit-roll-back! affected . log-values)
      ; (listof integer)
      (define transaction-ids 
        (map struct-id (hash-table-values affected)))
      ; (listof (listof delta))
      ;
      ; A list of groups of deltas, where each group represents the changes to
      ; a particular struct in a particular transaction.
      ;
      ; Deltas are retrieved from the database in chronological order and consed into
      ; a list. This means the list is in reverse chronological order and can be folded
      ; over to revert the structs in the correct sequence.
      (define delta-groups
        (let ([gen (g:find (q:select #:what  DELTA
                                     #:from  (q:inner TXN DELTA (q:= TXN-id DELTA-transaction-id))
                                     #:where (q:in TXN-id transaction-ids)
                                     #:order (list (q:asc (q:attr DELTA 'id)))))])
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
      ; (hash-table-of guid (U persistent-struct #f))
      ;
      ; A hash table of guids to current working versions of structs.
      (define working 
        (let ([ans (make-hash-table 'equal)])
          (hash-table-for-each 
           affected
           (lambda (guid txn)
             (hash-table-put! ans guid (find-by-guid guid))))
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
                           (define old-struct (hash-table-get working guid))
                           (define new-struct 
                             (foldl (cut revert-delta! guid <> <>)
                                    old-struct
                                    deltas))
                           ;(pretty-print (list (cons "FROM" old-struct) (cons "TO" new-struct) (cons "DELTAS" deltas)))
                           (case (audit-delta-type (car deltas))
                             [(I) (delete/id+revision! old-struct (list delete-stage))]
                             [(U) (update/id+revision! new-struct (list update-stage))]
                             [(D) (insert/id+revision! new-struct (list insert-stage))])
                           (hash-table-put! working guid new-struct))
                         delta-groups))
             log-values))
    
    (inspect #f)))

; [audit-trail%] snooze<%> entity (listof entity) -> audit-trail<%>
(define make-audit-trail
  (case-lambda 
    [(snooze entity:audit-transaction entities)
     (make-audit-trail audit-trail% snooze entity:audit-transaction entities)]
    [(audit-trail% snooze entity:audit-transaction entities)
     (define entity-cache 
       (new entity-cache%
            [snooze snooze]))
     (define attribute-cache
       (new attribute-cache%
            [snooze snooze]
            [entity-cache entity-cache]))
     (new audit-trail% 
          [snooze snooze]
          [entity:audit-transaction entity:audit-transaction]
          [entities entities]
          [entity-cache entity-cache]
          [attribute-cache attribute-cache])]))

; Provide statements -----------------------------

(provide (all-from "attribute.ss")
         (all-from "entity.ss")
         (all-from "transaction.ss")
         (all-from "delta.ss")
         audit-trail<%>
         audit-trail%)

(provide/contract
 [make-audit-trail (case-> (-> (is-a?/c snooze<%>)
                               entity?
                               (listof entity?)
                               (is-a?/c audit-trail%))
                           (-> (subclass?/c audit-trail%)
                               (is-a?/c snooze<%>)
                               entity?
                               (listof entity?)
                               (is-a?/c audit-trail%)))])
