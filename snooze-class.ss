#lang scheme/base

(require "base.ss")

(require scheme/class
         scheme/list
         srfi/26
         (planet untyped/unlib:3/gen)
         (planet untyped/unlib:3/parameter)
         "core/core.ss"
         "common/common.ss"
         "sql/sql.ss")

; Logging ----------------------------------------

; (parameter (U (query natural -> void) #f))
(define query-logger (make-parameter #f))

; (parameter (U (guid natural -> void) #f))
(define direct-find-logger (make-parameter #f))

(define (call-with-log logger msg thunk)
  (if logger
      (let ([start #f])
        (dynamic-wind
         (lambda ()
           (set! start (current-inexact-milliseconds)))
         thunk
         (lambda ()
           (logger msg (- (current-inexact-milliseconds) start)))))
      (thunk)))

; Constructor ------------------------------------

; database<%> [#:connect-on-demand? boolean] -> snooze<%>
(define (make-snooze database #:connect-on-demand? [connect-on-demand? #f])
  (new snooze% [database database] [connect-on-demand? connect-on-demand?]))

; Classes ----------------------------------------

(define snooze%
  (class* object% (snooze<%>)
    
    (inspect #f)
    
    ; Fields -------------------------------------
    
    ; (thread-cell (U connection #f))
    ;
    ; A thread-cell to store the current connection. 
    ; See the current-connection method below for more information.
    (field [current-connection-cell (make-thread-cell #f)])
    
    ; (parameter (U transaction-frame #f))
    (field [current-transaction-frame (make-parameter #f)])
    
    ; (connection snooze-struct -> any) connection snooze-struct -> any
    ;
    ; A transparent procedure that wraps the body of any
    ; call-with-transaction block. Must return the same value
    ; as the transaction body.
    (field [transaction-hook (lambda (continue conn struct) (continue conn struct))])
    
    ; Constructor --------------------------------
    
    ; database<%>
    (init-field database)
    
    ; boolean
    (init-field [connect-on-demand? #f])
    
    (super-new)
    
    (send database set-snooze! this)
    
    ; Public interface ---------------------------
    
    ; -> database<%>
    (define/public (get-database)
      database)
    
    ; database<%> -> void
    (define/public (set-database! new-database)
      (set! database new-database))
    
    ; -> ((connection snooze-struct -> any) connection snooze-struct -> any)
    (define/public (get-transaction-hook)
      transaction-hook)
    
    ; ((connection snooze-struct -> any) connection snooze-struct -> any) -> void
    (define/public (set-transaction-hook! hook)
      (set! transaction-hook hook))
    
    ; (-> any) -> any
    (define/public (call-with-connection thunk)
      (if (thread-cell-ref current-connection-cell)
          (thunk)
          (dynamic-wind (if connect-on-demand?
                            void
                            (cut connect))
                        (cut thunk)
                        (cut disconnect))))
    
    ; -> void
    (define/public (connect)
      (unless (thread-cell-ref current-connection-cell)
        (thread-cell-set! current-connection-cell (send database connect))))
    
    ; -> void
    (define/public (disconnect)
      (when (thread-cell-ref current-connection-cell)
        (send database disconnect (thread-cell-ref current-connection-cell))
        (thread-cell-set! current-connection-cell #f)))
    
    ; -> void
    (define (connect-on-demand)
      (when (and connect-on-demand? (not (thread-cell-ref current-connection-cell)))
        (connect)))
    
    ; -> connection
    (define/public (current-connection)
      (or (thread-cell-ref current-connection-cell)
          (raise-exn exn:fail:snooze "no database connection: use call-with-connection to set one up")))
    
    ; entity -> void
    (define/public (create-table entity)
      (call-with-transaction (cut send database create-table (current-connection) entity)))
    
    ; entity -> void
    (define/public (drop-table entity)
      (connect-on-demand)
      (call-with-transaction (cut send database drop-table (current-connection) entity)))
    
    ; snooze-struct -> snooze-struct
    (define/public (save! struct)
      (connect-on-demand)
      (parameterize ([currently-saving struct])
        (let ([guid   (snooze-struct-guid   struct)]
              [entity (snooze-struct-entity struct)])
          (call-with-transaction
           (lambda ()
             (transaction-frame-data-add! (get-current-transaction-frame) guid)
             ((entity-on-save entity)
              (lambda (conn struct)
                (transaction-frame-cache-add! 
                 (get-current-transaction-frame)
                 (begin
                   (if (snooze-struct-saved? struct)
                       (send database update-struct conn struct)
                       (send database insert-struct conn struct)))))
              (current-connection)
              struct))))))
    
    ; snooze-struct -> snooze-struct
    (define/public (delete! struct)
      (unless (snooze-struct-saved? struct)
        (raise-exn exn:fail:snooze (format "unsaved structs cannot be deleted ~a" struct)))
      (connect-on-demand)
      (parameterize ([currently-deleting struct])
        (let ([guid   (snooze-struct-guid   struct)]
              [entity (snooze-struct-entity struct)])
          (call-with-transaction
           (lambda ()
             (transaction-frame-data-add! (get-current-transaction-frame) guid)
             ((entity-on-delete entity)
              (lambda (conn struct)
                (transaction-frame-cache-remove!
                 (get-current-transaction-frame)
                 (send database delete-struct conn struct)))
              (current-connection)
              struct))))))
    
    ; query -> (list-of result)
    (define/public (find-all query)
      (g:collect (g:find query)))
    
    ; select -> (U result #f)
    (define/public (find-one query)
      (let ([result ((g:find query))])
        (and (not (g:end? result)) result)))
    
    ; select -> result-generator
    (define/public (g:find select)
      (connect-on-demand)
      (call-with-log
       (query-logger)
       select
       (lambda ()
         (send database g:find
               (current-connection)
               select
               (or (get-current-transaction-frame)
                   (transaction-frame-push #f))))))
    
    ; entity natural -> snooze-struct
    (define/public (find-by-id entity id)
      (let ([ans (find-by-guids (list (entity-make-guid entity id)))])
        (and (pair? ans) (car ans))))
    
    ; database-guid -> snooze-struct
    (define/public (find-by-guid guid)
      (let ([ans (find-by-guids (list guid))])
        (and (pair? ans) (car ans))))
    
    ; (listof database-guid) -> (listof snooze-struct)
    (define/public (find-by-guids guids)
      (connect-on-demand)
      (call-with-log
       (direct-find-logger)
       guids
       (lambda ()
         (cond [(null? guids) null]
               [(null? (cdr guids))
                (g:collect (send (get-database) direct-find
                                 (current-connection)
                                 guids
                                 (get-current-transaction-frame)))]
               [else (let ([lookup (g:collect/hash
                                    (send (get-database) direct-find
                                          (current-connection)
                                          (remove-duplicates guids)
                                          (get-current-transaction-frame))
                                    snooze-struct-guid)])
                       (map (cut hash-ref lookup <>) guids))]))))
    
    ; Takes:
    ;   - a list of structs of the same entity;
    ;   - a foreign key attribute from that entity.
    ;
    ; Iterates through the structs finding any unloaded database-guids in the foreign key attribute.
    ; Loads the related structs and cross references the foreign keys.
    ; Returns the original (listof struct) argument, mutated with the cross references in place.
    ;
    ; (listof snooze-struct) attribute -> (listof snooze-struct)
    (define/public (load-related! structs attr)
      (let ([entity   (attribute-entity attr)]
            [accessor (attribute-private-accessor attr)]
            [mutator  (attribute-private-mutator  attr)])
        
        ; Quick type check:
        (unless (and (guid-type? (attribute-type attr))
                     (memq attr (entity-data-attributes entity)))
          (raise-type-error 'find-related! "foreign-key-attribute" attr))
        
        (let*-values ([(to-mutate to-find)
                       ; Work out which foreign keys need loading, and which structs need mutating:
                       (for/fold ([to-mutate null]
                                  [to-find   null])
                                 ([struct (in-list structs)])
                                 (let ([val (accessor struct)])
                                   (if (database-guid? val)
                                       (values (cons struct to-mutate)
                                               (cons val    to-find))
                                       (values to-mutate to-find))))]
                      ; Look up the related structs:
                      [(found) (find-by-guids to-find)])
          ; Mutate the original structs:
          (for ([struct (in-list to-mutate)]
                [found  (in-list found)])
            (mutator struct found))
          ; Return the original argument:
          structs)))
    
    ; Takes:
    ;   - a list of structs of the same entity;
    ;   - a foreign key attribute from that entity;
    ;   - a list of structs of the foreign key type.
    ;
    ; Iterates through the structs finding any unloaded database-guids in the foreign key attribute.
    ; Loads the related structs and cross references the foreign keys.
    ; Returns the original (listof struct) argument, mutated with the cross references in place.
    ;
    ; (listof snooze-struct) attribute -> (listof snooze-struct)
    (define/public (cross-reference-related! structs attr other-structs)
      (let ([entity       (attribute-entity attr)]
            [accessor     (attribute-private-accessor attr)]
            [mutator      (attribute-private-mutator  attr)]
            [lookup-table (make-hash)])
        
        ; database-guid -> (U database-guid snooze-struct)
        (define (lookup guid)
          (hash-ref lookup-table guid guid))
        
        ; Quick type check:
        (unless (and (guid-type? (attribute-type attr))
                     (memq attr (entity-data-attributes entity)))
          (raise-type-error 'find-related! "foreign-key-attribute" attr))
        
        ; Populate the lookup table:
        (for ([struct (in-list other-structs)])
          (hash-set! lookup-table (snooze-struct-guid struct) struct))
        
        ; Work out which foreign keys need loading, and which structs need mutating:
        (for ([struct (in-list structs)])
          (let ([val (accessor struct)])
            (when (database-guid? val)
              ; Lookup returns:
              ;   - the new struct if it's in lookup-table;
              ;   - the original guid if the struct isn't in the lookup-table;
              (mutator struct (lookup val)))))
        
        ; Return the original argument:
        structs))
    
    ; thunk [#:metadata list] -> any
    ;
    ; If the database allows it, a transaction is started and the thunk argument
    ; is called. Some databases do not allow nested transactions, so a new
    ; transaction is not guaranteed at all times with all backends.
    ;
    ; If the thunk is allowed to finish gracefully, the transaction is committed.
    ;
    ; If, however, execution is terminated via an exception or escape 
    ; continuation, the transaction is rolled back.
    ;
    ; A continuation barrier is installed around the transaction to prevent 
    ; arbitrary jumps into and out of the body.
    ;
    ; You are advised to only allow a single thread to execute within a transaction body.
    ;
    ; The extra arguments are passed to the transaction hook (if it is present)
    ; but *not* to the body thunk.
    (define/public (call-with-transaction #:metadata [metadata null] body)
      (connect-on-demand)
      (let ([conn (current-connection)])
        (if (send database transaction-allowed? conn)
            (call-with-transaction-frame 
             (lambda ()
               (let ([hook (get-transaction-hook)])
                 (send database call-with-transaction
                       conn
                       (lambda ()
                         (hook (lambda _ (body)) conn metadata))))))
            (body))))
    
    ; -> (U transaction-frame #f)
    (define/public (get-current-transaction-frame)
      (current-transaction-frame))
    
    ; (-> any) -> any
    (define/private (call-with-transaction-frame thunk)
      (let ([frame (transaction-frame-push (get-current-transaction-frame))]
            [complete? #f])
        (call-with-continuation-barrier
         (lambda ()
           (dynamic-wind 
            ; Entry
            void
            ; Body
            (lambda ()
              (parameterize ([current-transaction-frame frame])
                (begin0
                  (thunk)
                  (set! complete? #t))))
            ; Exit
            (lambda ()
              (if complete?
                  (transaction-frame-commit! frame)
                  (transaction-frame-rollback! frame))))))))
    
    ; -> (listof symbol)
    (define/public (table-names)
      (connect-on-demand)
      (send database table-names (current-connection)))
    
    ; (U symbol entity) -> boolean
    (define/public (table-exists? table)
      (connect-on-demand)
      (send database table-exists? (current-connection) table))
    
    ; query -> string
    (define/public (query->string query)
      (let ([out (open-output-string)])
        (send database debug-sql query out "~a")
        (get-output-string out)))
    
    ;  select
    ;  [string]
    ;  [output-port]
    ; ->
    ;  select
    ;
    ; Prints an SQL string to stdout as a side effect.
    (define/public (debug-sql query #:format [format "~a~n"] #:output-port [output-port (current-output-port)])
      (send database debug-sql query output-port format))))

; Provide statements -----------------------------

(provide/contract
 [query-logger       (parameter/c (-> query? number? any))]
 [direct-find-logger (parameter/c (-> (listof database-guid?) number? any))]
 [snooze%            class?]
 [make-snooze        (->* ((is-a?/c database<%>)) (#:connect-on-demand? boolean?) any)])
