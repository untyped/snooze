#lang scheme/base

(require "base.ss")

(require scheme/class
         srfi/26
         (planet untyped/unlib:3/gen)
         (planet untyped/unlib:3/parameter)
         (prefix-in real: (only-in "era/snooze-struct.ss" make-snooze-struct))
         "cache.ss"
         "era/era.ss"
         "generic/generic.ss"
         "sql/sql.ss")

; database<%> [#:auto-connect? boolean] -> snooze<%>
(define (make-snooze database #:auto-connect? [auto-connect? #f])
  (new snooze% [database database] [auto-connect? auto-connect?]))

(define snooze%
  (class* object% (snooze<%>)
    
    (inspect #f)
    
    ; Fields -------------------------------------
    
    ; (thread-cell (U connection #f))
    ;
    ; A thread-cell to store the current connection. 
    ; See the current-connection method below for more information.
    (field [current-connection-cell (make-thread-cell #f)])
    
    ; (connection guid -> any) connection guid -> any
    ;
    ; A transparent procedure that wraps the body of any
    ; call-with-transaction block. Must return the same value
    ; as the transaction body.
    (field [transaction-hook
            (lambda (continue conn guid)
              (continue conn guid))])
    
    ; (parameter snooze-cache%)
    (field [current-cache (make-parameter (new snooze-cache% [snooze this]))])
    
    ; Constructor --------------------------------
    
    ; database<%>
    (init-field database)
    
    ; boolean
    (init-field [auto-connect? #f])
    
    (super-new)
    
    (send database set-snooze! this)
    
    ; Public interface ---------------------------
    
    ; (-> any) -> any
    (define/public (call-with-cache thunk)
      (parameterize ([current-cache (new snooze-cache% [snooze this] [parent (current-cache)])])
        (thunk)))
    
    ; -> snooze-cache%
    (define/public (get-current-cache)
      (current-cache))
    
    ; -> database<%>
    (define/public (get-database)
      database)
    
    ; database<%> -> void
    (define/public (set-database! new-database)
      (set! database new-database))
    
    ; -> ((connection guid -> any) connection guid -> any)
    (define/public (get-transaction-hook)
      transaction-hook)
    
    ; ((connection guid -> any) connection guid -> any) -> void
    (define/public (set-transaction-hook! hook)
      (set! transaction-hook hook))
    
    ; (-> any) -> any
    (define/public (call-with-connection thunk)
      (dynamic-wind (cut connect)
                    (cut thunk)
                    (cut disconnect)))
    
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
    (define (auto-connect)
      (when (and auto-connect? (not (thread-cell-ref current-connection-cell)))
        (connect)))
    
    ; -> connection
    ;
    ; Returns the current database connection One connection is stored per thread.
    ; If the thread is suspended or killed, the connection is disconnected and set to #f.
    (define/public (current-connection)
      (or (thread-cell-ref current-connection-cell)
          (raise-exn exn:fail:snooze "no database connection: use call-with-connection to set one up")))
    
    ; entity -> void
    (define/public (create-table entity)
      (auto-connect)
      (call-with-transaction (cut send database create-table (current-connection) entity)))
    
    ; entity -> void
    (define/public (drop-table entity)
      (auto-connect)
      (call-with-transaction (cut send database drop-table (current-connection) entity)))
    
    ; guid -> guid
    (define/public (save! guid)
      (auto-connect)
      (let ([entity (guid-entity guid)]
            [cache  (get-current-cache)])
        (call-with-transaction
         (lambda ()
           ((entity-on-save entity)
            (lambda (conn guid)
              (let ([saved-struct (if (struct-saved? guid)
                                      (send database update-struct conn (guid-ref guid))
                                      (send database insert-struct conn (guid-ref guid)))])
                (send cache add-saved-struct! saved-struct guid)))
            (current-connection)
            guid)))))
    
    ; guid -> guid
    (define/public (delete! guid)
      (debug "deleting" guid)
      (unless (struct-saved? guid)
        (raise-exn exn:fail:snooze "Unsaved structs cannot be deleted"))
      (auto-connect)
      (let ([entity (guid-entity guid)]
            [cache  (get-current-cache)])
        (call-with-transaction
         (lambda ()
           ((entity-on-delete entity)
            (lambda (conn guid)
              (let ([deleted-struct (send database delete-struct conn (guid-ref guid))])
                (send cache add-delete-struct! deleted-struct guid)))
            (current-connection)
            guid)))))
    
    ; guid [((connection guid -> any) connection guid -> any)] -> guid
    (define/public (insert/id+revision! guid [hook (lambda (continue conn guid) (continue conn guid))])
      (auto-connect)
      (hook (lambda (conn guid)
              (send database insert-record/id conn guid)
              guid)
            (current-connection)
            guid))
    
    ; guid [((connection guid -> any) connection guid -> any)] -> guid
    (define/public (update/id+revision! guid [hook (lambda (continue conn guid) (continue conn guid))])
      (auto-connect)
      (hook (lambda (conn guid)
              (send database update-record conn guid)
              guid)
            (current-connection)
            guid))
    
    ; guid [((connection guid -> any) connection guid -> any)] -> guid
    (define/public (delete/id+revision! guid [hook (lambda (continue conn guid) (continue conn guid))])
      (auto-connect)
      (hook (lambda (conn guid)
              (send database delete-record conn guid)
              guid)
            (current-connection)
            guid))
    
    ; query -> (list-of result)
    (define/public (find-all query)
      (g:collect (g:find query)))
    
    ; select -> (U result #f)
    (define/public (find-one query)
      (let ([result ((g:find query))])
        (and (not (g:end? result)) result)))
    
    ; select -> result-generator
    (define/public (g:find select)
      (auto-connect)
      (send database g:find (current-connection) select))
    
    ; thunk any ... -> any
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
    (define/public (call-with-transaction body . metadata-args)
      (auto-connect)
      (let ([conn (current-connection)])
        (if (send database transaction-allowed? conn)
            (let ([hook (get-transaction-hook)])
              (send database call-with-transaction
                    conn
                    (lambda ()
                      (hook (lambda _ (body))
                            conn
                            metadata-args))))
            (body))))
    
    ; -> (listof symbol)
    (define/public (table-names)
      (auto-connect)
      (send database table-names (current-connection)))
    
    ; (U symbol entity) -> boolean
    (define/public (table-exists? table)
      (auto-connect)
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
    (define/public (debug-sql query [format "~a~n"] [output-port (current-output-port)])
      (send database debug-sql query output-port format))
    
    ; Helpers ------------------------------------
    
    ; entity integer integer -> boolean
    (define (record-exists-with-revision? entity guid revision)
      ; entity-alias
      ; attribute-alias
      ; attribute-alias
      (let-alias ([x entity])
        ; boolean
        (and (find-one (sql (select #:what  x.guid
                                    #:from  x
                                    #:where (and (= x.guid ,guid) (= x.revision ,revision)))))
             #t)))))

; Provide statements -----------------------------

(provide/contract
 [snooze%     class?]
 [make-snooze (->* ((is-a?/c database<%>)) (#:auto-connect? boolean?) any)])
