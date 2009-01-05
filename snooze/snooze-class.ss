#lang scheme/base

(require scheme/class
         scheme/contract
         mzlib/etc
         mzlib/kw
         srfi/26/cut
         (planet untyped/unlib:3/gen)
         (planet untyped/unlib:3/parameter)
         (planet untyped/unlib:3/pipeline)
         "base.ss"
         "snooze-interface.ss"
         "era/era.ss"
         "generic/connection.ss"
         "generic/database.ss"
         "sql/sql.ss")

; database<%> [#:auto-connect? boolean] -> snooze<%>
(define (make-snooze database #:auto-connect? [auto-connect? #f])
  (new snooze% [database database] [auto-connect? auto-connect?]))

(define snooze%
  (class* object% (snooze<%>)
    
    ; Constructor --------------------------------
    
    ; database<%>
    (init-field [database #f])
    
    ; boolean
    (init-field [auto-connect? #f])
    
    (super-new)
    
    ; Fields -------------------------------------
    
    ; current-connection-cell : (thread-cell (U connection #f))
    ;
    ; A thread-cell to store the current connection. 
    ; See the current-connection method below for more information.
    (define current-connection-cell
      (make-thread-cell #f))
    
    ; (listof (stage-> any ... -> any))
    (define transaction-pipeline
      null)
    
    ; Public interface -----------------------------
    
    ; -> database<%>
    (define/public (get-database)
      database)
    
    ; database<%> -> void
    (define/public (set-database! new-database)
      (set! database new-database))
    
    ; -> (listof stage)
    (define/public (get-transaction-pipeline)
      transaction-pipeline)
    
    ; (listof stage) -> void
    (define/public (set-transaction-pipeline! pipeline)
      (set! transaction-pipeline pipeline))
    
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
      (define conn (thread-cell-ref current-connection-cell))
      (if conn
          conn
          (raise-exn exn:fail:snooze
            "No database connection: use call-with-connection to set one up.")))
    
    ; entity -> void
    (define/public (create-table entity)
      (auto-connect)
      (send database create-table (current-connection) entity))
    
    ; entity -> void
    (define/public (drop-table entity)
      (auto-connect)
      (send database drop-table (current-connection) entity))
    
    ; persistent-struct -> persistent-struct
    (define/public (save! struct)
      ; (U integer #f)
      (define id (struct-id struct))
      ; (U integer #f)
      (define revision (struct-revision struct))
      ; entity
      (define entity (struct-entity struct))
      (auto-connect)
      (call-with-transaction
       (lambda ()
         (if id
             (begin (if (and revision (record-exists-with-revision? entity id revision))
                        ; The audit trail package requires us to update the revision *before* calling the pipeline:
                        (begin (set-struct-revision! struct (add1 revision))
                               (call-with-pipeline
                                (append (entity-save-pipeline entity) (entity-update-pipeline entity))
                                (lambda (conn struct)
                                  (send database update-record conn struct))
                                (current-connection)
                                struct)
                               struct)
                        (raise-exn exn:fail:snooze:revision
                          "Structure has been revised since it was loaded from the database." 
                          struct)))
             ; The audit trail package requires us to update the revision *before* calling the pipeline:
             (begin (set-struct-revision! struct 0)
                    ; Run the insert pipeline:
                    (call-with-pipeline
                     (append (entity-save-pipeline entity) (entity-insert-pipeline entity))
                     (lambda (conn struct)
                       (set-struct-id! struct (send database insert-record conn struct)))
                     (current-connection)
                     struct)
                    struct)))))
    
    ; persistent-struct -> persistent-struct
    (define/public (delete! struct)
      ; (U integer #f)
      (define id (struct-id struct))
      ; entity
      (define entity (struct-entity struct))
      ; (U integer #f)
      (define revision (struct-revision struct))
      (auto-connect)
      (if id
          (call-with-transaction
           (lambda ()
             (if (and revision (record-exists-with-revision? entity id (struct-revision struct)))
                 (begin (call-with-pipeline
                         (entity-delete-pipeline entity)
                         (lambda (conn struct)
                           (send database delete-record (current-connection) (struct-guid struct))
                           (set-struct-id! struct #f)
                           struct)
                         (current-connection)
                         struct))
                 (raise-exn exn:fail:snooze:revision
                   "Database has been revised since structure was loaded."
                   struct))))
          (raise-exn exn:fail:snooze
            (format "Cannot delete a struct that has not been saved to the database: ~a" struct))))
    
    ; persistent-struct [(listof stage)] -> persistent-struct
    (define/public (insert/id+revision! struct [pipeline null])
      ; (U integer #f)
      (define id (struct-id struct))
      ; entity
      (define entity (struct-entity struct))
      (auto-connect)
      (call-with-pipeline pipeline
                          (cut send database insert-record/id <> <>)
                          (current-connection)
                          struct)
      struct)
    
    ; persistent-struct [(listof stage)] -> persistent-struct
    (define/public (update/id+revision! struct [pipeline null])
      ; (U integer #f)
      (define id (struct-id struct))
      ; entity
      (define entity (struct-entity struct))
      (auto-connect)
      (call-with-pipeline pipeline
                          (cut send database update-record <> <>)
                          (current-connection)
                          struct)
      struct)
    
    ; persistent-struct [(listof stage)] -> persistent-struct
    (define/public (delete/id+revision! struct [pipeline null])
      ; (U integer #f)
      (define id (struct-id struct))
      ; entity
      (define entity (struct-entity struct))
      ; (U integer #f)
      (define revision (struct-revision struct))
      (auto-connect)
      (call-with-pipeline pipeline
                          (lambda (conn struct)
                            (send database delete-record (current-connection) (struct-guid struct)))
                          (current-connection)
                          struct)
      struct)
    
    ; query -> (list-of result)
    (define/public (find-all query)
      (g:collect (g:find query)))
    
    ; select -> (U result #f)
    (define/public (find-one query)
      (define result ((g:find query)))
      (and (not (g:end? result)) result))
    
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
    ; The extra arguments are passed to the transaction pipeline (if it is present)
    ; but *not* to the body thunk.
    (define/public (call-with-transaction body . log-values)
      ; connection
      (define conn (current-connection))
      (auto-connect)
      ; Main procedure body:
      (if (send database transaction-allowed? conn)
          (call-with-transaction-frame 
           (lambda ()
             (send database call-with-transaction
                   conn
                   (lambda ()
                     (apply call-with-pipeline
                            (get-transaction-pipeline)
                            ; Don't pass the pipeline arguments to the body thunk:
                            (lambda args (body))
                            conn
                            log-values)))))
          (body)))
    
    ; entity (U integer #f) -> (U persistent-struct #f)
    (define/public (find-by-id entity id)
      (cond [(not id) #f]
            [(integer? id)
             (let ([x (sql:entity 'x entity)])
               (find-one (sql:select #:from x #:where (sql:= (sql:attr x 'id) id))))]
            [else (raise-exn exn:fail:snooze
                    (format "Expected (U integer #f), received ~s." id))]))
    
    
    ; guid -> (U persistent-struct #f)
    (define/public (find-by-guid guid)
      (find-by-id (guid-entity guid) (guid-id guid)))
    
    ; -> (listof symbol)
    (define/public (table-names)
      (auto-connect)
      (send database table-names (current-connection)))
    
    ; (U symbol entity) -> boolean
    (define/public (table-exists? table)
      (auto-connect)
      (send database table-exists? (current-connection) table))
    
    ;  select
    ;  [#:output-port output-port]
    ;  [#:format string]
    ; ->
    ;  select
    ;
    ; Prints an SQL string to stdout as a side effect.
    (define/public (dump-sql query [format "~a~n"] [output-port (current-output-port)])
      (send database dump-sql query output-port format))
    
    ; Helpers ---------------------------------------
    
    ; entity integer integer -> boolean
    (define (record-exists-with-revision? entity id revision)
      ; entity-alias
      ; attribute-alias
      ; attribute-alias
      (define x     (sql:entity 'x entity))
      (define x-id  (sql:attr x 'id))
      (define x-rev (sql:attr x 'revision))
      ; boolean
      (if (find-one (sql:select #:what x-id 
                                #:from x
                                #:where (sql:and (sql:= x-id id) 
                                                 (sql:= x-rev revision))))
          #t
          #f))
    
    (inspect #f)))

; Provide statements -----------------------------

; contract
(define snooze%/c
  (object-contract
    
    [field database            (is-a?/c database<%>)]
    [field auto-connect?       boolean?]
    
    [get-database              (-> (is-a?/c database<%>))]
    [set-database!             (-> (is-a?/c database<%>) void?)]
    
    [get-transaction-pipeline  (-> (listof procedure?))]
    [set-transaction-pipeline! (-> (listof procedure?) void?)]
    
    [call-with-connection      (-> procedure? any)]
    [connect                   (-> any)]
    [disconnect                (-> any)]
    [current-connection        (-> connection?)]
    
    [create-table              (-> entity? void?)]
    [drop-table                (-> (or/c entity? symbol?) void?)]
    
    [save!                     (-> persistent-struct? persistent-struct?)]
    [delete!                   (-> persistent-struct? persistent-struct?)]
    
    [insert/id+revision!       (->* (persistent-struct?) 
                                    ((listof procedure?))
                                    persistent-struct?)]
    [update/id+revision!       (->* (persistent-struct?) 
                                    ((listof procedure?))
                                    persistent-struct?)]
    [delete/id+revision!       (->* (persistent-struct?)
                                    ((listof procedure?))
                                    persistent-struct?)]
    
    [find-all                  (-> query? list?)]
    [find-one                  (-> query? any)]
    [g:find                    (-> query? procedure?)]
    
    [call-with-transaction     (->* (procedure?) () #:rest any/c any)]
    
    [find-by-id                (-> entity? (or/c integer? false/c) (or/c persistent-struct? false/c))]
    [find-by-guid              (-> guid? (or/c persistent-struct? false/c))]
    
    [table-names               (-> (listof symbol?))]
    [table-exists?             (-> (or/c entity? symbol?) boolean?)]
    
    [dump-sql                  (->* (query?)
                                    (string? output-port?)
                                    query?)]))

(provide/contract
 [snooze%     class?]
 [snooze%/c   contract?]
 [make-snooze (->* ((is-a?/c database<%>))
                   (#:auto-connect? boolean?)
                   snooze%/c)])
