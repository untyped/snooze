#lang scheme/base

(require scheme/class
         scheme/contract
         mzlib/etc
         mzlib/kw
         srfi/26/cut
         (planet untyped/unlib:3/gen)
         (planet untyped/unlib:3/parameter)
         "base.ss"
         "era/cache.ss"
         "era/era.ss"
         "generic/connection.ss"
         "generic/database.ss"
         "sql/sql.ss")

; database<%> [#:auto-connect? boolean] -> snooze<%>
(define (make-snooze database #:auto-connect? [auto-connect? #f])
  (new snooze% [database database] [auto-connect? auto-connect?]))

(define snooze%
  (class* (snooze-cache-mixin object%) (snooze<%>)
    
    (inspect #f)
    
    (inherit cache-ref)
    
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
    
    ; (connection struct -> any) connection struct -> any
    ;
    ; A transparent procedure that wraps the body of any
    ; call-with-transaction block. Must return the same value
    ; as the transaction body.
    (define (transaction-hook continue conn struct)
      (continue conn struct))
    
    ; Public interface ---------------------------
    
    ; -> database<%>
    (define/public (get-database)
      database)
    
    ; database<%> -> void
    (define/public (set-database! new-database)
      (set! database new-database))
    
    ; -> ((connection struct -> any) connection struct -> any)
    (define/public (get-transaction-hook)
      transaction-hook)
    
    ; ((connection struct -> any) connection struct -> any) -> void
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
          (error "no database connection: use call-with-connection to set one up.")))
    
    ; entity -> void
    (define/public (create-table entity)
      (auto-connect)
      (send database create-table (current-connection) entity))
    
    ; entity -> void
    (define/public (drop-table entity)
      (auto-connect)
      (send database drop-table (current-connection) entity))
    
    ; snooze-struct -> snooze-struct
    (define/public (save! struct)
      (auto-connect)
      (let* ([guid     (struct-guid struct)]
             [revision (struct-revision struct)]
             [entity   (struct-entity struct)])
        (call-with-transaction
         (lambda ()
           (if (struct-saved? struct)
               (begin (if (and revision (record-exists-with-revision? entity guid revision))
                          ; The audit trail package requires us to update the revision *before* calling the hook:
                          (begin (set-struct-revision! struct (add1 revision))
                                 ((entity-on-save entity)
                                  (lambda (conn struct)
                                    (send database update-record conn struct)
                                    struct)
                                  (current-connection)
                                  struct))
                          (raise-exn exn:fail:snooze:revision
                            "Structure has been revised since it was loaded from the database." 
                            struct)))
               ; The audit trail package requires us to update the revision *before* calling the hook:
               (begin (set-struct-revision! struct 0)
                      ; Run the insert hook:
                      ((entity-on-save entity)
                       (lambda (conn struct)
                         (set-guid-id!
                          (struct-guid struct)
                          (send database insert-record conn struct))
                         struct)
                       (current-connection)
                       struct)))))))
    
    ; snooze-struct -> snooze-struct
    (define/public (delete! struct)
      ; entity
      (define entity (struct-entity struct))
      ; (U integer #f)
      (define revision (struct-revision struct))
      (auto-connect)
      (if (struct-saved? struct)
          (call-with-transaction
           (lambda ()
             (if (and revision
                      (record-exists-with-revision?
                       entity
                       (struct-id struct)
                       (struct-revision struct)))
                 ((entity-on-delete entity)
                  (lambda (conn struct)
                    (send database delete-record conn (struct-guid struct))
                    (set-guid-id! (struct-guid struct) #f)
                    struct)
                  (current-connection)
                  struct)
                 (raise-exn exn:fail:snooze:revision "database has been revised since structure was loaded" struct))))
          (error "cannot delete: struct has not been saved" struct)))
    
    ; snooze-struct [((connection struct -> any) connection struct -> any)] -> snooze-struct
    (define/public (insert/id+revision! struct [hook (lambda (continue conn struct) (continue conn struct))])
      (auto-connect)
      (hook (lambda (conn struct)
              (send database insert-record/id conn struct)
              struct)
            (current-connection)
            struct))
    
    ; snooze-struct [((connection struct -> any) connection struct -> any)] -> snooze-struct
    (define/public (update/id+revision! struct [hook (lambda (continue conn struct) (continue conn struct))])
      (auto-connect)
      (hook (lambda (conn struct)
              (send database update-record conn struct)
              struct)
            (current-connection)
            struct))
    
    ; snooze-struct [((connection struct -> any) connection struct -> any)] -> snooze-struct
    (define/public (delete/id+revision! struct [hook (lambda (continue conn struct) (continue conn struct))])
      (auto-connect)
      (hook (lambda (conn struct)
              (send database delete-record conn (struct-guid struct))
              struct)
            (current-connection)
            struct))
    
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
      (send database g:find this (current-connection) select))
    
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
    
    ; entity (U integer #f) -> (U snooze-struct #f)
    ;(define/public (find-by-id entity id)
    ;  (cond [(not id) #f]
    ;        [(integer? id)
    ;         (let-alias ([x entity])
    ;           (find-one (sql (select #:from x #:where (= x.guid ,(make-guid id)))))]
    ;        [else (raise-type-error 'find-by-id "(U integer #f)" id)]))
    
    ; guid -> persistent-struct
    (define/public (find-by-guid guid)
      (cache-ref guid))
    
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
        (and (find-one (sql (select #:what x.guid
                                    #:from x
                                    #:where (and (= x.guid     ,guid)
                                                 (= x.revision ,revision))))) #t)))))

; Provide statements -----------------------------

; contract
;(define snooze%/c
;  (object-contract
;    
;    [field database            (is-a?/c database<%>)]
;    [field auto-connect?       boolean?]
;    
;    [get-database              (-> (is-a?/c database<%>))]
;    [set-database!             (-> (is-a?/c database<%>) void?)]
;    
;    [get-transaction-pipeline  (-> (listof procedure?))]
;    [set-transaction-pipeline! (-> (listof procedure?) void?)]
;    
;    [call-with-connection      (-> procedure? any)]
;    [connect                   (-> any)]
;    [disconnect                (-> any)]
;    [current-connection        (-> connection?)]
;    
;    [create-table              (-> entity? void?)]
;    [drop-table                (-> (or/c entity? symbol?) void?)]
;    
;    [save!                     (-> snooze-struct? snooze-struct?)]
;    [delete!                   (-> snooze-struct? snooze-struct?)]
;    
;    [insert/id+revision!       (->* (snooze-struct?) 
;                                    ((listof procedure?))
;                                    snooze-struct?)]
;    [update/id+revision!       (->* (snooze-struct?) 
;                                    ((listof procedure?))
;                                    snooze-struct?)]
;    [delete/id+revision!       (->* (snooze-struct?)
;                                    ((listof procedure?))
;                                    snooze-struct?)]
;    
;    [find-all                  (-> query? list?)]
;    [find-one                  (-> query? any)]
;    [g:find                    (-> query? procedure?)]
;    
;    [call-with-transaction     (->* (procedure?) ((or/c string? #f)) any)]
;    
;    [find-by-id                (-> entity? (or/c integer? #f) (or/c snooze-struct? #f))]
;    [find-by-guid              (-> guid? (or/c snooze-struct? #f))]
;    
;    [table-names               (-> (listof symbol?))]
;    [table-exists?             (-> (or/c entity? symbol?) boolean?)]
;    
;    [query->string             (-> query? string?)]
;    [debug-sql                 (->* (query?) (string? output-port?) query?)]))

(provide/contract
 [snooze%     class?]
 ;[snooze%/c   contract?]
 [make-snooze (->* ((is-a?/c database<%>)) (#:auto-connect? boolean?) any)])
