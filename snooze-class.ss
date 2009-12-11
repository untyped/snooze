#lang scheme/base

(require "base.ss")

(require scheme/class
         srfi/26
         (planet untyped/unlib:3/gen)
         (planet untyped/unlib:3/parameter)
         "core/core.ss"
         "common/common.ss"
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
    (init-field [auto-connect? #f])
    
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
          (dynamic-wind (cut connect)
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
    
    ; snooze-struct -> snooze-struct
    (define/public (save! struct)
      (auto-connect)
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
      (auto-connect)
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
      (auto-connect)
      (with-query-profiling
       (lambda ()
         (send database g:find
               (current-connection)
               select
               (or (get-current-transaction-frame)
                   (transaction-frame-push #f))))
       (let ([str (query->string select)])
         (if (> (string-length str) 72)
             (substring str 0 72)
             str))))
    
    ; entity natural -> snooze-struct
    (define/public (find-by-id entity id)
      (find-by-guid (entity-make-guid entity id)))
    
    ; database-guid -> snooze-struct
    (define/public (find-by-guid guid)
      (auto-connect)
      (with-query-profiling
       (lambda ()
         (let ([ans (send (get-database) direct-find
                          (current-connection)
                          (list guid)
                          (get-current-transaction-frame))])
           (and (pair? ans) (car ans))))
       (format "direct-find ~a" guid)))
    
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
      (auto-connect)
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
    (define/public (debug-sql query #:format [format "~a~n"] #:output-port [output-port (current-output-port)])
      (send database debug-sql query output-port format))))

; Helpers ----------------------------------------

(define (with-query-profiling thunk msg)
  (let ([start #f])
    (dynamic-wind
     (lambda ()
       (set! start (current-inexact-milliseconds)))
     thunk
     (lambda ()
       (printf "DB\t~a\t~a\n"
               (real->decimal-string (- (current-inexact-milliseconds) start) 2)
               msg)))))

; Provide statements -----------------------------

(provide/contract
 [snooze%     class?]
 [make-snooze (->* ((is-a?/c database<%>)) (#:auto-connect? boolean?) any)])
