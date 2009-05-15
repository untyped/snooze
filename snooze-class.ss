#lang scheme/base

(require "base.ss")

(require scheme/class
         srfi/26
         (planet untyped/unlib:3/gen)
         (planet untyped/unlib:3/parameter)
         (prefix-in real: (only-in "era/snooze-struct.ss" make-snooze-struct))
         "cache.ss"
         "era/era.ss"
         "generic/connection.ss"
         "generic/database.ss"
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
    (field [current-connection-cell
            (make-thread-cell #f)])
    
    ; (connection guid -> any) connection guid -> any
    ;
    ; A transparent procedure that wraps the body of any
    ; call-with-transaction block. Must return the same value
    ; as the transaction body.
    (field [transaction-hook
            (lambda (continue conn guid)
              (continue conn guid))])
    
    ; (parameter snooze-cache%)
    (field [current-cache (make-parameter (new snooze-cache%))])
    
    ; Constructor --------------------------------
    
    ; database<%>
    (init-field [database #f])
    
    ; boolean
    (init-field [auto-connect? #f])
    
    (super-new)
    
    ; Public interface ---------------------------
    
    ; (-> any) -> any
    (define/public (call-with-cache thunk)
      (parameterize ([current-cache (new snooze-cache% [parent (current-cache)])])
        (thunk)))
    
    ; -> snooze-cache%
    (define/public (get-current-cache)
      (current-cache))
    
    ; guid -> (U snooze-struct #f)
    (define/public (cache-ref guid)
      (auto-connect)
      (parameterize ([in-cache-code? #t])
        (log-cache "snooze.cache-ref" guid)
        (or (send (current-cache) cache-ref guid)
            (cond [(guid-serial guid)
                   (raise-exn exn:fail:snooze:cache (format "guid with serial not found in cache: ~s" guid))]
                  [(guid-id guid)
                   (let-alias ([x (guid-entity guid)])
                     (let* ([gen (send database g:find
                                       this
                                       (current-connection)
                                       (sql (select #:from x #:where (= x.guid ,guid))))]
                            [ans (gen)])
                       (and (not (g:end? ans)) 
                            (send (current-cache) cache-ref ans))))]
                  [else (raise-exn exn:fail:snooze:cache (format "guid found with no id and no serial: ~s" guid))]))))
    
    ; snooze-struct -> void
    (define/public (cache-add! struct)
      (parameterize ([in-cache-code? #t])
        (log-cache "snooze.cache-add!" struct)
        (send (current-cache) cache-add! struct)))
    
    ; guid -> snooze-struct
    (define/public (cache-remove! guid)
      (parameterize ([in-cache-code? #t])
        (log-cache "snooze.cache-remove!" guid)
        (send (current-cache) cache-remove! guid)))
    
    ; giud (U natural #f) (U symbol #f) -> void
    (define/public (recache! guid id serial)
      (parameterize ([in-cache-code? #t])
        (log-cache "snooze.recache!" (list guid id serial))
        (send (current-cache) recache! guid id serial)))
    
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
      (let* ([entity        (guid-entity     guid)]
             [revision      (struct-revision guid)]
             [next-revision (if revision (add1 revision) 0)])
        (call-with-transaction
         (lambda ()
           (begin0
             (if (struct-saved? guid)
                 (if (and revision (record-exists-with-revision? entity guid revision))
                     ; Run the update hook:
                     ((entity-on-save entity)
                      (lambda (conn guid)
                        (set-struct-revision! guid next-revision)
                        (send database update-record conn guid)
                        (recache! guid (guid-id guid) #f)
                        guid)
                      (current-connection)
                      guid)
                     (raise-exn exn:fail:snooze:revision "cannot save: structure has been revised since it was loaded from the database" guid))
                 ; Run the insert hook:
                 ((entity-on-save entity)
                  (lambda (conn guid)
                    (set-struct-revision! guid next-revision)
                    (let ([next-id (send database insert-record conn guid)])
                      (recache! guid next-id #f)
                      guid))
                  (current-connection)
                  guid)))))))
    
    ; guid -> guid
    (define/public (delete! guid)
      (auto-connect)
      (let ([entity   (struct-entity guid)]
            [id       (struct-id guid)]
            [revision (struct-revision guid)])
        (begin0
          (if (struct-saved? guid)
              (call-with-transaction
               (lambda ()
                 (if (and revision (record-exists-with-revision? entity guid revision))
                     ((entity-on-delete entity)
                      (lambda (conn guid)
                        (send database delete-record conn guid)
                        (set-struct-revision! guid #f)
                        (send (current-cache) recache! guid #f (gensym))
                        guid)
                      (current-connection)
                      guid)
                     (raise-exn exn:fail:snooze:revision "cannot delete: database has been revised since structure was loaded" guid))))
              (raise-exn exn:fail:snooze (format "cannot delete: struct has not been saved: ~s" guid))))))
    
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
    
    ; guid -> guid
    (define/public (find-by-guid guid)
      (auto-connect)
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
    
    ; entity natural natural guid -> snooze-struct
    ;(define (make-saved-copy entity id revision original)
    ;  
    ;  (apply (entity-private-constructor entity)
    ;         (entity-make-guid #:snooze this entity id #f)
    ;         revision
    ;         (map (lambda (val)
    ;                (if (guid? val)
    ;                    (entity-make-guid #:snooze this (guid-entity val) (guid-id val) #f)
    ;                    val))
    ;              (cddr (snooze-struct-ref* original)))))
    
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
;    [save!                     (-> guid? guid?)]
;    [delete!                   (-> guid? guid?)]
;    
;    [insert/id+revision!       (->* (guid?) ((listof procedure?)) guid?)]
;    [update/id+revision!       (->* (guid?) ((listof procedure?)) guid?)]
;    [delete/id+revision!       (->* (guid?) ((listof procedure?)) guid?)]
;    
;    [find-all                  (-> query? list?)]
;    [find-one                  (-> query? any)]
;    [g:find                    (-> query? procedure?)]
;    
;    [call-with-transaction     (->* (procedure?) ((or/c string? #f)) any)]
;    
;    [find-by-id                (-> entity? (or/c integer? #f) (or/c guid? #f))]
;    [find-by-guid              (-> guid? (or/c guid? #f))]
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
