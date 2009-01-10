(module postgresql8 mzscheme
  
  (require (only (lib "class.ss") send)
           (lib "kw.ss")
           (lib "pregexp.ss")
           (lib "unitsig.ss")
           (lib "cut.ss" "srfi" "26"))
  
  (require (prefix postgresql: (planet "spgsql.ss" ("schematics" "spgsql.plt" 2)))
           (planet "debug.ss" ("untyped" "unlib.plt" 2))
           (planet "gen.ss" ("untyped" "unlib.plt" 2))
           (planet "profile.ss" ("untyped" "unlib.plt" 2))
           (planet "symbol.ss" ("untyped" "unlib.plt" 2)))
  
  (require ;(prefix postgresql: (file "spgsql-ssl/spgsql.ss"))
           (file "../generic/util.ss")
           (file "../base.ss")
           (file "../db-sig.ss")
           (prefix era: (file "../era.ss"))
           (file "../query-core.ss")
           (file "../transaction.ss")
           (file "../type.ss")
           (file "extract.ss")
           (file "sql.ss"))
  
  (provide config?
           config-server
           config-port
           config-database
           config-username
           config-password
           config-ssl
           config-ssl-encrypt
           (rename create-config make-config)
           db@)
  
  ;; struct config : string integer string string string
  (define-struct config (server port database username password ssl ssl-encrypt))
  
  ;; make-config
  ;;     : string 
  ;;       integer 
  ;;       string
  ;;       string
  ;;       [(U string #f)]
  ;;       #:ssl (U 'yes 'no 'optional)
  ;;       #:ssl-encrypt (U 'sslv2-or-v3 'sslv2 'sslv3 'tls)
  ;;    -> config
  (define create-config
    (lambda/kw (server port database username #:optional [password #f] #:key [ssl 'yes] [ssl-encrypt 'sslv2-or-v3])
      (make-config server port database username password ssl ssl-encrypt)))
  
  ;; db@ : -> db^
  (define db@
    (unit/sig db^
      (import)
      
      ;; connect : sqlite3-config -> connection
      (define (connect config)
        (with-snooze-reraise (exn:fail? "Could not connect to database")
          (let* ([server      (config-server      config)]
                 [port        (config-port        config)]
                 [database    (config-database    config)]
                 [username    (config-username    config)]
                 [password    (config-password    config)]
                 [ssl         (config-ssl         config)]
                 [ssl-encrypt (config-ssl-encrypt config)]
                 [conn        (postgresql:connect #:server      server
                                                  #:port        port
                                                  #:database    database
                                                  #:user        username
                                                  #:password    password
                                                  #:ssl         ssl
                                                  #:ssl-encrypt ssl-encrypt)])
            ; TODO : We need a test for this. Unfortunately, back-end tests are currently not
            ; a unit, so they can't be linked with the current snooze@.
            (send conn exec "SET DATESTYLE TO ISO;")
            conn)))
      
      ;; disconnect : connection -> void
      (define (disconnect conn)
        (with-snooze-reraise (exn:fail? "Could not disconnect from database")
          (send conn disconnect)))
      
      ;; create-table : connection entity -> void
      (define (create-table conn entity)
        (with-snooze-reraise (exn:fail? (format "Could not create table for ~a" entity))
          (for-each (cut send conn exec <>)
                    (map (cut string-append <> ";")
                         (pregexp-split #px";" (create-sql entity))))))
      
      ;; drop-table : connection entity -> void
      (define (drop-table conn entity)
        (with-snooze-reraise (exn:fail? (format "Could not drop table for ~a" entity))
          (for-each (cut send conn exec <>)
                    (map (cut string-append <> ";")
                         (pregexp-split #px";" (drop-sql entity))))))
      
      ;; insert-record : connection persistent-struct -> integer
      ;;
      ;; Inserts a new database record for the struct
      ;; *AND RETURNS ITS ID*.
      (define (insert-record conn struct)
        (with-snooze-reraise (exn:fail? (format "Could not insert database record for ~a" struct))
          (let ([sequence-name (symbol-append (era:entity-name (era:struct-entity struct)) '-seq)])
            ; The two lines below work as follows:
            ;   - the first line inserts the record, using the SEQUENCE "entity-seq" 
            ;     to determine the new ID
            ;   - the second line reads the current value of "entity-seq",
            ;     retrieving the ID of the new struct
            ; Note that there is no transaction around this: the PostgreSQL function "currval" returns
            ; a session-local value, 
            ; The INSERT SQL from insert-sql lets PostgreSQL assign a new ID from the sequence
            ; "entity-seq". The 
            ; Return the new value of the ID sequence (and thus the ID of the just-inserted element):
            (send conn exec (insert-sql struct))
            (unquote-data type:id (send conn query-value (string-append "SELECT currval('" (quote-id sequence-name) "');"))))))
      
      ;; update-record : connection persistent-struct -> void
      (define (update-record conn struct)
        ; Update record
        (with-snooze-reraise (exn:fail? (format "Could not update database record for ~a" struct))
          (send conn exec (update-sql struct)))
        ; Return void
        (void))
      
      ;; delete-record : connection entity integer -> void
      (define (delete-record conn entity id)
        ; Delete record
        (with-snooze-reraise (exn:fail? (format "Could not delete database record for ~a ~a" entity id))
          (send conn exec (delete-sql entity id)))
        ; Return void
        (void))
      
      ;; find-gen : connection select -> result-generator
      (define (find-gen conn select)
        (define sql (select-sql select))
        (with-snooze-reraise (exn:fail? (format "Could not execute SELECT query:~n~a" (select-sql select)))
          (g:map (make-struct-extractor (select-what-entities select) (select-single-item? select))
                 (g:map (make-data-unquoter (select-what-types select))
                        (list->generator (send conn map sql vector))))))
      
      ;; parameter savepoints : (list-of symbol)
      (define current-savepoints (make-parameter null))
      
      ;; call-with-transaction : connection thunk -> any
      ;;
      ;; A transaction is started and the thunk argument is
      ;; called. If the thunk is allowed to finish gracefully,
      ;; the transaction is committed. If, however, execution is
      ;; terminated via an exception or escape continuation,
      ;; the transaction is rolled back.
      (define (call-with-transaction conn body)
        (let* ([frame                     (make-frame)]
               [savepoint                 (gensym 'save)]
               [old-savepoints            (current-savepoints)]
               [quoted-savepoint          (quote-id savepoint)]
               [body-complete-cell        (make-thread-cell #f)]  ; Has the body procedure been completed successfully?
               [transaction-complete-cell (make-thread-cell #f)]) ; Has the dynamic extent of the program left the transaction block?
          (parameterize ([current-savepoints (cons savepoint old-savepoints)])
            ; Start the transaction by setting a savepoint:
            (dynamic-wind
             ; DYNAMIC ENTRY: Check we haven't tried to suspend/resume the transaction
             (lambda ()
               (if (thread-cell-ref transaction-complete-cell)
                   (raise-exn exn:fail:snooze:transaction
                     "Transaction block was interrupted and cannot be resumed.")
                   (begin
                     ; If this is the outermost call to call-with-transaction, start an SQL TRANSACTION
                     (when (null? old-savepoints)
                       (send conn exec "BEGIN;"))
                     ; The actual COMMIT / ROLLBACK process is governed by SQL SAVEPOINTS
                     (send conn exec (string-append "SAVEPOINT " quoted-savepoint ";")))))
             ; DYNAMIC BODY: Run the body code: indicate success on completion
             (lambda ()
               (call-with-frame frame
                 (lambda ()
                   (begin0 (body)
                           (thread-cell-set! body-complete-cell #t)))))
             ; DYNAMIC EXIT: Commit or rollback the transaction
             (lambda ()
               (if (thread-cell-ref transaction-complete-cell)
                   ; The raise statement in DYNAMIC ENTRY should prevent this situation:
                   ; this branch of code should never get run.
                   (raise-exn exn:fail:snooze:transaction
                     "Transaction block was interrupted and resumed.")
                   (begin
                     ; COMMIT or ROLLBACK the savepoint
                     (if (thread-cell-ref body-complete-cell)
                         (send conn exec (string-append "RELEASE SAVEPOINT " quoted-savepoint ";"))
                         (begin
                           (send conn exec (string-append "ROLLBACK TO SAVEPOINT " quoted-savepoint ";"))
                           (roll-back-frame! frame)))
                     (thread-cell-set! transaction-complete-cell #t)
                     ; If this is the outermost call to call-with-transaction, exit the TRANSACTION block
                     (when (null? old-savepoints)
                       (send conn exec "COMMIT;")))))))))
      
      ;; dump-sql : select output-port string -> select
      ;;
      ;; Prints an SQL string to stdout as a side effect.
      (define (dump-sql select output-port format)
        (fprintf output-port format (select-sql select))
        select)
      
      ))
  
  )
 
