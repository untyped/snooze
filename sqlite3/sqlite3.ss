(module sqlite3 mzscheme
  
  (require (lib "unitsig.ss"))
  
  (require (planet "gen.ss" ("untyped" "unlib.plt" 2))
           (planet "parameter.ss" ("untyped" "unlib.plt" 2)))
  
  (require (prefix sqlite: (planet "sqlite.ss" ("jaymccarthy" "sqlite.plt" 3))))
  
  (require (file "../generic/util.ss")
           (file "../base.ss")
           (file "../db-sig.ss")
           (prefix era: (file "../era.ss"))
           (file "../query-core.ss")
           (file "../transaction.ss")
           (file "../type.ss")
           (file "extract.ss")
           (file "sql.ss"))
  
  (provide (struct config (path))
           db@)
  
  ;; struct config : path
  (define-struct config (path))
  
  ;; db@ : -> db^
  (define db@
    (unit/sig db^
      (import)
      
      ;; connect : sqlite3-config -> connection
      (define (connect config)
        (with-snooze-reraise (sqlite:exn:sqlite? "Could not connect to database")
          (sqlite:open (config-path config))))
      
      ;; disconnect : connection -> void
      (define (disconnect conn)
        (with-snooze-reraise (sqlite:exn:sqlite? "Could not disconnect to database")
          (sqlite:close conn)))
      
      ;; create-table : connection entity -> void
      (define (create-table conn entity)
        (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not create table for ~a" entity))
          (sqlite:exec/ignore conn (create-sql entity))))
      
      ;; drop-table : connection entity -> void
      (define (drop-table conn entity)
        (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not drop table for ~a" entity))
          (sqlite:exec/ignore conn (drop-sql entity))))
      
      ;; insert-record : connection persistent-struct -> integer
      ;;
      ;; Inserts a new database record for the struct
      ;; *AND RETURNS ITS ID*.
      (define (insert-record conn struct)
        ; The call to sqlite:insert returns the id of the new record:
        (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not insert database record for ~a" struct))
          (sqlite:insert conn (insert-sql struct))))
      
      ;; update-record : connection persistent-struct -> void
      (define (update-record conn struct)
        ; Update record
        (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not update database record for ~a" struct))
          (sqlite:exec/ignore conn (update-sql struct)))
        ; Return void
        (void))
      
      ;; delete-record : connection entity integer -> void
      (define (delete-record conn entity id)
        ; Delete record
        (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not delete database record for ~a ~a" entity id))
          (sqlite:exec/ignore conn (delete-sql entity id)))
        ; Return void
        (void))
      
      ;; find-gen : connection select -> result-generator
      (define (find-gen conn select)
        (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not execute SELECT query: ~a" select))
          (let ([results (sqlite:select conn (select-sql select))])
            (g:map (make-struct-extractor (select-what-entities select) (select-single-item? select))
                   (g:map (make-data-unquoter (select-what-types select))
                          (list->generator (if (null? results) null (cdr results))))))))
      
      ;; call-with-transaction : connection thunk -> any
      ;;
      ;; A transaction is started and the thunk argument is
      ;; called. If the thunk is allowed to finish gracefully,
      ;; the transaction is committed. If, however, execution is
      ;; terminated via an exception or escape continuation,
      ;; the transaction is rolled back.
      (define (call-with-transaction conn body)
        ; SQLite cannot handle nested transactions, so only the outermost call to
        ; call-with-transaction actually starts a transaction.
        (if (in-transaction?)
            (body)
            (sqlite:with-transaction
             (conn sqlite-escape)
             (let ([frame                     (make-frame)]
                   [body-complete-cell        (make-thread-cell #f)]  ; Has the body procedure been completed successfully?
                   [transaction-complete-cell (make-thread-cell #f)]) ; Has the dynamic extent of the program left the transaction block?
               (dynamic-wind
                ; DYNAMIC ENTRY: Check we haven't tried to suspend/resume the transaction
                (lambda ()
                  (when (thread-cell-ref transaction-complete-cell)
                    (raise-exn exn:fail:snooze:transaction
                      "Transaction block was interrupted and cannot be resumed.")))
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
                        (unless (thread-cell-ref body-complete-cell)
                          (roll-back-frame! frame))
                        (thread-cell-set! transaction-complete-cell #t)))))))))
      
      ;; dump-sql : select output-port string -> select
      ;;
      ;; Prints an SQL string to stdout as a side effect.
      (define (dump-sql select output-port format)
        (fprintf output-port format (select-sql select))
        select)
      
      ))
  
  )