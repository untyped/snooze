#lang scheme/base

(require scheme/class)

(require (planet untyped/unlib:3/gen)
         (prefix-in sqlite: (planet jaymccarthy/sqlite:4:2/sqlite))
         "../base.ss"
         "../extract.ss"
         "../era/era.ss"
         "../generic/connection.ss"
         "../generic/database.ss"
         "../generic/snooze-reraise.ss"
         "../sql/sql-struct.ss"
         "sql.ss")

(define database%
  (class* object% (database<%>)
    
    ; Constructor --------------------
    
    ; (U path string)
    (init-field path)
    
    (super-new)
    
    ; Methods ------------------------
    
    ; -> connection
    (define/public (connect)
      (with-snooze-reraise (sqlite:exn:sqlite? "Could not connect to database")
        (make-connection (sqlite:open path) #f)))
    
    ; connection -> void
    (define/public (disconnect conn)
      (with-snooze-reraise (sqlite:exn:sqlite? "Could not disconnect from database")
        (sqlite:close (connection-back-end conn))))
    
    ; connection entity -> void
    (define/public (create-table conn entity)
      (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not create table for ~a" entity))
        (sqlite:exec/ignore (connection-back-end conn) (create-sql entity))))
    
    ; connection entity -> void
    (define/public (drop-table conn entity)
      (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not drop table for ~a" entity))
        (sqlite:exec/ignore (connection-back-end conn) (drop-sql entity))))
    
    ; connection persistent-struct -> integer
    ;
    ; Inserts a new database record for the struct and returns its ID.
    (define/public (insert-record conn struct)
      (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not insert database record for ~a" struct))
        (sqlite:insert (connection-back-end conn) (insert-sql struct))))
    
    ; connection persistent-struct -> void
    ;
    ; Inserts a new database record for the struct and returns its ID.
    (define/public (insert-record/id conn struct)
      (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not insert database record for ~a" struct))
        (sqlite:insert (connection-back-end conn) (insert-sql struct #t))
        (void)))
    
    ; connection persistent-struct -> void
    (define/public (update-record conn struct)
      (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not update database record for ~a" struct))
        (sqlite:exec/ignore (connection-back-end conn) (update-sql struct))
        (void)))
    
    ; connection guid -> void
    (define/public (delete-record conn guid)
      (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not delete database record for ~a" guid))
        (sqlite:exec/ignore (connection-back-end conn) (delete-sql guid))
        (void)))
    
    ; connection query -> result-generator
    (define/public (g:find conn query)
      (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not execute SELECT query: ~a" (query-sql query)))
        (let ([results (sqlite:select (connection-back-end conn) (query-sql query))])
          (g:map (make-struct-extractor (query-extract-info query))
                 (g:map (make-parser (map expression-type (query-what query)))
                        (g:list (remove-column-names results)))))))
    
    ; connection -> boolean
    (define/public (transaction-allowed? conn)
      (not (connection-in-transaction? conn)))
    
    ; connection thunk -> any
    ;
    ; A transaction is started and the thunk argument is
    ; called. If the thunk is allowed to finish gracefully,
    ; the transaction is committed. If, however, execution is
    ; terminated via an exception or escape continuation,
    ; the transaction is rolled back.
    (define/public (call-with-transaction conn body)
      (when (connection-in-transaction? conn)
        (raise-exn exn:fail:snooze 
          (string-append "Connection is already in a transaction. "
                         "This should have been prevented by a call to transaction-allowed?")))
      (dynamic-wind
       (lambda ()
         (set-connection-in-transaction?! conn #t))
       (lambda ()
         (sqlite:with-transaction ((connection-back-end conn) sqlite-escape)
           (body)))
       (lambda ()
         (set-connection-in-transaction?! conn #f))))
    
    ; connection -> (listof symbol)
    (define/public (table-names conn)
      (define result (sqlite:select (connection-back-end conn) "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"))
      (map (lambda (src)
             (parse-value type:symbol (vector-ref src 0)))
           (remove-column-names result)))
    
    ; connection (U symbol entity) -> boolean
    (define/public (table-exists? conn table)
      (define sql
        (format "SELECT name FROM sqlite_master WHERE type='table' AND name=~a;"
                (cond [(entity? table) (escape-value type:symbol (entity-table-name table))]
                      [(symbol? table) (escape-value type:symbol table)]
                      [else            (raise-exn exn:fail:snooze
                                         (format "Expected (U entity symbol), recevied ~s" table))])))
      (define result (sqlite:select (connection-back-end conn) sql))
      (not (null? result)))
    
    ; query output-port string -> query
    ;
    ; Prints an SQL string to stdout as a side effect.
    (define/public (dump-sql query [output-port (current-output-port)] [format "~a"])
      (fprintf output-port format (query-sql query))
      query)))

; Helpers ----------------------------------------

; (listof vector) -> (listof vector)
;
; The SQLite library returns (cons column-names results) if there are results to be returned,
; and null if there aren't. This procedure chops off the column-names.
(define (remove-column-names results)
  (if (null? results)
      null
      (cdr results)))

; Provide statements -----------------------------

(provide database%)
