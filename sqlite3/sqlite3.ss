#lang scheme/base

(require "../base.ss")

(require (prefix-in sqlite: (sqlite-in sqlite))
         (unlib-in gen)
         "../era/core.ss"
         "../era/snooze-struct.ss"
         "../generic/generic.ss"
         "../sql/sql-struct.ss"
         "sql.ss")

; (U path ':memory: ':temp:) -> database%
(define (make-sqlite3-database path)
  (new sqlite3-database%
       [path   path]))

; Database class ---------------------------------

(define sqlite3-database%
  (class* (sqlite3-sql-query-mixin (generic-sql-query-mixin (sqlite3-sql-mixin generic-database%))) (database<%>)
    
    (inspect #f)
    
    (inherit escape-sql-name
             escape-sql-value
             get-snooze
             create-table-sql
             drop-table-sql
             insert-sql
             update-sql
             delete-sql
             direct-find-sql
             query-sql
             parse-value
             make-parser
             make-query-extractor
             make-single-item-extractor
             make-multiple-item-extractor)
    
    ; Fields -------------------------------------
    
    ; (U path string ':memory ':temp:)
    (init-field path)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
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
        (sqlite:exec/ignore (connection-back-end conn) (create-table-sql entity))))
    
    ; connection entity -> void
    (define/public (drop-table conn entity)
      (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not drop table for ~a" entity))
        (sqlite:exec/ignore (connection-back-end conn) (drop-table-sql entity))))
    
    ; connection snooze-struct -> guid
    (define/public (insert-record conn struct)
      (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not insert database record for ~a" struct))
        (sqlite:insert (connection-back-end conn) (insert-sql struct))))
    
    ; connection snooze-struct -> void
    (define/public (update-record conn struct)
      (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not update database record for ~a" struct))
        (sqlite:exec/ignore (connection-back-end conn) (update-sql struct))
        (void)))
    
    ; connection guid -> void
    (define/public (delete-record conn guid)
      (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not delete database record for ~a" guid))
        (sqlite:exec/ignore (connection-back-end conn) (delete-sql guid))
        (void)))
    
    ; (listof vanilla-guid) -> (listof interned-vanilla-guid)
    (define/public (direct-find conn guids)
      (if (null? guids)
          null
          (let ([sql    (direct-find-sql guids)]
                [entity (guid-entity (car guids))])
            (with-snooze-reraise (exn:fail? (format "Could not execute SELECT query:~n~a" sql))
              (g:collect
               (g:map (make-single-item-extractor entity)
                      (g:map (make-parser (map attribute-type (entity-attributes entity)))
                             (g:list (send (connection-back-end conn) map sql list)))))))))
    
    ; snooze<%> connection query -> result-generator
    (define/public (g:find snooze conn query)
      (with-snooze-reraise (sqlite:exn:sqlite? (format "Could not execute SELECT query: ~a" (query-sql query)))
        (let ([results (sqlite:select (connection-back-end conn) (query-sql query))])
          (g:map (make-query-extractor query)
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
      (let* ([sql (format "SELECT name FROM sqlite_master WHERE type='table' AND name=~a;"
                          (cond [(entity? table) (escape-sql-value type:symbol (entity-table-name table))]
                                [(symbol? table) (escape-sql-value type:symbol table)]
                                [else            (raise-type-error 'table-exists? "(U entity symbol)" table)]))]
             [ans (sqlite:select (connection-back-end conn) sql)])
        (not (null? ans))))
    
    ; query output-port string -> query
    ;
    ; Prints an SQL string to stdout as a side effect.
    (define/public (debug-sql query [output-port (current-output-port)] [format "~a"])
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

(provide (struct-out connection)
         sqlite3-database%)

(provide/contract
 [make-sqlite3-database (-> (or/c path? ':memory: ':temp:) (is-a?/c database<%>))])
