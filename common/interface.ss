#lang scheme/base

(require "../base.ss")

(require scheme/class)

(define generic-database<%>
  (interface ()
    ; -> snooze
    get-snooze))

(define database<%>
  (interface (generic-database<%>)
    ; -> connection
    connect
    
    ; connection -> void
    disconnect
    
    ; entity -> void
    ; Creates a table for the given entity. Raises exn:fail:snooze if the table already exists.
    create-table
    
    ; entity -> void
    ; Drops the table for the given entity. Does nothing if the table does not exist.
    drop-table
    
    ; ***** NOTE *****
    ; The xxx-record functions are called from the main Snooze interface code.
    ; The main Snooze functions in snooze-unit.ss call hooks and update 
    ; snooze-structs with new guids and revisions.
    ; 
    ; These functions do not do these things.
    ; ****************
    
    ; connection snooze-struct -> snooze-struct
    ; Inserts a new database record for the structure and returns the new id.
    insert-struct
    
    ; connection snooze-struct -> snooze-struct
    ; Updates the database record for the structure.
    update-struct
    
    ; connection guid -> snooze-struct
    ; Deletes the database record for the given guid.
    delete-struct
    
    ; connetion guid -> guid
    ; Deletes the database record for the given guid.
    ; delete-guid
    
    ; connection (listof database-guid) -> (gen-> snooze-struct)
    ; Selects a number of similarly typed structs by guid.
    direct-find
    
    ; connection query -> (gen-> result)
    ; Queries the database and returns a generator that parses the results
    ; and extracts data as necessary before returning results.
    g:find
    
    ; -> boolean
    ; Can this DBMS perform and rollback nested transactions?
    supports-nested-transactions?
    
    ; connection -> boolean
    ; Determines a transaction can be started. Some databases do not allow
    ; nested transactions, so the return value may be context dependent.
    transaction-allowed?
    
    ; thunk -> any
    ; A transaction is started and the thunk argument is called. If the thunk
    ; is allowed to finish gracefully, the transaction is committed. If, however, 
    ; execution is terminated via an exception or escape continuation, the transaction
    ; is rolled back.
    call-with-transaction
    
    ; select [string] -> select
    ; where the arguments are:
    ;     select      : the select structure
    ;     output-port : the output port to print to
    ;     format      : a format string with one "~a" or "~s" in it
    ; Prints an SQL string to stdout as a side effect.
    debug-sql
    
    ; -> (listof symbol)
    ; Returns an alphabetically ordered list of the table names in the current database.
    table-names
    
    ; (U symbol entity) -> boolean
    ; Determines whether the supplied table (or table name) exists in the database.
    table-exists?))

(define sql-escape<%>
  (interface ()
    ; symbol -> string
    ; Escapes an SQL identifier. For example:
    ;   id -> "id" in PostgreSQL
    ;   id -> [id] in SQLite
    ;   id -> `id` in MySQL
    escape-sql-name
    ; type any -> string
    escape-sql-value))

(define sql-create<%>
  (interface ()
    ; entity -> string
    create-table-sql))

(define sql-drop<%>
  (interface ()
    ; entity -> string
    drop-table-sql))

(define sql-insert<%>
  (interface ()
    ; snooze-struct -> string
    insert-sql))

(define sql-update<%>
  (interface ()
    ; snooze-struct -> string
    update-sql))

(define sql-delete<%>
  (interface ()
    ; guid -> string
    delete-sql))

(define sql-query<%>
  (interface ()
    
    ; (listof database-guid) -> string
    direct-find-sql
    
    ; query -> string
    query-sql
    
    ; query output-port -> void
    display-query
    
    ; (U expression boolean) (listof column) output-port -> void
    display-distinct
    
    ; (listof column) (listof column) output-port -> void
    display-what
    
    ; source (listof column) output-port -> void
    display-from
    
    ; (listof column) (listof column) output-port -> void
    display-group
    
    ; (listof order) (listof column) output-port -> void
    display-order
    
    ; expression (listof column) output-port -> void
    display-expression))

(define parse<%>
  (interface ()
    ; type any -> any
    parse-value
    
    ; (listof type) -> ((U (listof db-value) #f) -> (U (listof snooze-attr-value) #f))
    make-parser))

(define extract<%>
  (interface ()
    ; query -> (U single-item-extractor multi-item-extractor)
    make-query-extractor
    
    ; (U entity #f) -> single-item-extractor
    ; where single-item-extractor
    ;         : (U (listof scheme-primitive) #f)
    ;           transaction-frame
    ;          ->
    ;           (U snooze-struct scheme-primitive)
    ; Exposed for direct-find and for debugging.
    make-single-item-extractor
    
    ; (listof (U entity #f)) -> multiple-item-extractor
    ; where multiple-item-extractor
    ;         : (U (listof scheme-primitive) #f)
    ;           transaction-frame
    ;          ->
    ;          (listof (U snooze-struct scheme-primitive))
    ; Exposed for debugging.
    make-multiple-item-extractor))

(define cross-reference<%>
  (interface ()
    ; query -> cross-referencer
    make-query-cross-referencer))

; Provide statements -----------------------------

(provide generic-database<%>
         database<%>
         sql-escape<%>
         sql-create<%>
         sql-drop<%>
         sql-insert<%>
         sql-update<%>
         sql-delete<%>
         sql-query<%>
         parse<%>
         extract<%>
         cross-reference<%>)
