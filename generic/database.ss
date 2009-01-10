#lang scheme/base

(require scheme/class)

(provide database<%>)

; DBMS-specific parts of Snooze.
(define database<%>
  (interface ()
    ; -> connection
    connect
    
    ; connection -> void
    disconnect
    
    ; entity -> void
    ;
    ; Creates a table for the given entity. Raises exn:fail:snooze if the table
    ; already exists.
    create-table
    
    ; entity -> void
    ;
    ; Drops the table for the given entity. Does nothing if the table does not
    ; exist.
    drop-table
    
    ; ***** NOTE *****
    ; The xxx-record functions are called from the main Snooze interface code.
    ; The main Snooze functions in snooze-unit.ss handle the calling of
    ; pipelines and the updating of the structures: these functions 
    ; *DO NOT DO THESE THINGS*.
    ; ****************
    
    ; connection persistent-struct -> integer
    ;
    ; Inserts a new database record for the structure and returns the new id,
    ; but does not store the id in the struct.
    insert-record
    
    ; connection persistent-struct -> void
    ;
    ; Inserts a new database record for the structure using the ID present in
    ; the structure.
    insert-record/id
    
    ; connection persistent-struct -> void
    ;
    ; Updates the database record for the structure.
    update-record
    
    ; connection guid -> void
    ;
    ; Deletes the database record for the given guid.
    delete-record
    
    ; select -> (gen-> result)
    ;
    ; Queries the database and returns a generator that parses the results
    ; and extracts persistent-structs as necessary before returning rows.
    g:find
    
    ; conn -> boolean
    ;
    ; Determines a transaction can be started. Some databases do not allow
    ; nested transactions, so the return value may be context dependent.
    transaction-allowed?
    
    ; thunk -> any
    ;
    ; A transaction is started and the thunk argument is called. If the thunk
    ; is allowed to finish gracefully, the transaction is committed. If, however, 
    ; execution is terminated via an exception or escape continuation, the transaction
    ; is rolled back.
    call-with-transaction
    
    ; select [output-port] [string] -> select
    ;
    ; where the arguments are:
    ;     select      : the select structure
    ;     output-port : the output port to print to
    ;     format      : a format string with one "~a" or "~s" in it
    ; 
    ; Prints an SQL string to stdout as a side effect.
    dump-sql
    
    ; -> (listof symbol)
    ;
    ; Returns an alphabetically ordered list of the table names in the current database.
    table-names
    
    ; (U symbol entity) -> boolean
    ;
    ; Determines whether the supplied table (or table name) exists in the database.
    table-exists?))
