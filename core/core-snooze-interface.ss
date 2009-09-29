#lang scheme/base

(require "../base.ss")

(define snooze<%>
  (interface ()
    
    ; (-> any) -> any
    call-with-connection
    
    ; -> void
    connect
    
    ; -> void
    disconnect
    
    ; -> connection
    current-connection
    
    ; entity -> void
    ; Creates a table for the supplied entity. Raises exn:fail:snooze if the table already exists.
    create-table
    
    ; entity -> void
    ; Drops the supplied table (or table name). Does nothing if the table does not exist.
    drop-table
    
    ; guid -> guid
    ; Saves a struct to the database.
    save!
    
    ; guid -> guid
    ; Deletes a struct from the database.
    delete!
    
    ; select -> (U result #f)
    find-one
    
    ; select -> (listof result)
    find-all
    
    ; select -> (gen-> result)
    g:find
    
    ; (-> ans) any ... -> ans
    ;
    ; A transaction is started and the thunk argument is
    ; called. If the thunk is allowed to finish gracefully,
    ; the transaction is committed. If, however, execution is
    ; terminated via an exception or escape continuation,
    ; the transaction is rolled back.
    call-with-transaction
    
    ; query -> string
    query->string
    
    ;  select
    ;  [#:output-port output-port]
    ;  [#:format string]
    ; ->
    ;  select
    ;
    ; Prints an SQL string to stdout as a side effect.
    debug-sql
    
    ; -> (listof symbol)
    ;
    ; Returns an alphabetically ordered list of the table names in the database.
    table-names
    
    ; (U entity symbol) -> boolean
    ;
    ; Determines whether the supplied table (or table name) exists in the database.
    table-exists?))

; Provide statements -----------------------------

(provide/contract
 [snooze<%> interface?])
