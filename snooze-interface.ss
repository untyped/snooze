#lang scheme/base

(require scheme/class)

(provide snooze<%>)

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
    ;
    ; Creates a table for the supplied entity. Raises exn:fail:snooze if the table already exists.
    create-table
    
    ; entity -> void
    ;
    ; Drops the supplied table (or table name). Does nothing if the table does not exist.
    drop-table
    
    ; persistent-struct -> persistent-struct
    ;
    ; Saves a struct to the database.
    ;
    ; If this is the first time the struct has been saved:
    ;   - the struct's revision is set to 0;
    ;   - the save pipeline is run;
    ;   - the insert pipeline is run;
    ;   - an ID is allocated;
    ;   - the struct is returned with the new ID and revision in place.
    ;
    ; If the struct is already in the database:
    ;   - the struct's revision is checked against the revision in the database;
    ;   - the revision is incremented;
    ;   - the save pipeline is run;
    ;   - the update pipeline is run;
    ;   - the struct is returned with the new revision in place.
    save!
    
    ; persistent-struct -> persistent-struct
    delete!
    
    ; persistent-struct [(listof stage)] -> persistent-struct
    ;
    ; Used to specifically insert a persistent-struct with a particular ID and revision.
    ; Does not check or update the revision number of the record. Does not run the default
    ; pipelines, although a pipeline may be specified as the optional second argument.
    ; 
    ; Only use this method if you want to bypass the default behaviour of allocating and
    ; checking IDs and revision numbers.
    insert/id+revision!
    
    ; persistent-struct [(listof stage)] -> persistent-struct
    ;
    ; Used to specifically update a persistent-struct with a particular ID and revision.
    ; Does not check or update the revision number of the record. Does not run the default
    ; pipelines, although a pipeline may be specified as the optional second argument.
    ; 
    ; Only use this method if you want to bypass the default behaviour of checking IDs and
    ; revision numbers.
    update/id+revision!
    
    ; persistent-struct [(listof stage)] -> persistent-struct
    ;
    ; Used to specifically delete a persistent-struct with a particular ID and revision.
    ; Does not check or update the revision number of the record. Does not run the default
    ; pipelines, although a pipeline may be specified as the optional second argument.
    ; 
    ; Only use this method if you want to bypass the default behaviour of checking IDs and
    ; revision numbers.
    delete/id+revision!
    
    ; select -> (U result #f)
    find-one
    
    ; select -> (listof result)
    find-all
    
    ; select -> (gen-> result)
    g:find
    
    ; entity (U integer #f) -> (U persistent-struct #f)
    find-by-id
    
    ; guid -> (U persistent-struct #f)
    find-by-guid
    
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
    dump-sql
    
    ; -> (listof symbol)
    ;
    ; Returns an alphabetically ordered list of the table names in the database.
    table-names
    
    ; (U entity symbol) -> boolean
    ;
    ; Determines whether the supplied table (or table name) exists in the database.
    table-exists?))

(provide snooze<%>)
