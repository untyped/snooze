#lang scheme/base

(require "../base.ss")

(define snooze-cache<%>
  (interface ()
    
    ; -> #s(snooze ...)
    get-serializable-cache-address
    
    ; thunk -> any
    ;
    ; Pushes a new frame onto the cache stack and calls thunk in
    ; the context of that frame.
    call-with-cache-frame
    
    ; guid [frame] -> snooze-struct
    ;
    ; Retrieves a struct from the current cache frame.
    ; If the struct is not found, parent cache frames and the database
    ; are queried and values are copied into the relevant frames.
    cache-ref
    
    ; guid snooze-struct [frame] -> guid
    ;
    ; Sets a guid/struct mapping in the current cache frame.
    cache-set!
    
    ; entity integer -> guid
    get-interned-guid
    
    ; snooze-struct [frame] -> guid
    ;
    ; Adds a new struct to the current cache frame.
    ; Raises exn:fail if the guid is already in the frame.
    cache-add!))

(define snooze<%>
  (interface (snooze-cache<%>)
    
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
    
    ; snooze-struct -> snooze-struct
    ;
    ; Saves a struct to the database.
    ;
    ; If this is the first time the struct has been saved:
    ;   - the struct's revision is set to 0;
    ;   - the save hook is run;
    ;   - an ID is allocated;
    ;   - the struct is returned with the new ID and revision in place.
    ;
    ; If the struct is already in the database:
    ;   - the struct's revision is checked against the revision in the database;
    ;   - the revision is incremented;
    ;   - the save hook is run;
    ;   - the struct is returned with the new revision in place.
    save!
    
    ; snooze-struct -> snooze-struct
    delete!
    
    ; snooze-struct [(listof stage)] -> snooze-struct
    ;
    ; Used to specifically insert a snooze-struct with a particular ID and revision.
    ; Does not check or update the revision number of the record. Does not run the default
    ; hooks, although a pipeline may be specified as the optional second argument.
    ; 
    ; Only use this method if you want to bypass the default behaviour of allocating and
    ; checking IDs and revision numbers.
    insert/id+revision!
    
    ; snooze-struct [(listof stage)] -> snooze-struct
    ;
    ; Used to specifically update a snooze-struct with a particular ID and revision.
    ; Does not check or update the revision number of the record. Does not run the default
    ; pipelines, although a pipeline may be specified as the optional second argument.
    ; 
    ; Only use this method if you want to bypass the default behaviour of checking IDs and
    ; revision numbers.
    update/id+revision!
    
    ; snooze-struct [(listof stage)] -> snooze-struct
    ;
    ; Used to specifically delete a snooze-struct with a particular ID and revision.
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
    
    ; entity (U integer #f) -> (U snooze-struct #f)
    ; find-by-id
    
    ; guid -> (U snooze-struct #f)
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
 [snooze-cache<%> interface?]
 [snooze<%>       interface?])
