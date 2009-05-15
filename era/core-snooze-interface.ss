#lang scheme/base

(require "../base.ss")

(define snooze-cache<%>
  (interface ()
    
    ; guid -> (U snooze-struct #f)
    ;
    ; Retrieves a struct from the cache. Implementations:
    ;   - database - load by ID;
    ;   - cache    - if hit, return;
    ;              - if miss, propagate to parent, clone, store, intern entity/id and return.
    cache-ref
    
    ; snooze-struct -> guid
    ;
    ; Adds a new struct to the cache. Implementations:
    ;   - database - do nothing;
    ;   - cache    - store.
    cache-add!
    
    ; guid -> guid
    cache-remove!))

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
    
    ; guid -> guid
    ;
    ; Saves a struct to the database. Implementations:
    ;   - database - do revision check, run hook, perform update/insert, mutate guid with new ID;
    ;   - cache    - already stored locally - add a clone to parent, propagate, reintern entity/id.
    save!
    
    ; guid -> guid
    ;
    ; Deletes a struct from the database. Implementations:
    ;   - database - do revision check, run hook, perform delete, mutate guid with #f ID;
    ;   - cache    - propagate, unintern entity/id.
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
