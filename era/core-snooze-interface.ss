#lang scheme/base

(require "../base.ss")

(define snooze-cache<%>
  (interface ()
    
    ; -> (U snooze-cache<%> #f)
    ; Returns the parent of this cache.
    get-parent
    
    ; local-guid -> snooze-struct
    ; Dereferences a local guid.
    cache-ref/local
    
    ; local-guid -> (U snooze-struct #f)
    ; Dereferences a vanilla guid, promoting it to the current cache if necessary.
    ; Returns the mapped struct, or #f if the guid was not found in any cache.
    cache-ref/vanilla
    
    ; snooze-struct -> local-guid
    ; Adds a struct to the cache and returns a new local guid that points to it:
    ;   - if the struct contains an id, it is cached by vanilla and local guid;
    ;   - if the struct's id is #f, it is cached by local guid only.
    add-struct!
    
    ; vanilla-guid -> (U local-guid #f)
    ; Searches for vanilla-guid in this cache and its ancestors,
    ; syncing the caches as it goes. Returns a local guid pointing to the same struct,
    ; or #f if the struct is not in the cache.
    get-local-alias))

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
