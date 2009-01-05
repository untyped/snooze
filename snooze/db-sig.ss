(module db-sig mzscheme
  
  (require (lib "unitsig.ss"))
  
  (require (file "base.ss"))
  
  ; Units and signatures -------------------------
  
  ; Interface to a database-specific backend. Units of this
  ; signature are imported by snooze@ in snooze-unit.ss, to create a
  ; Snooze front-end.
  ;
  ; ***** NOTE *****
  ; The type "connection" referred to in the comments below is not
  ; a concrete type. Connections are implemented differently by each backend.
  ; The only restriction is as follows:
  ;   - the connect function must create a connection object of a specific type
  ;   - all other functions must accept an object of this same type
  ; ****************
  
  (define-signature db^
    (;db-type              ; -> string
     
     connect              ; -> connection
     disconnect           ; connection -> void
     
     create-table         ; connection entity -> void
     drop-table           ; connection entity -> void
     
     ; ***** NOTE *****
     ; The xxx-record functions are called from the main Snooze interface code.
     ; The main Snooze functions in snooze-unit.ss handle the calling of
     ; pipelines and the updating of the structures: these functions 
     ; *DO NOT DO THESE THINGS*.
     ; ****************
     
     ;; insert-record : connection persistent-struct -> integer 
     ;;
     ;; Inserts a new database record for the structure
     ;; and returns the new id (but does not store the id 
     ;; in the struct).
     insert-record
     
     ;; update-record : connection persistent-struct -> void 
     ;;
     ;; Updates the database record for the structure.
     update-record
     
     ;; delete-record : connection entity integer -> void
     ;;
     ;; Deletes the database record for the given entity.
     ;; and primary key.
     delete-record
     
     find-gen        ; connection select -> result-generator
     
     ; where v200-result      : (U persistent-struct (cons persistent-struct (list-of v200-result)))
     ;       result-generator : (-> (U result #f))
     ;       result           : (list-of (U persistent-struct #f))
     
     ;; call-with-transaction : connection thunk -> any
     ;;
     ;; A transaction is started and the thunk argument is
     ;; called. If the thunk is allowed to finish gracefully,
     ;; the transaction is committed. If, however, execution is
     ;; terminated via an exception or escape continuation,
     ;; the transaction is rolled back.
     call-with-transaction
     
     ;; dump-sql : connection select output-port string -> select
     ;;
     ;; where the arguments are:
     ;;     connection  : the database connection
     ;;     select      : the select structure
     ;;     output-port : the output port to print to
     ;;     format      : a format string with one "~a" or "~s" in it
     ;; 
     ;; Prints an SQL string to stdout as a side effect.
     dump-sql))

  ; Provide statements --------------------------- 
  
  (provide db^)
  
  )
 