(module snooze-sig mzscheme
  
  (require (lib "unitsig.ss"))
  
  (require (file "base.ss"))
  
  (provide (all-defined))
  
  ;; Signature for the front-end Snooze API.
  ;;
  ;; ***** NOTE *****
  ;; Some functions have an optional connection argument. If this argument is not provided,
  ;; the functions default to a parameterized connection (that can be set up with the
  ;; (call-with-database ...) form or the equivalent (with-database ...) syntax from snooze.ss).
  ;;
  ;; Note also that the "config" and "connection" types are not concrete types: they refer 
  ;; to backend-specific types exported by the relevant implementation of db^ (see db-sig.ss).
  ;; ****************
  (define-signature snooze^
    (connect            ; config -> connection
     disconnect         ; connection -> void
     
     call-with-database ; config (-> any) -> any
     current-connection ; -> connection
     
     create-table       ; [connection] entity -> void
     drop-table         ; [connection] entity -> void
     
     save!              ; [connection] persistent-struct -> integer
     delete!            ; [connection] persistent-struct -> void
     
     find-gen           ; [connection] select -> (gen-> result)
     find-all           ; [connection] select -> (list-of result)
     find-one           ; [connection] select -> (U result #f)
     
     g:find             ; [connection] select -> (gen-> result) ; alias of find-gen

     find-by-id         ; [connection] entity (U integer #f) -> (U persistent-struct #f)
     
     ;; call-with-transaction : [connection] thunk -> any
     ;;
     ;; A transaction is started and the thunk argument is
     ;; called. If the thunk is allowed to finish gracefully,
     ;; the transaction is committed. If, however, execution is
     ;; terminated via an exception or escape continuation,
     ;; the transaction is rolled back.
     call-with-transaction
     
     ;; dump-sql : select
     ;;            [#:output-port output-port]
     ;;            [#:format string]
     ;;         -> select
     ;;
     ;; Prints an SQL string to stdout as a side effect.
     dump-sql))
  
  )
 