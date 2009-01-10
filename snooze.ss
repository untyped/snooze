(module snooze mzscheme
  
  (require (lib "unitsig.ss"))
  
  (require (planet "gen.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "base.ss")
           (file "db-sig.ss")
           (file "era.ss")
           (file "persistent-struct.ss")
           (prefix q: (file "query-lang.ss"))
           (file "query-util.ss")
           (file "snooze-sig.ss")
           (file "snooze-unit.ss")
           (file "type.ss"))
  
  (provide (all-from (planet "gen.ss" ("untyped" "unlib.plt" 2))))
  
  (provide (all-from (file "base.ss"))
           (all-from (file "era.ss"))
           (all-from (file "persistent-struct.ss"))
           (all-from (file "query-lang.ss"))
           (all-from (file "query-util.ss"))
           (all-from (file "type.ss")))
           
  (provide (all-defined))
  
  ;; syntax define-snooze-interface
  ;;
  ;; Defines the functions from snooze^, given an appropriate backend.
  ;; See snooze-sig.ss for a list of the functions in the interface.
  ;;
  ;; Example usage at the end of this file.
  (define-syntax (define-snooze-interface main-stx)
    (syntax-case main-stx ()
      [(define-snooze-interface db@)
        (syntax-local-introduce
         #`(begin
             
             ; The main snooze^ interface:
             (define-values/invoke-unit/sig snooze^
               (compound-unit/sig
                 (import)
                 (link (db : db^ (db@))
                       (snooze : snooze^ (snooze@ db)))
                 (export (open snooze))))
             
             ;; syntax (with-database (config) expr ...)
             ;;
             ;; Convenience form for call-with-database.
             ;;
             ;; NOTE: There's bit of hocus pocus here suggested by Jens Axel Søgaard, to get call-with-database
             ;; as it is defined in the module containing (define-snooze-interface ...):
             ;;
             ;;     http://list.cs.brown.edu/pipermail/plt-scheme/2006-October/014996.html
             (define-syntax (with-database stx)
               (syntax-case stx ()
                 [(_ config expr (... ...))
                  #'(let ([call/database #,(syntax-local-get-shadower #'call-with-database)])
                      (call/database config (lambda () expr (... ...))))]))
             
             ;; syntax (with-transaction () expr ...)
             ;;        (with-transaction (connection) expr ...)
             ;;
             ;; Convenience form for call-with-transaction.
             ;;
             ;; NOTE: There's bit of hocus pocus here suggested by Jens Axel Søgaard, to get call-with-transaction
             ;; as it is defined in the module containing (define-snooze-interface ...):
             ;;
             ;;     http://list.cs.brown.edu/pipermail/plt-scheme/2006-October/014996.html
             (define-syntax (with-transaction stx)
               (syntax-case stx ()
                 [(_ () expr (... ...))
                  #'(let ([call/transaction #,(syntax-local-get-shadower #'call-with-transaction)])
                      (call/transaction (lambda () expr (... ...))))]
                 [(_ (conn) expr (... ...))
                  #'(let ([call/transaction #,(syntax-local-get-shadower #'call-with-transaction)])
                      (call/transaction conn (lambda () expr (... ...))))]))
             
             ))]))
  
  #| Example SQLite usage:

  (require (planet "snooze.ss" ("untyped" "snooze.plt" X Y))
           (prefix sqlite3: (planet "sqlite3.ss" ("untyped" "snooze.plt" X Y) "sqlite3")))
  
  (define-snooze-interface sqlite3:db@)
  
  (with-database (sqlite3:make-config (string->path "database.db"))
    ...)

  |#
  
  #| Example PostgreSQL usage:

  (require (planet "snooze.ss" ("untyped" "snooze.plt" X Y))
           (prefix postgresql8: (planet "postgresql8.ss" ("untyped" "snooze.plt" X Y) "postgresql8")))
  
  (define-snooze-interface postgresql8:db@)
  
  ; Use #f instead of a password for passwordless connections
  (with-database (postgresql8:make-config "localhost" 5432 "database" "username" "password")
    ...)

  |#
  
  )
 
