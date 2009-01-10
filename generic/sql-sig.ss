(module sql-sig mzscheme

  (require (lib "unitsig.ss"))

  (provide (all-defined))
  
  ; Units and signatures -------------------------

  ;; signature sql-quote^ : signature for conversion between Scheme and DBMS primitives
  ;;
  ;; This sig provides procedures for converting between Scheme and database primitives:
  ;;
  ;;   identifiers (for columns, tables and so on)
  ;;     These are typically stored as symbols in Scheme, but are sometimes also 
  ;;     stored as strings. Identifiers can contain many different puncuation symbols
  ;;     (the most common of which is the hyphen), so they typically have to be quoted
  ;;     in the database backend (e.g. [my-table] in SQLite and `my-table` in MySQL).
  ;;
  ;;   data (the atomic values stored in each record)
  ;;     Snooze supports a number of data types (strings, numbers, and so on), which may
  ;;     or may not be supported by a given database backend. "Quoting" data involves
  ;;     converting a Scheme primitive into a corresponding string for insertion into SQL.
  ;;     "Unquoting" data involves converting the value from the result of a query back into
  ;;     a Scheme primitive.
  ;;
  ;;     A Scheme data type may be stored differently by different database backends. Snooze
  ;;     does not guarantee that data in one DBMS can be ported to another without conversion.
  ;;     Snooze currently does not provide software tools for migrating from database to
  ;;     database.
  (define-signature sql-quote^
    (quote-id       ; (U symbol string) -> string
     quote-data     ; type any -> string
     unquote-data   ; type string -> any
     ;; make-data-unquoter
     ;;     : (list-of type)
     ;;    -> ((U (row-of (U string #f)) #f) -> (U (vector-of scheme-primitive) #f))
     ;;
     ;; where (row-of ...) represents a list or vector of quoted values, or whatever the DB
     ;; passes to Snooze.
     ;; 
     ;; Creates a procedure that "unquotes" (parses) a vector of data retrieved from a database.
     make-data-unquoter))

  ;; signature sql-query^ : top-level signature for select statements
  ;;
  ;; Snooze uses structures to represent the various aspects of a SELECT query in a DBMS-
  ;; independent way. This signature provides functions for converting these structures into
  ;; SQL. While must of the data manipulation language in SQL is common across DBMSs, subtle
  ;; DBMS-specific variations may exist (particularly in the quoted forms of identifiers and
  ;; data primitives - see sql-quote^).
  (define-signature sql-select^
    (select-sql))       ; select -> string
  
  ;; signature sql-select-internals^ : signature for constructing SELECT statements
  (define-signature sql-select-internals^
    (display-select-sql  ; select output-port -> void
     display-what-sql    ; (list-of (U field aggregate)) output-port -> void
     display-from-sql    ; (U table join select) output-port -> void ; doesn't display the word "FROM"
     display-expr-sql    ; expr output-port output-port -> void ; doesn't display the words "ON" or "WHERE"
     display-group-sql   ; (list-of (U field aggregate)) output-port -> void ; doesn't display the words "GROUP BY"
     display-order-sql)) ; (list-of order) output-port -> void ; doesn't display the words "ORDER BY"
  
  ;; signature sql-update^ : signature for creating, updating and dropping tables and records
  ;;
  ;; This sig provides procedures for creating and dropping entities in/from the database.
  ;;
  ;; DBMSs vary greatly in their support for the different features of Snooze: the SQL
  ;; produced by these procedures will be quite DBMS specific.
  ;;
  ;; Note that a single Snooze entity may map to several tables (and potentially other 
  ;; structures) in a database. The create-sql procedure returns a single SQL string that
  ;; creates all of the necessary structures in the correct order. The drop-sql procedure
  ;; returns SQL to drop all of the structures created by create-sql.
  (define-signature sql-update^
    (create-sql       ; entity -> string
     drop-sql         ; entity -> string
     insert-sql       ; persistent-struct -> string
     update-sql       ; persistent-struct -> string
     delete-sql))     ; entity integer -> string      ; an entity and an id
  
  ;; signature sql-update-internals^ : convenience signature for building parts of update queries
  (define-signature sql-update-internals^
    (create-fields-sql ; entity -> string
     column-names      ; entity -> string
     column-values))   ; persistent-struct -> string
  
  )