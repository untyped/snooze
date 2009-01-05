(module sql mzscheme
  
  (require (lib "unitsig.ss"))
  
  (require (file "../base.ss")
           (file "../generic/sql-select-unit.ss")
           (file "../generic/sql-select-internals-unit.ss")
           (file "../generic/sql-sig.ss")
           (file "sql-quote-unit.ss")
           (file "sql-update-internals-unit.ss")
           (file "sql-update-unit.ss"))
  
  (provide (all-defined))
  
  ; select-sql : select -> string
  (define-values/invoke-unit/sig
    sql-select^
    (compound-unit/sig
      (import)
      (link (quote     : sql-quote^ (sql-quote@))
            (internals : sql-select-internals^ (sql-select-internals@ quote))
            (select    : sql-select^ (sql-select@ internals quote)))
      (export (open select))))
  
  ; create-sql : entity -> string
  ; drop-sql   : entity -> string
  ; insert-sql : persistent-struct -> string
  ; update-sql : persistent-struct -> string
  ; delete-sql : persistent-struct -> string
  (define-values/invoke-unit/sig
    sql-update^
    (compound-unit/sig
     (import)
     (link (quote     : sql-quote^ (sql-quote@))
           (internals : sql-update-internals^ (sql-update-internals@ quote))
           (update    : sql-update^ (sql-update@ internals quote)))
     (export (open update))))
  
  ; quote-id     : (U symbol string) -> string
  ; quote-data   : type any -> string
  ; unquote-data : type any -> string
  (define-values/invoke-unit/sig
    sql-quote^
    sql-quote@)
  
  )
