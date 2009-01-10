(module extract mzscheme
  
  (require (lib "unitsig.ss"))
  
  (require (file "../base.ss")
           (file "extract-sig.ss")
           (file "extract-unit.ss")
           (file "sql-sig.ss")
           (file "sql-quote-unit.ss"))
  
  (provide (all-defined))
  
  (define-values/invoke-unit/sig
    extract^
    (compound-unit/sig
      (import)
      (link (quote   : sql-quote^ (sql-quote@))
            (extract : extract^ (extract@ quote)))
      (export (open extract))))
  
  )
