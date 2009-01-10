(module sql-select-unit mzscheme
  
  (require (lib "etc.ss")
           (lib "unitsig.ss"))
  
  (require (planet "profile.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "sql-sig.ss"))
  
  (provide sql-select@)
  
  ;; unit sql-select@ : sql-select-internals^ sql-quote^ -> sql-select^
  (define sql-select@
    (unit/sig sql-select^
      (import sql-select-internals^ sql-quote^)
      
      ;; select-sql : select -> string
      (define (select-sql select)
        (let ([out (open-output-string)])
          (display-select-sql select out)
          (display ";" out)
          (get-output-string out)))
      
      ))
  
  
  )