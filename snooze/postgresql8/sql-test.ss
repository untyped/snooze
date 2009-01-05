(module sql-test mzscheme
  
  (require (file "../test-base.ss")
           (file "sql.ss"))
  
  (provide sql-tests)

  (define sql-tests
    (test-suite
     "sql.ss"

     ))

  )
