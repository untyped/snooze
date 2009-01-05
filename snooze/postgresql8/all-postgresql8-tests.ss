(module all-postgresql8-tests mzscheme
  
  (require (file "../test-base.ss")
           (file "sql-quote-unit-test.ss")
           (file "sql-test.ss"))

  (provide all-postgresql8-tests)
  
  (define all-postgresql8-tests
    (test-suite 
     "all-postgresql8-tests"
     sql-quote-unit-tests
     sql-tests))
  
  )