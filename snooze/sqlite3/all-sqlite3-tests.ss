(module all-sqlite3-tests mzscheme
  
  (require (file "../test-base.ss")
           (file "sql-quote-unit-test.ss")
           (file "sql-test.ss"))
  
  (provide all-sqlite3-tests)
  
  (define all-sqlite3-tests
    (test-suite 
     "all-sqlite3-tests"
     sql-quote-unit-tests
     sql-tests))
  
  )