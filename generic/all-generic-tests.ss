(module all-generic-tests mzscheme
  
  (require (file "../test-base.ss")
           (file "extract-test.ss")
           (file "sql-quote-unit-test.ss")
           (file "sql-select-internals-unit-test.ss")
           (file "sql-test.ss"))
  
  (provide all-generic-tests)
  
  ; Tests ----------------------------------------
  
  (define all-generic-tests
    (test-suite
     "all-generic-tests"
     extract-tests
     sql-quote-unit-tests
     sql-select-internals-unit-tests
     sql-tests))
  
  )
