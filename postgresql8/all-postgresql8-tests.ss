#lang scheme/base
  
(require (file "../test-base.ss")
         (file "sql-data-unit-test.ss")
         (file "sql-name-unit-test.ss")
         (file "sql-query-unit-test.ss"))

(define all-postgresql8-tests
  (test-suite "postgresql8"
    sql-data-unit-tests
    sql-name-unit-tests
    sql-query-unit-tests))

(provide all-postgresql8-tests)
