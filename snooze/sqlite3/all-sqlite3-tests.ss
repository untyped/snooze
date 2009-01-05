#lang scheme/base
  
(require (file "../test-base.ss")
         (file "sql-data-unit-test.ss")
         (file "sql-name-unit-test.ss")
         (file "sql-query-unit-test.ss"))

(provide all-sqlite3-tests)

(define all-sqlite3-tests
  (test-suite "sqlite3"
    sql-data-unit-tests
    sql-name-unit-tests
    sql-query-unit-tests))
