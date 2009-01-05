#lang scheme/base
  
(require "../test-base.ss"
         "sql-data-unit-test.ss"
         "sql-name-unit-test.ss"
         "sql-query-unit-test.ss")

(define all-postgresql8-tests
  (test-suite "postgresql8"
    sql-data-unit-tests
    sql-name-unit-tests
    sql-query-unit-tests))

(provide all-postgresql8-tests)
