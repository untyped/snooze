#lang scheme/base
  
(require "../test-base.ss"
         "sql-data-unit-test.ss"
         "sql-name-unit-test.ss"
         "sql-query-unit-test.ss")

(provide all-sqlite3-tests)

(define all-sqlite3-tests
  (test-suite "sqlite3"
    sql-data-unit-tests
    sql-name-unit-tests
    sql-query-unit-tests))
