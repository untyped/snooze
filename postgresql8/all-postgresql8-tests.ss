#lang scheme/base
  
(require "../test-base.ss"
         "postgresql8-test.ss"
         "sql-data-unit-test.ss"
         "sql-name-unit-test.ss"
         "sql-query-unit-test.ss")

(define (make-all-postgresql8-tests snooze)
  (test-suite "postgresql8"
    (make-postgresql8-tests snooze)
    sql-data-unit-tests
    sql-name-unit-tests
    sql-query-unit-tests))

(provide make-all-postgresql8-tests)
