#lang scheme/base

(require "../test-base.ss"
         "sql-alias-test.ss"
         "sql-lang-test.ss"
         "sql-syntax-test.ss"
         "sql-util-test.ss")

; Tests ------------------------------------------

(define all-sql-tests
  (test-suite "sql"
    sql-alias-tests
    sql-util-tests
    sql-lang-tests
    sql-syntax-tests))

; Provide statements -----------------------------

(provide all-sql-tests)
