#lang scheme/base

(require (file "../test-base.ss")
         (file "sql-alias-test.ss")
         (file "sql-lang-test.ss")
         (file "sql-syntax-test.ss")
         (file "sql-util-test.ss"))

; Tests ------------------------------------------

(define all-sql-tests
  (test-suite "sql"
    sql-alias-tests
    sql-util-tests
    sql-lang-tests
    sql-syntax-tests))

; Provide statements -----------------------------

(provide all-sql-tests)