#lang scheme/base

(require mzlib/etc
         "../test-base.ss"
         "sql-alias.ss"
         (prefix-in sql: "sql-lang.ss")
         "sql-struct.ss"
         "sql-util.ss")

; Tests ----------------------------------------

(define sql-util-tests
  (test-suite "sql-util.ss"))

; Provide statements -----------------------------

(provide sql-util-tests)
