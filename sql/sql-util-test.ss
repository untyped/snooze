#lang scheme/base

(require mzlib/etc
         (file "../test-base.ss")
         (file "../test-data.ss")
         (file "sql-alias.ss")
         (prefix-in sql: (file "sql-lang.ss"))
         (file "sql-struct.ss")
         (file "sql-util.ss"))

; Tests ----------------------------------------

(define sql-util-tests
  (test-suite "sql-util.ss"

    ))

; Provide statements -----------------------------

(provide sql-util-tests)
