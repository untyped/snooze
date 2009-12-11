#lang scheme/base

(require "../test-base.ss")

(require "sql-alias.ss"
         (prefix-in sql: "sql-lang.ss")
         "sql-struct.ss"
         "sql-util.ss")

; Tests ----------------------------------------

(define sql-util-tests
  (test-suite "sql-util.ss"
    
    (test-case "source->foreign-keys"
      (fail "write these tests!!!"))))

; Provide statements -----------------------------

(provide sql-util-tests)
