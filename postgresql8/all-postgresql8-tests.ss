#lang scheme/base
  
(require "../test-base.ss")

(require "postgresql8-test.ss"
         "sql-test.ss")

; Tests ------------------------------------------

(define/provide-test-suite all-postgresql8-tests
  (test-suite "postgresql8"
    postgresql8-tests
    sql-tests))
