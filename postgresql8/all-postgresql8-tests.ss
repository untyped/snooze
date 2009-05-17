#lang scheme/base
  
(require "../test-base.ss")

(require "sql-test.ss")

(define all-postgresql8-tests
  (test-suite "postgresql8"
    sql-tests))

(provide all-postgresql8-tests)
