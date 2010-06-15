#lang scheme/base
  
(require "../test-base.ss")

(require "sql-test.ss")

(define all-sqlite3-tests
  (test-suite "sqlite3"
    sql-tests))

(provide all-sqlite3-tests)
