#lang scheme/base
  
(require scheme/unit
         srfi/19/time)

(require "../test-base.ss"
         "sql-name-unit.ss")

(provide sql-name-unit-tests)

(define-values/invoke-unit/infer sql-name@)

(define sql-name-unit-tests
  (test-suite "sql-name-unit.ss"
    
    (test-equal? "escape-name"
      (escape-name 'my-id)
      "[my-id]")
    
    ))
