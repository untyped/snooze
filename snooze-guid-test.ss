#lang scheme/base

(require "test-base.ss")

(require (planet untyped/unlib:3/gen)
         "test-data.ss"
         "snooze-api.ss"
         "era/era.ss"
         "sql/sql.ss")

(define-entity node
  ([to  node]
   [val integer]))

; Tests ----------------------------------------

; snooze% -> test-suite
(define snooze-guid-tests
  (test-suite "snooze-guid-tests"
    
    #:before
    (lambda ()
      (drop-all-tables)
      (create-table node))
    
    #:after
    drop-all-tables
    
    
    
    ))

; Provide statements -----------------------------

(provide snooze-guid-tests)
