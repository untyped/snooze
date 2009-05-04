#lang scheme/base

(require scheme/match
         srfi/26
         (planet untyped/unlib:3/gen)
         "test-base.ss"
         "test-data.ss"
         "era/era.ss"
         "sql/sql.ss")

(define-entity Des
  ([val integer]))

(define-entity Src
  ([to  Des]
   [val integer]))

; Tests ----------------------------------------

; snooze% -> test-suite
(define (make-snooze-guid-tests snooze)
  (test-suite "snooze-guid-tests"
    
    ))

; Provide statements -----------------------------

(provide make-snooze-guid-tests)
