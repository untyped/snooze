#lang scheme/base
  
(require "../test-base.ss"
         "cached-struct-test.ss"
         "check-annotation-test.ss"
         "check-result-test.ss"
         "check-test.ss"
         "core-test.ss"
         "define-entity-test.ss"
         "pretty-test.ss"
         "snooze-struct-test.ss")

; Tests -------------------------------------------

; test-suite
(define all-core-tests
  (test-suite "core"
    core-tests
    snooze-struct-tests
    cached-struct-tests
    define-entity-tests
    pretty-tests
    check-result-tests
    check-annotation-tests
    check-tests))

; Provide statements -----------------------------

(provide all-core-tests)
