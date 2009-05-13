#lang scheme/base
  
(require "../test-base.ss"
         "cached-struct-test.ss"
         "core-test.ss"
         "define-entity-test.ss"
         "guid-test.ss"
         "pretty-test.ss"
         "snooze-struct-test.ss")

; Tests -------------------------------------------

; test-suite
(define all-era-tests
  (test-suite "era"
    core-tests
    guid-tests
    snooze-struct-tests
    cached-struct-tests
    define-entity-tests
    pretty-tests))

; Provide statements -----------------------------

(provide all-era-tests)
