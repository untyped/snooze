#lang scheme/base
  
(require "../test-base.ss")

(require "../core/struct.ss"
         "extract-test.ss"
         "connection-pool-test.ss"
         "cross-reference-test.ss")

(define/provide-test-suite all-common-tests
  extract-tests
  (make-connection-pool-tests (current-snooze))
  cross-reference-tests)
