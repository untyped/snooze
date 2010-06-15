#lang scheme/base
  
(require "../test-base.ss")

(require "extract-test.ss"
         "connection-pool-test.ss"
         "cross-reference-test.ss")

(define/provide-test-suite all-common-tests
  extract-tests
  connection-pool-tests
  cross-reference-tests)
