#lang scheme/base
  
(require "../test-base.ss")

(require "extract-test.ss")

(define all-common-tests
  (test-suite "common"
    extract-tests))

(provide all-common-tests)
