#lang scheme/base
  
(require "../test-base.ss")

(require "extract-test.ss")

(define all-generic-tests
  (test-suite "generic"
    extract-tests))

(provide all-generic-tests)
