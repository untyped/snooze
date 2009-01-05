#lang mzscheme

(require "../test-base.ss"
         "annotation-test.ss"
         "check-combinator-test.ss"
         "result-combinator-test.ss"
         "result-test.ss")

(define all-check-tests
  (test-suite "check"
    result-tests
    annotation-tests
    check-combinator-tests
    result-combinator-tests))

; Provide statements -----------------------------

(provide all-check-tests)
