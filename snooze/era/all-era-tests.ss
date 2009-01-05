#lang scheme/base
  
(require "../test-base.ss"
         "annotation-test.ss"
         "era-test.ss")

; Tests -------------------------------------------

; test-suite
(define all-era-tests
  (test-suite "era"
    annotation-tests
    era-tests))

; Provide statements -----------------------------

(provide all-era-tests)
