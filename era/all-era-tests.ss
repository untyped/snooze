#lang scheme/base
  
(require (file "../test-base.ss")
         (file "annotation-test.ss")
         (file "era-test.ss"))

; Tests -------------------------------------------

; test-suite
(define all-era-tests
  (test-suite "era"
    annotation-tests
    era-tests))

; Provide statements -----------------------------

(provide all-era-tests)
