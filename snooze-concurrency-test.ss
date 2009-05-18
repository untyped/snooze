#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "era/era.ss")

; Tests -------------------------------------------

; test-suite
(define snooze-concurrency-tests
  (test-suite "snooze-concurrency-tests"
    
    #:before
    (lambda ()
      (recreate-test-tables))
    
    #:after
    drop-all-tables))

; Provide statements -----------------------------

(provide snooze-concurrency-tests)
