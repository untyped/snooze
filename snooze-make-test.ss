#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "era/era.ss"
         (prefix-in real: "era/snooze-struct.ss"))

; Tests -------------------------------------------

; test-suite
(define snooze-make-tests
  (test-suite "snooze-make-tests"
    
    (test-case "make-person : returns a local guid"
      (recreate-test-tables/cache)
      (let* ([per     (make-person person)]
             [struct  (send (current-cache) cache-ref/local)]
             [vanilla (send (current-cache) get-vanilla-guid)])
        (check-pred guid-local? per)
        (check-false (car vanilla))
        (check-pred (entity-private-predicate person) struct)
        (check-false (real:struct-guid struct))))))

; Provide statements -----------------------------

(provide snooze-make-tests)