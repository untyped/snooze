#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "test-data.ss"
         "snooze-cache-test-util.ss"
         "era/era.ss"
         (prefix-in real: "era/snooze-struct.ss"))

; Tests -------------------------------------------

; test-suite
(define snooze-create-tests
  (test-suite "snooze-create-tests"
    
    (test-case "make-person creates a local guid with the correct mapping"
      (recreate-test-tables/cache)
      (let* ([local-guid (make-person person)]
             [local-val  (cdr (assq local-guid (cache-alist)))])
        (check-pred guid-local? local-guid)
        (check-false (car local-val))
        (check-pred real:snooze-struct? (cdr local-val))
        (check-false (real:struct-guid (cdr local-val)))))))

; Provide statements -----------------------------

(provide snooze-create-tests)