#lang scheme/base

(require scheme/class
         "snooze-api.ss"
         "test-base.ss")

; Tests -------------------------------------------

; test-suite
(define snooze-concurrency-tests
  (test-suite "snooze-concurrency-tests"
    
    ; We need to make a test-case that accurately reconstructs the behaviour of the
    ; web-server, but what with thread-pooling and what have you, there hasn't been
    ; time yet.
    
    #;(test-case "concurrent connections don't interfere with one another"
        (fail "Not implemented."))
    
    #;(test-case "concurrent continuations don't interfere with one another"
        (fail "Not implemented."))))

; Provide statements -----------------------------

(provide snooze-concurrency-tests)
