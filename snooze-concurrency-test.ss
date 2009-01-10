#lang scheme/base

(require scheme/class)

(require "snooze-syntax.ss"
         "test-base.ss")

(provide make-snooze-concurrency-tests)

; snooze% -> test-suite
(define (make-snooze-concurrency-tests snooze)
  (define-snooze-interface snooze)
  
  ; test-suite
  (test-suite "snooze-concurrency-tests"
    
    ; We need to make a test-case that accurately reconstructs the behaviour of the
    ; web-server, but what with thread-pooling and what have you, there hasn't been
    ; time yet.
    
    (test-case "concurrent connections don't interfere with one another"
      (fail "Not implemented (see source in snooze-unit-concurrency-test.ss for explanation)."))
    
    (test-case "concurrent continuations don't interfere with one another"
      (fail "Not implemented (see source in snooze-unit-concurrency-test.ss for explanation)."))
    
    ))
