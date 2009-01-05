#lang scheme/base

(require scheme/class)

(require "snooze-syntax.ss"
         "snooze-syntax-test-data.ss"
         "test-base.ss")

(provide snooze-syntax-tests)

(define snooze #f)

(define-snooze-interface defined: snooze)

; test-suite
(define snooze-syntax-tests
  (test-suite "snooze-syntax.ss"
    
    (test-pred "define-snooze-interface: method wrapper"
      procedure?
      defined:save!)
    
    (test-pred "provide-snooze-interface: method wrapper"
      procedure?
      provided:save!)
    
    ))
