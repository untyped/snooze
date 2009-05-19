#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "era/era.ss"
         (prefix-in real: "era/snooze-struct.ss"))

; Tests -------------------------------------------

; test-suite
(define snooze-delete-tests
  (test-suite "snooze-delete-tests"
    
    (test-case "delete! an unsaved struct should cause an error"
      (let ([per1 (make-person "Jon")])
        (check-exn exn:fail:snooze? (cut (delete! per1)))))
    
    
    
    ))

; Provide statements -----------------------------

(provide snooze-delete-tests)