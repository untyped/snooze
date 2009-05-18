#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "era/era.ss"
         (prefix-in real: "era/snooze-struct.ss"))

; Tests -------------------------------------------

; test-suite
(define snooze-save-tests
  (test-suite "snooze-save-tests"
    
    (test-case "save! : returns a local guid"
      (recreate-test-tables/cache)
      (let* ([per (save! (make-person "Dave"))])
        (check-cache-size (list 3))
        (check-pred guid-local? per)
        
        ))
    
    (test-case "save! : actually saves data"
      (recreate-test-tables/cache)
      (let* ([local-guid (make-person person)]
             [local-val  (cdr (assq local-guid (cache-alist)))])
        (check-pred guid-local? local-guid)
        (check-false (car local-val))
        (check-pred real:snooze-struct? (cdr local-val))
        (check-false (real:struct-guid (cdr local-val)))))
    
    ))

; Provide statements -----------------------------

(provide snooze-save-tests)