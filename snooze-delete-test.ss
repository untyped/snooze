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
    
    (test-case "delete! an unsaved struct : should cause an error"
      (recreate-test-tables/cache)
      (let ([per1 (make-person "Jon")])
        (check-exn exn:fail:snooze? (cut (delete! per1)))))
    
    ; fails for now...
    #;(test-case "delete! a saved struct : has correct effect on cache size" 
        (recreate-test-tables/cache)
        (with-cache
         (let ([per1 (save! (make-person "Jon"))])
           (collect-garbage)
           (check-cache-size (list 2 1))
           (let ([per2 (delete! per1)])
             (collect-garbage)
             (check-cache-size (list 2 0))) ; vanilla GUID still present - points to #f
           (collect-garbage)
           (check-cache-size (list 0 0)))))
    
    (test-case "delete! a saved struct : deleted struct should have same contents, without id/revision"
      (recreate-test-tables/cache)
      (let* ([per1 (save! (make-person "Jon"))]
             [per2 (delete! per1)])
        (check-equal? per1 per2)
        (check-true (struct-eq? per1 per2))
        (check-false (eq? per1 per2))
        (check-equal? (person-name per2) "Jon")))
    
    ))

; Provide statements -----------------------------

(provide snooze-delete-tests)