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
    
    (test-case "find-one returns a local GUID"
        (recreate-test-tables/cache)
        (let* ([per1    (begin0 (save! (make-person "Jon"))
                                (debug-location))]
               [per2    (select-one #:from person)])
          (check-pred guid-local? per2)))
    
    #;(test-case "delete! an unsaved struct : should cause an error"
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
    
    #;(test-case "delete! a saved struct : deleted struct should have same contents"
        (recreate-test-tables/cache)
        (let* ([per1 (save! (make-person "Jon"))]
               [per2 (delete! per1)])
          (check-equal? per1 per2)
          (check-true (struct-eq? per1 per2))
          (check-false (eq? per1 per2))
          (check-equal? (person-name per2) "Jon")
          (check-false (struct-saved? per1))
          (check-false (struct-saved? per2))))
    
    #;(test-case "delete! a saved struct : falsifies id and revision"
        (recreate-test-tables/cache)
        (let* ([per1    (save! (make-person "Jon"))]
               [per2    (delete! per1)]
               [struct1 (send (current-cache) cache-ref/local per1)]
               [struct2 (send (current-cache) cache-ref/local per2)])
          (check-false (struct-saved? per1))
          (check-false (real:struct-id struct1))
          (check-false (real:struct-revision struct1))
          (check-false (struct-saved? per2))
          (check-false (real:struct-id struct2))
          (check-false (real:struct-revision struct2))))
    
    
    #;(test-case "delete! an uncached struct : should return a new local GUID"
        (recreate-test-tables/cache)
        (let* ([per1    (save! (make-person "Jon"))]
               [per2    (delete! (select-one #:from person))]
               [struct2 (send (current-cache) cache-ref/local per2)]) ; per1
          (check-equal? (person-name per2) "Jon")
          (check-false (struct-saved? per2))
          (check-false (real:struct-id struct2))
          (check-false (real:struct-revision struct2))))
    
    #;(test-case "delete! a copy of a struct, then resaving original causes revision error"
      (recreate-test-tables/cache)
      (let* ([per1 (save! (make-person "Per1"))]
             [per2 (person-set per1)]
             [per3 (save! per2)])
        (check-false (equal? per1 per3))                    ; revision is different
        (check-true (equal? per2 per3))
        (check-false (struct-eq? per1 per3))
        (check-true (struct-eq? per2 per3))
        (check-exn exn:fail:snooze? (cut (delete! per1))))) ; revision exception
    
    #;(test-case "delete! a struct is not allowed if it has bad foreign keys"
      (recreate-test-tables/cache)
      (let* ([per1 (save! (make-person "Per1"))]
             [pet1 (save! (make-pet per1 "Pet1"))]
             [per2 (save! (person-set per1 #:name "Per2"))])
        (check-exn exn:fail? (cut delete! pet1))))
    ))

; Provide statements -----------------------------

(provide snooze-delete-tests)