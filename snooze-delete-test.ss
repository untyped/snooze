#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "core/core.ss"
         (prefix-in real: "core/snooze-struct.ss"))

; Tests -------------------------------------------

; test-suite
(define snooze-delete-tests
  (test-suite "snooze-delete-tests"
    
    (test-case "delete! an unsaved struct : should cause an error"
      (recreate-test-tables)
      (let ([per1 (make-person "Jon")])
        (check-exn exn:fail:snooze? (cut (delete! per1)))))
    
    (test-case "delete! a saved struct : deleted struct should have same contents"
      (recreate-test-tables)
      (let* ([per1 (save! (make-person "Jon"))]
             [per2 (delete! per1)])
        (check-equal? per1 per2)
        (check-true (snooze-struct-eq? per1 per2))
        (check-false (eq? per1 per2))
        (check-equal? (person-name per2) "Jon")
        (check-false (snooze-struct-saved? per1))
        (check-false (snooze-struct-saved? per2))))
    
    (test-case "delete! : changes to id and revision"
      (recreate-test-tables)
      (let* ([per1 (save! (make-person "Jon"))]
             [per2 (delete! per1)])
        (check-pred snooze-struct-saved? per1)
        (check-pred database-guid? (snooze-struct-guid per1))
        (check-equal? (snooze-struct-revision per1) 0)
        
        (check-false (snooze-struct-saved? per2))
        (check-false (database-guid? (snooze-struct-guid per1)))
        (check-false (snooze-struct-revision per2))))
        
    (test-case "delete! : resaving or redeleting causes error"
      (recreate-test-tables)
      (let* ([per1 (save! (make-person "Per1"))]
             [per2 (delete! per1)])
        (check-exn exn:fail:snooze? (cut (save! per2)))
        (check-exn exn:fail:snooze? (cut (delete! per2)))))
    
    (test-case "delete! has the correct effect on the database"
      (let* ([per1 (save! (make-person "per1"))]
             [per2 (delete! per1)])
        (check-false (find-by-guid (snooze-struct-guid per1)))))))

; Provide statements -----------------------------

(provide snooze-delete-tests)
