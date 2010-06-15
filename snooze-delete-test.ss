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
        (check-equal? (snooze-struct-data-ref* per1)
                      (snooze-struct-data-ref* per2))))
    
    (test-case "delete! : changes to id and revision"
      (recreate-test-tables)
      (let* ([per1 (save! (make-person "Jon"))]
             [per2 (delete! per1)])
        (check-false (snooze-struct-saved? per1))
        (check-false (snooze-struct-saved? per2))
        (check-equal? (snooze-struct-revision per1) 0)
        (check-equal? (snooze-struct-revision per2) #f)))
        
    (test-case "delete! : resaving and redeleting is okay"
      (recreate-test-tables)
      (check-not-exn
        (lambda ()
          (let* ([per1 (save! (make-person "Per1"))]
                 [per2 (delete! per1)]
                 [per3 (save! per2)]
                 [per4 (delete! per3)])
            (void)))))
    
    (test-case "delete! : resaving and redeleting is not okay if revisions don't match up"
      (recreate-test-tables)
      (let* ([per1 (save! (make-person "Per1"))]
             [per2 (delete! per1)]
             [per3 (save! per2)])
        (check-exn exn:fail:snooze:revision? (cut delete! per2))))
    
    (test-case "delete! has the correct effect on the database"
      (let* ([per1 (save! (make-person "per1"))]
             [id   (snooze-struct-id per1)]
             [per2 (delete! per1)])
        (check-false (find-by-id person id))))))

; Provide statements -----------------------------

(provide snooze-delete-tests)
