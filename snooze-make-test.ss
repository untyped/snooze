#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "core/core.ss"
         (prefix-in real: "core/snooze-struct.ss"))

; Tests -------------------------------------------

; test-suite
(define snooze-make-tests
  (test-suite "snooze-make-tests"
    
    (test-case "make-person"
      (recreate-test-tables)
      (let* ([per (make-person "Per")])
        (check-false (snooze-struct-saved? per))
        (check-pred temporary-guid? (snooze-struct-guid per))
        (check-false (snooze-struct-revision per))))
    
    (test-case "make-person twice, same contents: check independence"
      (recreate-test-tables)
      (let* ([per  (make-person "Per")]
             [per2 (make-person "Per")])
        (check-not-eq? per per2)
        (check-not-equal? per per2)
        (check-equal? (snooze-struct-data-ref* per)
                      (snooze-struct-data-ref* per2))))
    
    (test-case "make-person and create an identical copy: ensure completely independent copies"
      (recreate-test-tables)
      (let* ([per  (make-person "Per")]
             [per2 (person-set per #:name "Per")])
        ; per
        (check-pred person? per2)
        (check-false (snooze-struct-saved? per2))
        (check-pred temporary-guid? (snooze-struct-guid per2))
        (check-false (snooze-struct-revision per2))
        (check-not-eq? per per2)
        (check-equal? per per2)))))

; Provide statements -----------------------------

(provide snooze-make-tests)
