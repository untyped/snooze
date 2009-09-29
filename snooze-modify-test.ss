#lang scheme/base

(require srfi/26
         "snooze-api.ss"
         "test-base.ss"
         "core/core.ss")

; Tests ------------------------------------------

; This tests the functional update process and its effect on structs.

(define snooze-modify-tests
  (test-suite "snooze-modify-tests"
    
    (test-case "copying a saved struct : creates an independent copy, unsaved, with the same guid"
      (recreate-test-tables)
      (let* ([per  (save! (make-person "Per"))]                   ; saved
             [per2 (person-set per #:name "Per2")])               ; unsaved
        (check-not-eq? per per2))) ; different structs in memory
    
    (test-case "saving a copy of a struct updates the original struct, so that they refer to the same struct"
      (recreate-test-tables)
      (let* ([per   (save! (make-person "Per"))]                   ; saved
             [per2 (save! (person-set per #:name "Per2"))])       ; saved
        (check-equal? (person-name per)  "Per")
        (check-equal? (person-name per2) "Per2")
        (check-bit-eq? per per2))) ; different structs in memory
    
    (test-case "snooze-struct-copy : creates a totally independent copy"
      (let* ([test-person (save! (make-person "Per"))]
             [copy-person (snooze-struct-copy test-person)])
        (after (check-not-equal? copy-person test-person)
               (check-pred integer? (snooze-struct-id       test-person))
               (check-pred integer? (snooze-struct-revision test-person))
               (check-false         (snooze-struct-id       copy-person))
               (check-false         (snooze-struct-revision copy-person))
               (delete! test-person))))))

; Provide statements -----------------------------

(provide snooze-modify-tests)
