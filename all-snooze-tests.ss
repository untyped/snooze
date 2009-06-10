#lang scheme/base

(require mzlib/etc
         scheme/class
         scheme/unit
         "extract-test.ss"
         "persistent-struct-test.ss"
         "quick-find-test.ss"
         "snooze-create-test.ss"
         "snooze-find-test.ss"
         "snooze-modify-test.ss"
         "snooze-pipeline-test.ss"
         "snooze-revision-test.ss"
         "snooze-syntax-test.ss"
         "snooze-transaction-test.ss"
         "test-base.ss"
         "test-data.ss"
         "audit/all-audit-tests.ss"
         "check/all-check-tests.ss"
         "era/all-era-tests.ss"
         "sql/all-sql-tests.ss")

; Tests ----------------------------------------

; snooze% test-suite -> test-suite
(define (make-snooze-tests snooze back-end-tests)
  (test-suite "snooze"
    
    #:before
    ; Make sure the database is empty:
    (lambda ()
      (for-each (lambda (name)
                  (send snooze drop-table name))
                (send snooze table-names)))
    
    ; Tests that can be run without a database connection:
    all-era-tests
    extract-tests
    persistent-struct-tests
    all-sql-tests
    snooze-syntax-tests
    
    ; Tests for the back end:
    back-end-tests
    
    ; Tests the front end:
    (make-snooze-create-tests snooze)
    (make-snooze-modify-tests snooze)
    (make-snooze-pipeline-tests snooze)
    (make-snooze-find-tests snooze)
    (make-snooze-revision-tests snooze)
    (make-snooze-transaction-tests snooze)
    (make-quick-find-tests snooze)
    
    ; Tests for the audit trails:
    (make-audit-tests snooze)
    
    ; Tests for the check library:
    all-check-tests))

; Provide statements -----------------------------

(provide make-snooze-tests)
