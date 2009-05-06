#lang scheme/base

(require mzlib/etc
         scheme/class
         scheme/unit
         "extract-test.ss"
         "quick-find-test.ss"
         "snooze-api.ss"
         "snooze-create-test.ss"
         "snooze-concurrency-test.ss"
         "snooze-find-test.ss"
         "snooze-guid-test.ss"
         "snooze-hook-test.ss"
         "snooze-modify-test.ss"
         "snooze-revision-test.ss"
         "snooze-transaction-test.ss"
         "test-base.ss"
         "test-data.ss"
         ;"audit/all-audit-tests.ss"
         "check/all-check-tests.ss"
         "era/all-era-tests.ss"
         "sql/all-sql-tests.ss")

; Tests ----------------------------------------

; snooze% test-suite -> test-suite
(define (make-snooze-tests back-end-tests)
  (test-suite "snooze"
    
    #:before
    drop-all-tables
    
    ; Tests that can be run without a database connection:
    all-era-tests
    extract-tests
    all-sql-tests
    
    ; Tests for the back end:
    back-end-tests
    
    ; Tests the front end:
    snooze-create-tests
    snooze-modify-tests
    snooze-hook-tests
    snooze-find-tests
    snooze-revision-tests
    snooze-transaction-tests
    snooze-concurrency-tests
    quick-find-tests
    
    ; Tests for the audit trails:
    ;(make-audit-tests snooze)
    
    ; Tests for the check library:
    all-check-tests))

; Provide statements -----------------------------

(provide make-snooze-tests)
