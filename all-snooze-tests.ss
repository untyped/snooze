#lang scheme/base

(require mzlib/etc
         scheme/class
         scheme/unit
         "quick-find-test.ss"
         "snooze-api.ss"
         "snooze-concurrency-test.ss"
         "snooze-delete-test.ss"
         "snooze-find-test.ss"
         "snooze-foreign-key-test.ss"
         ;"snooze-hook-test.ss"
         "snooze-make-test.ss"
         "snooze-modify-test.ss"
         "snooze-save-test.ss"
         ;"snooze-transaction-test.ss"
         "test-base.ss"
         ;"audit/all-audit-tests.ss"
         "check/all-check-tests.ss"
         "era/all-era-tests.ss"
         "generic/all-generic-tests.ss"
         "sql/all-sql-tests.ss")

; Tests ----------------------------------------

; snooze% test-suite -> test-suite
(define (make-snooze-tests all-back-end-tests)
  (test-suite "snooze"
    
    #:before
    drop-all-tables
    
    ; Tests that can be run without a database connection:
    all-era-tests
    all-sql-tests
    
    ; Tests for the back end:
    all-generic-tests
    all-back-end-tests
    
    ; Tests the front end:
    snooze-make-tests
    snooze-save-tests
    snooze-delete-tests
    snooze-modify-tests
    snooze-foreign-key-tests
    ;snooze-hook-tests
    ;snooze-transaction-tests
    ;snooze-concurrency-tests
    snooze-find-tests
    ;quick-find-tests
    
    ; Tests for the audit trails:
    ;(make-audit-tests snooze)
    
    ; Tests for the check library:
    all-check-tests))

; Provide statements -----------------------------

(provide make-snooze-tests)
