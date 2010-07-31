#lang scheme/base

(require mzlib/etc
         scheme/class
         scheme/unit
         "snooze-api.ss"
         "snooze-check-test.ss"
         "snooze-concurrency-test.ss"
         "snooze-delete-test.ss"
         "snooze-find-test.ss"
         "snooze-foreign-key-test.ss"
         "snooze-hook-test.ss"
         "snooze-make-test.ss"
         "snooze-modify-test.ss"
         "snooze-quick-find-test.ss"
         "snooze-save-test.ss"
         "snooze-transaction-test.ss"
         "test-base.ss"
         "audit/all-audit-tests.ss"
         "core/all-core-tests.ss"
         "common/all-common-tests.ss"
         "sql/all-sql-tests.ss")

; Tests ----------------------------------------

; snooze% test-suite -> test-suite
(define (make-snooze-tests all-back-end-tests)
  (test-suite "snooze"
    
    #:before
    (lambda ()
      (connect)
      (drop-all-tables))
    
    ; Tests that can be run without a database connection:
    #;all-core-tests
    #;all-sql-tests
    
    ; Tests for the back end:
    #;all-common-tests
    #;all-back-end-tests
    
    ; Tests the front end:
    #;snooze-make-tests
    #;snooze-save-tests
    #;snooze-delete-tests
    #;snooze-modify-tests
    #;snooze-foreign-key-tests
    #;snooze-check-tests
    #;snooze-hook-tests
    #;snooze-transaction-tests
    ;snooze-concurrency-tests
    #;snooze-find-tests
    #;snooze-quick-find-tests
    ; Tests for the audit trails:
    all-audit-tests))

; Provides ---------------------------------------

(provide make-snooze-tests)
