#lang scheme/base

(require "snooze-api.ss"
         "test-base.ss"
         "test-data.ss"
         "era/era.ss"
         "sql/sql.ss")

; Tests ----------------------------------------

(define-alias a person)

; test-suite
(define snooze-create-tests
  (test-suite "snooze-create-tests"
    
    #:before
    drop-all-tables
    
    #:after
    drop-all-tables
    
    (test-case "create-table, drop-table and table-exists?, entity arguments"
      (check-false (table-exists? person))
      (create-table person)
      (check-pred table-exists? person)
      (drop-table person)
      (check-false (table-exists? person)))
    
    (test-case "drop-table and table-exists?, symbol arguments"
      (check-false (table-exists? 'people))
      (create-table person)
      (check-pred table-exists? 'people)
      (drop-table 'people)
      (check-false (table-exists? 'people)))
    
    (test-case "table-names"
      (check-equal? (table-names) null)
      (create-table person)
      (create-table pet)
      (create-table course)
      (check-equal? (table-names) (list 'courses 'people 'pets))
      (drop-table 'people)
      (drop-table 'pets)
      (drop-table 'courses)
      (check-equal? (table-names) (list)))))

; Provide statements -----------------------------

(provide snooze-create-tests)
