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
    
    #:after
    (lambda ()
      (drop-table person)
      (drop-table pet)
      (drop-table course))
    
    (test-case "create-table, drop-table and table-exists?, entity arguments"
      (check-false (table-exists? person))
      (create-table person)
      (check-pred table-exists? person)
      (drop-table person)
      (check-false (table-exists? person)))
    
    (test-case "drop-table and table-exists?, symbol arguments"
      (check-false (table-exists? 'Person))
      (create-table person)
      (check-pred table-exists? 'Person)
      (drop-table 'Person)
      (check-false (table-exists? 'Person)))
    
    (test-case "table-names"
      (check-equal? (table-names) null)
      (create-table person)
      (create-table pet)
      (create-table course)
      (check-equal? (table-names) (list 'Course 'Person 'Pet))
      (drop-table 'Person)
      (drop-table 'Pet)
      (drop-table 'Course)
      (check-equal? (table-names) (list)))))

; Provide statements -----------------------------

(provide snooze-create-tests)
