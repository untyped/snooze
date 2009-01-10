#lang scheme/base

(require "snooze-syntax.ss"
         "test-base.ss"
         "test-data.ss"
         "era/era.ss"
         "sql/sql.ss")

; Tests ----------------------------------------

; snooze% -> test-suite
(define (make-snooze-create-tests snooze)
  (define-snooze-interface snooze)

  (define-alias a person)
  
  ; test-suite
  (test-suite "snooze-create-tests"
    
    #:after
    (lambda ()
      (drop-table entity:person)
      (drop-table entity:pet)
      (drop-table entity:course))
    
    (test-case "create-table, drop-table and table-exists?, entity arguments"
      (check-false (table-exists? entity:person) "check 1")
      (create-table entity:person)
      (check-pred table-exists? entity:person "check 2")
      (drop-table entity:person)
      (check-false (table-exists? entity:person) "check 3"))
    
    (test-case "drop-table and table-exists?, symbol arguments"
      (check-false (table-exists? 'Person) "check 1")
      (create-table entity:person)
      (check-pred table-exists? 'Person "check 2")
      (drop-table 'Person)
      (check-false (table-exists? 'Person) "check 3"))
    
    (test-case "table-names"
      (check-equal? (table-names) null "check 1")
      (create-table entity:person)
      (create-table entity:pet)
      (create-table entity:course)
      (check-equal? (table-names) (list 'Course 'Person 'Pet) "check 2")
      (drop-table 'Person)
      (drop-table 'Pet)
      (drop-table 'Course)
      (check-equal? (table-names) (list) "check 3"))))

; Provide statements -----------------------------

(provide make-snooze-create-tests)
