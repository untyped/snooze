#lang scheme/base

(require mzlib/etc
         scheme/class
         srfi/26/cut
         (planet untyped/unlib:3/pipeline)
         "base.ss"
         "snooze-syntax.ss"
         "test-base.ss"
         "test-data.ss"
         "test-util.ss"
         "era/era.ss"
         "sql/sql.ss")

; Tests ----------------------------------------

; snooze% -> test-suite
(define (make-snooze-transaction-tests snooze)
  
  (define-snooze-interface snooze)

  (define-alias per person)
  (define-alias cou course)

  ; (integer -> course)
  (define (find-course-by-value val)
    (find-one (sql:select #:from cou #:where (sql:= cou-value val))))
  
  ; time-tai
  (define time-tai1 (string->time-tai "2001-01-01 01:01:01"))
  
  ; course
  (define course (make-course 'code "Name" 12345 1234.5 #t time-tai1))
  
  ; (U integer #f)
  ; initialised below
  (define course-revision #f)
  
  ; test-suite
  (test-suite "snooze-transaction-tests"
    
    ; ***** NOTE *****
    ; Each test below depends on the tests before it. Add/edit tests at your peril!
    ; ****************
    
    ; create test data for transaction tests
    #:before
    (lambda ()
      (create-table entity:course)
      (create-table entity:person)
      (save! course)
      (set! course-revision (struct-revision course)))
    
    ; delete test data from transaction tests
    #:after
    (lambda ()
      (drop-table entity:person)
      (drop-table entity:course))
    
    (test-case "call-with-transaction: transaction committed"
      (check-not-false (find-course-by-value 12345) "Precondition failed.")
      (call-with-transaction
       (lambda ()
         (set-course-value! course 54321)
         (save! course)))
      ; Revision number should have increased by 1:
      (check-equal? (struct-revision course) (add1 course-revision))
      (check-not-false (find-course-by-value 54321) "Postcondition failed.")
      ; Need to reset the course-revision variable for subsequent tests:
      (set! course-revision (struct-revision course)))
    
    (test-case "call-with-transaction: transaction aborted"
      (check-not-false (find-course-by-value 54321) "Precondition failed.")
      (with-handlers ([exn:snooze? void])
        (call-with-transaction
         (lambda ()
           (set-course-value! course 12345)
           (save! course)
           (raise-exn exn:snooze "Aborting transaction."))))
      (check-equal? (struct-revision course) course-revision "check 1")
      (check-not-false (find-course-by-value 54321) "Postcondition failed."))
    
    (test-case "call-with-transaction: nested transactions aborted"
      (check-not-false (find-course-by-value 54321) "check 1 - precondition 1")
      (check-equal? (course-value course) 54321 "check 2 - precondition 2")
      (with-handlers ([exn:snooze? void])
        (call-with-transaction
         (lambda ()
           (set-course-value! course 12345)
           (save! course)
           (call-with-transaction
            (lambda ()
              (set-course-value! course 13579)
              (save! course)
              (raise-exn exn:snooze "Aborting transaction."))))))
      (check-equal? (struct-revision course) course-revision "check 3")
      (check-equal? (course-value course) 54321 "check 4")
      (check-not-false (find-course-by-value 54321) "check 5 - postcondition"))
    
    (test-case "call-with-transaction: inner nested transaction aborted (SQLite will fail this test)"
      (check-not-false (find-course-by-value 54321) "Precondition failed.")
      (call-with-transaction
       (lambda ()
         (set-course-value! course 12345)
         (save! course)
         (with-handlers ([exn:snooze? void])
           (call-with-transaction
            (lambda ()
              (set-course-value! course 13579)
              (save! course)
              (raise-exn exn:snooze "Aborting transaction."))))))
      (check-not-false (find-course-by-value 12345) 
                       (format "Postcondition failed (~a)."
                               (if (find-course-by-value 13579)
                                   "both nested transactions were aborted: this is the expected behaviour for SQLite"
                                   (format "final course value was: ~a"
                                           (course-value (find-by-id entity:course (course-id course))))))))
    
    ; Persistent struct roll back -----------
    
    (test-case "call-with-transaction: attributes rolled back"
      (let ([course (make-course 'code "Name" 10000 1234.5 #t time-tai1)])
        (set-course-value! course 12345)
        (save! course)
        (set-course-value! course 23456)
        (check-equal? (struct-revision course) 0 "check 1")
        (check-equal? (course-value course) 23456 "check 2")
        (with-handlers ([exn:snooze? void]) ; Should roll back to here, where value is 23456
          (call-with-transaction
           (lambda ()
             (set-course-value! course 54321)
             (save! course)
             (check-equal? (struct-revision course) 1 "check 3")
             (check-equal? (course-value course) 54321 "check 4")
             (raise-exn exn:snooze "Aborting transaction."))))
        (check-equal? (struct-revision course) 0 "check 5")
        (check-equal? (course-value course) 23456 "check 6")
        (check-not-exn (cut save! course) "check 7")
        (check-not-exn (cut delete! course) "check 8")))
    
    (test-case "call-with-transaction: repeated assignments rolled back"
      (let ([course (make-course 'code "Name" 10000 1234.5 #t time-tai1)])
        (save! course)
        (set-course-value! course 12345)
        (set-course-value! course 23456)
        (with-handlers ([exn:snooze? void])
          (call-with-transaction
           (lambda ()
             (set-course-value! course 54321)
             (set-course-value! course 65432)
             (save! course)
             (check-equal? (course-value course) 65432 "check 1")
             (raise-exn exn:snooze "Aborting transaction."))))
        (check-equal? (course-value course) 23456 "check 2")
        (check-not-exn (cut save! course) "check 3")
        (check-not-exn (cut delete! course) "check 4")))
    
    (test-case "call-with-transaction: set enable-transaction-backups? to #f"
      (parameterize ([enable-transaction-backups? #f])
        (let ([course (make-course 'code "Name" 10000 1234.5 #t time-tai1)])
          (save! course)
          (set-course-value! course 12345)
          (set-course-value! course 23456)
          (with-handlers ([exn:snooze? void])
            (call-with-transaction
             (lambda ()
               (set-course-value! course 54321)
               (set-course-value! course 65432)
               (save! course)
               (check-equal? (course-value course) 65432 "check 1")
               ; Changes shouldn't be undone:
               (raise-exn exn:snooze "Aborting transaction."))))
          (check-equal? (course-value course) 65432)
          ; Can't delete or save because revision numbers are out of sync:
          (check-exn exn:fail:snooze:revision? (cut save! course) "check 2")
          (check-exn exn:fail:snooze:revision? (cut delete! course) "check 3")
          ; Have to delete the test record by loading/deleting it:
          (check-not-exn (cut delete! (find-by-id entity:course (struct-id course))) "check 4"))))
    
    (test-case "cannot make full continuation jumps into or out of transactions"
      (begin-with-definitions
        
        ; General continuation jump out:
        (let/cc escape
          (call-with-transaction
           (lambda ()
             (check-exn exn:fail:contract:continuation?
               (lambda ()
                 (escape #f))
               "check 1"))))
        
        ; Escape continuation jump out:
        (define resume
          (check-not-exn
            (lambda ()
              (let/ec escape
                (call-with-transaction
                 (lambda ()
                   (let/cc resume
                     (escape resume))))))
            "check 2"))
        
        ; General continuation jump in:
        (check-exn exn:fail:contract:continuation?
          (lambda ()
            (resume #f))
          "check 3")))
    
    (test-case "transaction-pipeline called"
      (begin-with-definitions
        
        ; (U string #f)
        (define num-transactions 0)
        
        ; stage
        (define-stage (log-stage continue conn . args)
          (set! num-transactions (add1 num-transactions))
          (apply continue conn args))
        
        (send snooze set-transaction-pipeline! (list log-stage))
        
        (delete! (save! (make-person "Dave")))
        
        (check-equal? num-transactions 2)))
    
    (test-case "transaction-pipeline aborts transaction before body"
      (begin-with-definitions
        
        ; stage
        (define-stage (log-stage continue conn . args)
          (raise-exn exn:snooze "Escaping")
          (apply continue conn args))
        
        (send snooze set-transaction-pipeline! (list log-stage))
          
        (with-handlers ([exn:snooze? void])
          (save! (make-person "Dave")))
        
        (check-equal? (length (find-all (sql:select #:from per))) 0 "check 1")))
    
    (test-case "transaction-pipeline aborts transaction after body"
      (begin-with-definitions
        
        ; stage
        (define-stage (log-stage continue conn . args)
          (begin0 (apply continue conn args)
                  (raise-exn exn:snooze "Escaping")))
        
        (send snooze set-transaction-pipeline! (list log-stage))

        (with-handlers ([exn:snooze? void])
          (save! (make-person "Dave")))
        
        (check-equal? (length (find-all (sql:select #:from per))) 0 "check 1")))
    
    (test-case "structs rolled back when transaction aborted"
      (begin-with-definitions
        
        (define person (make-person "Dave"))
        
        (send snooze set-transaction-pipeline! null)
        
        (check-not-exn
          (lambda ()
            (let/ec escape
              (call-with-transaction
               (lambda ()
                 (set-person-name! person "Noel")
                 (save! person)
                 (escape #f)))))
          "check 0")
        
        (check-equal? (person-name person) "Dave" "check 1")
        
        (check-not-exn
          (lambda ()
            (let/ec escape
              (set-person-name! person "Matt")
              (save! person)
              (escape #f)))
          "check 1.5")
        
        (check-equal? (person-name person) "Matt" "check 2")
        
        (delete! person)))))

; Provide statements -----------------------------

(provide make-snooze-transaction-tests)

