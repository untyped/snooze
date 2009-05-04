#lang scheme/base

(require "test-base.ss")

(require scheme/class
         srfi/26
         "snooze-api.ss"
         "test-data.ss"
         "era/era.ss"
         "sql/sql.ss")

; Tests ----------------------------------------

(define-alias per person)
(define-alias cou course)

; (integer -> course)
(define (find-course-by-value val)
  (find-one (sql (select #:from cou #:where (= cou.value ,val)))))

; time-tai
(define time-tai1 (string->time-tai "2001-01-01 01:01:01"))

; course
; integer
; (initisalised below)
(define-values (c1 c1-revision) (values #f #f))

; test-suite
(define snooze-transaction-tests
  (test-suite "snooze-transaction-tests"
    
    ; ***** NOTE *****
    ; Each test below depends on the tests before it. Add/edit tests at your peril!
    ; ****************
    
    ; create test data for transaction tests
    #:before
    (lambda ()
      (create-table course)
      (create-table person)
      (set! c1 (save! (make-course 'code "Name" 12345 1234.5 #t time-tai1)))
      (set! c1-revision (struct-revision c1)))
    
    ; delete test data from transaction tests
    #:after
    (lambda ()
      (drop-table person)
      (drop-table course))
    
    (test-case "call-with-transaction: transaction committed"
      (check-not-false (find-course-by-value 12345) "Precondition failed.")
      (call-with-transaction
       (lambda ()
         (set-course-value! c1 54321)
         (save! c1)))
      ; Revision number should have increased by 1:
      (check-equal? (struct-revision c1) (add1 c1-revision))
      (check-not-false (find-course-by-value 54321) "Postcondition failed.")
      ; Need to reset the c1-revision variable for subsequent tests:
      (set! c1-revision (struct-revision c1)))
    
    (test-case "call-with-transaction: transaction aborted"
      (check-not-false (find-course-by-value 54321) "Precondition failed.")
      (with-handlers ([exn? void])
        (call-with-transaction
         (lambda ()
           (set-course-value! c1 12345)
           (save! c1)
           (error "aborting transaction"))))
      (check-equal? (struct-revision c1) c1-revision "check 1")
      (check-not-false (find-course-by-value 54321) "Postcondition failed."))
    
    (test-case "call-with-transaction: nested transactions aborted"
      (check-not-false (find-course-by-value 54321) "check 1 - precondition 1")
      (check-equal? (course-value c1) 54321 "check 2 - precondition 2")
      (with-handlers ([exn? void])
        (call-with-transaction
         (lambda ()
           (set-course-value! c1 12345)
           (save! c1)
           (call-with-transaction
            (lambda ()
              (set-course-value! c1 13579)
              (save! c1)
              (error "aborting transaction"))))))
      (check-equal? (struct-revision c1) c1-revision "check 3")
      (check-equal? (course-value c1) 54321 "check 4")
      (check-not-false (find-course-by-value 54321) "check 5 - postcondition"))
    
    (test-case "call-with-transaction: inner nested transaction aborted (SQLite will fail this test)"
      (check-not-false (find-course-by-value 54321) "Precondition failed.")
      (call-with-transaction
       (lambda ()
         (set-course-value! c1 12345)
         (save! c1)
         (with-handlers ([exn? void])
           (call-with-transaction
            (lambda ()
              (set-course-value! c1 13579)
              (save! c1)
              (error "aborting transaction"))))))
      (check-not-false (find-course-by-value 12345) 
                       (format "Postcondition failed (~a)."
                               (if (find-course-by-value 13579)
                                   "both nested transactions were aborted: this is the expected behaviour for SQLite"
                                   (format "final course value was: ~a"
                                           (course-value (find-by-id course (course-guid c1))))))))
    
    ; Persistent struct roll back -----------
    
    (test-case "call-with-transaction: attributes rolled back"
      (let ([c1 (make-course 'code "Name" 10000 1234.5 #t time-tai1)])
        (set-course-value! c1 12345)
        (save! c1)
        (set-course-value! c1 23456)
        (check-equal? (struct-revision c1) 0)
        (check-equal? (course-value c1) 23456)
        (with-handlers ([exn? void]) ; Should roll back to here, where value is 23456
          (call-with-transaction
           (lambda ()
             (set-course-value! c1 54321)
             (save! c1)
             (check-equal? (struct-revision c1) 1)
             (check-equal? (course-value c1) 54321)
             (error "aborting transaction"))))
        (check-equal? (struct-revision (find-by-id course (struct-id c1))) 0)
        (check-equal? (struct-revision c1) 0)
        (check-equal? (course-value c1) 23456)
        (check-not-exn (cut save! c1))
        (check-not-exn (cut delete! c1))))
    
    (test-case "call-with-transaction: repeated assignments rolled back"
      (let ([c1 (make-course 'code "Name" 10000 1234.5 #t time-tai1)])
        (save! c1)
        (set-course-value! c1 12345)
        (set-course-value! c1 23456)
        (with-handlers ([exn? void])
          (call-with-transaction
           (lambda ()
             (set-course-value! c1 54321)
             (set-course-value! c1 65432)
             (save! c1)
             (check-equal? (course-value c1) 65432)
             (error "aborting transaction"))))
        (check-equal? (course-value c1) 23456)
        (check-not-exn (cut save! c1))
        (check-not-exn (cut delete! c1))))
    
    #;(test-case "call-with-transaction: set enable-transaction-backups? to #f"
        (parameterize ([enable-transaction-backups? #f])
          (let ([c1 (make-course 'code "Name" 10000 1234.5 #t time-tai1)])
            (save! c1)
            (set-course-value! c1 12345)
            (set-course-value! c1 23456)
            (with-handlers ([exn? void])
              (call-with-transaction
               (lambda ()
                 (set-course-value! c1 54321)
                 (set-course-value! c1 65432)
                 (save! c1)
                 (check-equal? (course-value c1) 65432)
                 ; Changes shouldn't be undone:
                 (error "aborting transaction"))))
            (check-equal? (course-value c1) 65432)
            ; Can't delete or save because revision numbers are out of sync:
            (check-exn exn:fail:snooze:revision? (cut save! c1))
            (check-exn exn:fail:snooze:revision? (cut delete! c1))
            ; Have to delete the test record by loading/deleting it:
            (check-not-exn (cut delete! (find-by-id course (struct-id c1)))))))
    
    (test-case "cannot make full continuation jumps into or out of transactions"
      ; General continuation jump out:
      (define _
        (let/cc escape
          (call-with-transaction
           (lambda ()
             (check-exn exn:fail:contract:continuation?
               (lambda ()
                 (escape #f)))))))
      
      ; Escape continuation jump out:
      (define resume
        (check-not-exn
          (lambda ()
            (let/ec escape
              (call-with-transaction
               (lambda ()
                 (let/cc resume
                   (escape resume))))))))
      
      ; General continuation jump in:
      (check-exn exn:fail:contract:continuation?
        (lambda ()
          (resume #f))))
    
    (test-case "transaction-pipeline called"
      (define num-transactions 0)
      
      (define (hook continue conn . args)
        (set! num-transactions (add1 num-transactions))
        (apply continue conn args))
      
      (send (current-snooze) set-transaction-hook! hook)
      
      (delete! (save! (make-person "Dave")))
      
      (check-equal? num-transactions 2))
    
    (test-case "transaction-pipeline aborts transaction before body"
      (define (hook continue conn . args)
        (error "escaping")
        (apply continue conn args))
      
      (send (current-snooze) set-transaction-hook! hook)
      
      (with-handlers ([exn? void])
        (save! (make-person "Dave")))
      
      (check-equal? (length (find-all (sql:select #:from per))) 0))
    
    (test-case "transaction-pipeline aborts transaction after body"
      (define (hook continue conn . args)
        (begin0 (apply continue conn args)
                (error "escaping")))
      
      (send (current-snooze) set-transaction-hook! hook)
      
      (with-handlers ([exn? void])
        (save! (make-person "Dave")))
      
      (check-equal? (length (find-all (sql (select #:from per)))) 0))
    
    (test-case "structs rolled back when transaction aborted"
      (define (hook continue conn . args)
        (apply continue conn args))
      
      (define person (make-person "Dave"))
      
      (send (current-snooze) set-transaction-hook! hook)
      
      (check-not-exn
        (lambda ()
          (let/ec escape
            (call-with-transaction
             (lambda ()
               (set-person-name! person "Noel")
               (save! person)
               (escape #f))))))
      
      (check-equal? (person-name person) "Dave")
      
      (check-not-exn
        (lambda ()
          (let/ec escape
            (set-person-name! person "Matt")
            (save! person)
            (escape #f))))
      
      (check-equal? (person-name person) "Matt")
      
      (delete! person))))

; Provide statements -----------------------------

(provide snooze-transaction-tests)

