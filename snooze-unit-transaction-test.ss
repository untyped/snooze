(module snooze-unit-transaction-test mzscheme
  
  (require (lib "unitsig.ss")
           (lib "cut.ss" "srfi" "26"))
  
  (require (file "base.ss")
           (file "era.ss")
           (prefix q: (file "query-lang.ss"))
           (file "snooze-unit.ss")
           (file "snooze-sig.ss")
           (file "test-base.ss")
           (file "test-data.ss")
           (file "test-sig.ss"))
  
  (provide snooze-unit-transaction-tests@)
  
  ;; unit snooze-unit-transaction-tests@ : snooze^ -> test^
  (define snooze-unit-transaction-tests@
    (unit/sig test^
      (import snooze^)
      
      (define course (make-course 'code "Name" 12345 1234.5 #t time1))
      (define course-revision #f) ; Set in #:before procedure below
      
      ;; find-course-by-value : integer -> course
      (define (find-course-by-value val)
        (let ([a (q:entity entity:course)])
          (find-one (q:select #:from  a
                              #:where (q:= (q:attr a 'value) val)))))
      
      (define suite
        (test-suite
         "snooze-unit-transaction-test.ss"
         
         ; ***** NOTE *****
         ; Each test below depends on the tests before it.
         ; Add/edit tests at your peril!
         ; ****************
         
         ;; create test data for transaction tests
         #:before
         (lambda ()
           (check-pred null? 
                       (let ([a (q:entity entity:course)])
                         (find-all (q:select #:from a)))
                       "Test suite precondition failed.")
           (save! course)
           (set! course-revision (get-revision course)))
         
         ;; delete test data from transaction tests
         #:after
         (lambda ()
           (delete! course)
           (check-pred null? 
                       (let ([a (q:entity entity:course)])
                         (find-all (q:select #:from a)))
                       "Test suite postcondition failed."))
         
         (test-case
          "call-with-transaction: transaction gets committed successfully"
          (check-not-false (find-course-by-value 12345) "Precondition failed.")
          (call-with-transaction
              (lambda ()
                (set-course-value! course 54321)
                (save! course)))
          ; Revision number should have increased by 1:
          (check-equal? (get-revision course) (add1 course-revision))
          (check-not-false (find-course-by-value 54321) "Postcondition failed.")
          ; Need to reset the course-revision variable for subsequent tests:
          (set! course-revision (get-revision course)))
         
         (test-case
          "call-with-transaction: transaction gets aborted successfully"
          (check-not-false (find-course-by-value 54321) "Precondition failed.")
          (with-handlers ([exn:snooze? void])
            (call-with-transaction
                (lambda ()
                  (set-course-value! course 12345)
                  (save! course)
                  (raise-exn exn:snooze "Aborting transaction."))))
          (check-equal? (get-revision course) course-revision)
          (check-not-false (find-course-by-value 54321) "Postcondition failed."))
         
         (test-case
          "call-with-transaction: nested transactions get aborted successfully"
          (check-not-false (find-course-by-value 54321) "Precondition failed.")
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
          (check-equal? (get-revision course) course-revision)
          (check-equal? (course-value course) 54321)
          (check-not-false (find-course-by-value 54321) "Postcondition failed."))
         
         (test-case
          "call-with-transaction: inner nested transaction gets aborted successfully (SQLite will fail this test)"
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
                                   (if (find-course-by-value 54321)
                                       "both nested transactions were aborted: this is the expected behaviour for SQLite"
                                       (format "final course value was: ~a"
                                               (course-value (find-by-id entity:course (course-id course))))))))
         
         ; Persistent struct roll back -----------
         
         (test-case
          "call-with-transaction: data attributes and revision number are rolled back (but not too far)"
          (let ([course (make-course 'code "Name" 10000 1234.5 #t time1)])
            (set-course-value! course 12345)
            (save! course)
            (set-course-value! course 23456)
            (check-equal? (get-revision course) 0)
            (check-equal? (course-value course) 23456)
            (with-handlers ([exn:snooze? void]) ; Should roll back to here, where value is 23456
              (call-with-transaction
                  (lambda ()
                    (set-course-value! course 54321)
                    (save! course)
                    (check-equal? (get-revision course) 1)
                    (check-equal? (course-value course) 54321)
                    (raise-exn exn:snooze "Aborting transaction."))))
            (check-equal? (get-revision course) 0)
            (check-equal? (course-value course) 23456)
            (check-not-exn (cut save! course))
            (check-not-exn (cut delete! course))))
         
         (test-case
          "call-with-transaction: repeated assignments to the same field don't interfere with rolling back"
          (let ([course (make-course 'code "Name" 10000 1234.5 #t time1)])
            (save! course)
            (set-course-value! course 12345)
            (set-course-value! course 23456)
            (with-handlers ([exn:snooze? void])
              (call-with-transaction
                  (lambda ()
                    (set-course-value! course 54321)
                    (set-course-value! course 65432)
                    (save! course)
                    (check-equal? (course-value course) 65432)
                    (raise-exn exn:snooze "Aborting transaction."))))
            (check-equal? (course-value course) 23456)
            (check-not-exn (cut save! course))
            (check-not-exn (cut delete! course))))
         
         (test-case
          "call-with-transaction: deltas not stored when roll-back-persistent-structs? set to #f"
          (parameterize ([roll-back-persistent-structs? #f])
            (let ([course (make-course 'code "Name" 10000 1234.5 #t time1)])
              (save! course)
              (set-course-value! course 12345)
              (set-course-value! course 23456)
              (with-handlers ([exn:snooze? void])
                (call-with-transaction
                    (lambda ()
                      (set-course-value! course 54321)
                      (set-course-value! course 65432)
                      (save! course)
                      (check-equal? (course-value course) 65432)
                      ; Changes shouldn't be undone:
                      (raise-exn exn:snooze "Aborting transaction."))))
              (check-equal? (course-value course) 65432)
              ; Can't delete or save because revision numbers are out of sync:
              (check-exn exn:fail:snooze:revision? (cut save! course))
              (check-exn exn:fail:snooze:revision? (cut delete! course))
              ; Have to delete the test record by loading/deleting it:
              (check-not-exn (cut delete! (find-by-id entity:course (get-id course)))))))
         
         (test-case
          "call-with-transaction: cannot set roll-back-persistent-structs? when in a transaction"
          (check-exn 
           exn:fail:snooze:transaction?
           (lambda ()
             (call-with-transaction
                 (lambda ()
                   (parameterize ([roll-back-persistent-structs? #f])
                     (void)))))))
         
         ))
      
      ))
  
  )
 
 