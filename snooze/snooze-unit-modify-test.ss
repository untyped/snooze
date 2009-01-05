(module snooze-unit-modify-test mzscheme
  
  (require (lib "unitsig.ss"))
  
  (require (file "era.ss")
           (file "snooze-unit.ss")
           (file "snooze-sig.ss")
           (file "test-base.ss")
           (file "test-data.ss")
           (file "test-sig.ss"))
  
  (provide snooze-unit-modify-tests@)
  
  ;; unit snooze-unit-modify-tests@ : snooze^ -> test^
  (define snooze-unit-modify-tests@
    (unit/sig test^
      (import snooze^)
      
      (define course (make-course 'code "Name" 12345 1234.5 #t (string->time-tai "2001-01-01 01:01:01")))
      
      (define suite
        (test-suite
         "snooze-unit-modify-test.ss"

         ; ***** NOTE *****
         ; Each test below depends on the tests before it.
         ; Add/edit tests at your peril!
         ; ****************
         
         (test-case
          "new struct has #f id"
          (check-false (get-id course)))
         
         (test-case
          "first save! inserts a database record, updates id in struct, and returns the new id"
          (let ([return-value (save! course)])
            (check-pred integer? (get-id course))
            (check-equal? (find-by-id entity:course (get-id course)) course)
            (check-equal? return-value (get-id course))))
         
         (test-case
          "subsequent save! updates database record and returns the struct id"
          (set-course-value! course 54321)
          (let ([return-value (save! course)])
            (check-equal? (course-value (find-by-id entity:course (get-id course))) 54321)
            (check-equal? return-value (get-id course))))
         
         (test-case
          "delete! removes database record and sets struct id to #f"
          (let ([old-id (get-id course)])
            (delete! course)
            (check-false (get-id course))
            (check-false (find-by-id entity:course old-id))))
         
         (test-case
          "delete! of unsaved record raises exception"
          (check-exn exn:fail:snooze? (lambda () (delete! course))))
         
         ))
      
      ))
  
  )
 
