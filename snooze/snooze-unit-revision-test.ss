(module snooze-unit-revision-test mzscheme
  
  (require (lib "unitsig.ss")
           (lib "cut.ss" "srfi" "26"))
  
  (require (file "era.ss")
           (file "snooze-unit.ss")
           (file "snooze-sig.ss")
           (file "test-base.ss")
           (file "test-data.ss")
           (file "test-sig.ss"))
  
  (provide snooze-unit-revision-tests@)
  
  ;; unit snooze-unit-revision-tests@ : snooze^ -> test^
  (define snooze-unit-revision-tests@
    (unit/sig test^
      (import snooze^)
      
      (define suite
        (test-suite
         "snooze-unit-revision-test.ss"
         
         (test-case
          "revision is #f before struct is saved for the first time"
          (let ([course (make-course 'code "Name" 12345 1234.5 #t time1)])
            (check-equal? (get-revision course) #f)))
         
         (test-case
          "revision is set to 0 when struct is saved for the first time"
          (let ([course (make-course 'code "Name" 12345 1234.5 #t time1)])
            (check-equal? (get-revision course) #f)
            (save! course)
            (check-equal? (get-revision course) 0)
            (delete! course)))
         
         (test-case
          "revision is incremented when struct is (re-)saved"
          (let ([course (make-course 'code "Name" 12345 1234.5 #t time1)])
            (check-equal? (get-revision course) #f)
            (save! course)
            (check-equal? (get-revision course) 0)
            (save! course)
            (check-equal? (get-revision course) 1)
            (save! course)
            (check-equal? (get-revision course) 2)
            (delete! course)))
         
         (test-case
          "save! throws an exception when revision is incorrect"
          (let ([course1 (make-course 'code "Name" 12345 1234.5 #t time1)])
            (save! course1)
            (let ([course2 (find-by-id entity:course (get-id course1))])
              (check-equal? (get-revision course1) (get-revision course2))
              (save! course1)
              (check-equal? (get-revision course1) (add1 (get-revision course2)))
              (check-exn exn:fail:snooze:revision? (cut save! course2))
              (check-not-exn (cut save! course1)))
            (delete! course1)))
         
         (test-case
          "delete! throws an exception when revision is incorrect"
          (let ([course1 (make-course 'code "Name" 12345 1234.5 #t time1)])
            (save! course1)
            (let ([course2 (find-by-id entity:course (get-id course1))])
              (check-equal? (get-revision course1) (get-revision course2))
              (save! course1)
              (check-equal? (get-revision course1) (add1 (get-revision course2)))
              (check-exn exn:fail:snooze:revision? (cut delete! course2))
              (check-not-exn (cut delete! course1)))))
         
         ))
      
      ))
  
  )
 
