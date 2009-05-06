#lang scheme/base

(require srfi/26
         "snooze-api.ss"
         "test-base.ss"
         "test-data.ss"
         "era/era.ss")

; Tests ------------------------------------------

; course
(define c1 #f)

; test-suite
(define snooze-modify-tests
  (test-suite "snooze-modify-tests"
    
    #:before
    (lambda ()
      (recreate-test-tables)
      (set! c1 (make-course 'code "Name" 12345 1234.5 #t (string->time-tai "2001-01-01 01:01:01"))))
    
    #:after
    drop-all-tables
    
    ; ***** NOTE *****
    ; Each test below depends on the tests before it.
    ; Add/edit tests at your peril!
    ; ****************
    
    (test-case "new struct has #f id"
      (check-false (struct-id c1)))
    
    (test-case "first save! inserts a database record, updates id in struct, and returns the struct"
      (let ([return-value (save! c1)])
        (check-pred integer? (struct-id c1))
        ; (check-equal? (find-by-id course (struct-id c1)) c1)
        (fail "need way of bypassing cache")
        (check-equal? return-value c1)))
    
    (test-case "subsequent save! updates database record and returns the struct id"
      (set-course-value! c1 54321)
      (let ([return-value (save! c1)])
        ;(check-equal? (course-value (find-by-id course (struct-id c1))) 54321)
        (fail "need way of bypassing cache")
        (check-equal? return-value c1)))
    
    (test-case "delete! removes database record and sets struct id to #f"
      (let ([old-id (struct-id c1)])
        (delete! c1)
        (check-false (struct-id c1))
        ;(check-false (find-by-id course old-id))
        (fail "need way of bypassing cache")))
    
    (test-case "delete! of unsaved record raises exception"
      (check-exn exn:fail? (cut delete! c1)))))

; Provide statements -----------------------------

(provide snooze-modify-tests)
