#lang scheme/base

(require srfi/26)

(require "snooze-api.ss"
         "test-base.ss"
         "test-data.ss"
         "era/era.ss")

; Helpers ----------------------------------------

(define time-tai1 (string->time-tai "2001-01-01 01:01:01"))

; Tests ------------------------------------------

; test-suite
(define snooze-revision-tests
  (test-suite "snooze-revision-tests"
    
    #:before
    recreate-test-tables
    
    #:after
    drop-all-tables
    
    (test-case "revision is #f before struct is saved for the first time"
      (let ([course (make-course 'code "Name" 12345 1234.5 #t time-tai1)])
        (check-equal? (struct-revision course) #f)))
    
    (test-case "revision is set to 0 when struct is saved for the first time"
      (let ([course (make-course 'code "Name" 12345 1234.5 #t time-tai1)])
        (check-equal? (struct-revision course) #f)
        (save! course)
        (check-equal? (struct-revision course) 0)
        (delete! course)))
    
    (test-case "revision is incremented when struct is (re-)saved"
      (let ([course (make-course 'code "Name" 12345 1234.5 #t time-tai1)])
        (check-equal? (struct-revision course) #f)
        (save! course)
        (check-equal? (struct-revision course) 0)
        (save! course)
        (check-equal? (struct-revision course) 1)
        (save! course)
        (check-equal? (struct-revision course) 2)
        (delete! course)))
    
    (test-case "save! throws an exception when revision is incorrect"
      (let ([course1 (make-course 'code "Name" 12345 1234.5 #t time-tai1)])
        (save! course1)
        (let ([course2 (course-set course1)])
          (check-equal? (struct-revision course1) (struct-revision course2))
          (save! course1)
          (check-equal? (struct-revision course1) (add1 (struct-revision course2)))
          (check-exn exn:fail:snooze:revision? (cut save! course2))
          (check-not-exn (cut save! course1)))
        (delete! course1)))
    
    (test-case "delete! throws an exception when revision is incorrect"
      (let ([course1 (make-course 'code "Name" 12345 1234.5 #t time-tai1)])
        (save! course1)
        (let ([course2 (course-set course1)])
          (check-equal? (struct-revision course1) (struct-revision course2))
          (save! course1)
          (check-equal? (struct-revision course1) (add1 (struct-revision course2)))
          (check-exn exn:fail:snooze:revision? (cut delete! course2))
          (check-not-exn (cut delete! course1)))))))

; Provide statements -----------------------------

(provide snooze-revision-tests)
