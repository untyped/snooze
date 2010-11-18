#lang scheme/base

(require srfi/26/cut)

(require "snooze-syntax.ss"
         "test-base.ss"
         "test-data.ss"
         "test-util.ss"
         "era/era.ss")

(provide make-snooze-revision-tests)

; Helpers ----------------------------------------

(define time-tai1 (string->time-tai "2001-01-01 01:01:01"))

; Tests ------------------------------------------

; snooze% -> test-suite
(define (make-snooze-revision-tests snooze)
  (define-snooze-interface snooze)
  ; test-suite
  (test-suite "snooze-revision-tests"
    
    #:before
    (lambda ()
      (create-table entity:course))
    
    #:after
    (lambda ()
      (drop-table entity:course))
    
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
        (let ([course2 (find-by-id entity:course (struct-id course1))])
          (check-equal? (struct-revision course1) (struct-revision course2))
          (save! course1)
          (check-equal? (struct-revision course1) (add1 (struct-revision course2)))
          (check-exn exn:fail:snooze:revision? (cut save! course2))
          (check-not-exn (cut save! course1)))
        (delete! course1)))
    
    (test-case "delete! throws an exception when revision is incorrect"
      (let ([course1 (make-course 'code "Name" 12345 1234.5 #t time-tai1)])
        (save! course1)
        (let ([course2 (find-by-id entity:course (struct-id course1))])
          (check-equal? (struct-revision course1) (struct-revision course2))
          (save! course1)
          (check-equal? (struct-revision course1) (add1 (struct-revision course2)))
          (check-exn exn:fail:snooze:revision? (cut delete! course2))
          (check-not-exn (cut delete! course1)))))
    
    ))