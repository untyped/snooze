#lang scheme/base

(require srfi/26/cut
         (planet untyped/unlib:3/gen)
         "quick-find.ss"
         "snooze-api.ss"
         "test-base.ss"
         "core/era.ss"
         "sql/sql.ss")

; Tests ----------------------------------------

; course
(define-values (c1 c2 c3 c4 c5)
  (values #f #f #f #f #f))

; #:attr any ... -> (U course #f)
; #:attr any ... -> (listof course)
; #:attr any ... -> (gen-> course)
(define-values (find-count-courses find-course find-courses g:courses)
  (values (custom-find-count course #:order ((asc course.value)))
          (custom-find-one   course #:order ((asc course.value)))
          (custom-find-all   course #:order ((asc course.value)))
          (custom-g:find     course #:order ((asc course.value)))))

; test-suite
(define quick-find-tests
  (test-suite "quick-find.ss"
    
    #:before
    (lambda ()
      (recreate-test-tables)
      (set! c1 (save! (make-course 'course1 "Course 1" 1 1.1 #f (string->time-tai "2001-01-01 01:01:01"))))
      (set! c2 (save! (make-course 'course2 "Course 2" 2 2.2 #t (string->time-tai "2002-02-02 02:02:02"))))
      (set! c3 (save! (make-course 'course3 "Course 3" 3 3.3 #f (string->time-tai "2003-03-03 03:03:03"))))
      (set! c4 (save! (make-course 'course4 "Course 4" 4 4.4 #t (string->time-tai "2004-04-04 04:04:04"))))
      (set! c5 (save! (make-course 'course5 "Course 5" 5 5.5 #f (string->time-tai "2005-05-05 05:05:05")))))
    
    #:after
    drop-all-tables
    
    (test-case "find-count #:active?"
      (check-equal? (find-count-courses #:active? #t) 2)
      (check-equal? (find-count-courses #:active? #f) 3))
    
    (test-case "find-one #:guid"
      (check-equal? (find-course #:guid #f) #f)
      (check-equal? (find-course #:guid c1) c1)
      (check-equal? (find-course #:guid c2) c2))
    
    (test-case "find-all #:active?"
      (check-equal? (find-courses #:active? #t) (list c2 c4))
      (check-equal? (find-courses #:active? #f) (list c1 c3 c5)))
    
    (test-case "g:find #:active?"
      (check-equal? (g:collect (g:courses #:active? #t)) (list c2 c4))
      (check-equal? (g:collect (g:courses #:active? #f)) (list c1 c3 c5)))
    
    (test-case "find-one #:active? and #:value"
      (check-equal? (find-course #:active? #f #:value 1) c1)
      (check-equal? (find-course #:active? #f #:value 2) #f)
      (check-equal? (find-course #:active? #f #:value 3) c3)
      (check-equal? (find-course #:active? #f #:value 4) #f)
      (check-equal? (find-course #:active? #t #:value 1) #f)
      (check-equal? (find-course #:active? #t #:value 2) c2)
      (check-equal? (find-course #:active? #t #:value 3) #f)
      (check-equal? (find-course #:active? #t #:value 4) c4))
    
    (test-case "find-all #:limit and #:offset"
      (check-equal? (find-courses #:limit 1) (list c1))
      (check-equal? (find-courses #:limit 2) (list c1 c2))
      (check-equal? (find-courses #:limit 2 #:offset 1) (list c2 c3)))))

; Provide statements -----------------------------

(provide quick-find-tests)

