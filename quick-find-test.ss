#lang scheme/base

(require srfi/26/cut
         (planet untyped/unlib:3/gen)
         "quick-find.ss"
         "snooze-syntax.ss"
         "test-base.ss"
         "test-data.ss"
         "test-util.ss"
         "era/era.ss"
         "sql/sql.ss")

; Tests ----------------------------------------

; snooze% -> test-suite
(define (make-quick-find-tests snooze)
  (define-snooze-interface snooze)
  
  ; course
  (define-values (c1 c2 c3 c4 c5)
    (values (make-course 'course1 "Course 1" 1 1.1 #f (string->time-tai "2001-01-01 01:01:01"))
            (make-course 'course2 "Course 2" 2 2.2 #t (string->time-tai "2002-02-02 02:02:02"))
            (make-course 'course3 "Course 3" 3 3.3 #f (string->time-tai "2003-03-03 03:03:03"))
            (make-course 'course4 "Course 4" 4 4.4 #t (string->time-tai "2004-04-04 04:04:04"))
            (make-course 'course5 "Course 5" 5 5.5 #f (string->time-tai "2005-05-05 05:05:05"))))
  
  ; #:attr any ... -> (U course #f)
  ; #:attr any ... -> (listof course)
  ; #:attr any ... -> (gen-> course)
  (define-values (find-count-courses find-course find-courses g:courses)
    (values (custom-find-count course snooze #:order ((asc course-value)))
            (custom-find-one   course snooze #:order ((asc course-value)))
            (custom-find-all   course snooze #:order ((asc course-value)))
            (custom-g:find     course snooze #:order ((asc course-value)))))
  
  ; test-suite
  (test-suite "quick-find.ss"
    
    #:before
    (lambda ()
      (create-table entity:course)
      (create-table entity:person)
      (create-table entity:pet)
      ; Save in an odd order:
      (for-each save! (list c1 c2 c3 c4 c5)))
    
    #:after
    (lambda ()
      (drop-table entity:course)
      (drop-table entity:person)
      (drop-table entity:pet))
    
    (test-case "find-count #:active"
      (check-equal? (find-count-courses #:active #t) 2)
      (check-equal? (find-count-courses #:active #f) 3))
    
    (test-case "find-one #:id"
      (check-equal? (find-course #:id #f) #f)
      (check-equal? (find-course #:id 1000) #f)
      (check-equal? (find-course #:id (struct-id c1)) c1)
      (check-equal? (find-course #:id (struct-id c2)) c2))
    
    (test-case "find-all #:active"
      (check-equal? (find-courses #:active #t) (list c2 c4))
      (check-equal? (find-courses #:active #f) (list c1 c3 c5)))
    
    (test-case "g:find #:active"
      (check-equal? (g:collect (g:courses #:active #t)) (list c2 c4))
      (check-equal? (g:collect (g:courses #:active #f)) (list c1 c3 c5)))
    
    (test-case "find-one #:active and #:value"
      (check-equal? (find-course #:active #f #:value 1) c1)
      (check-equal? (find-course #:active #f #:value 2) #f)
      (check-equal? (find-course #:active #f #:value 3) c3)
      (check-equal? (find-course #:active #f #:value 4) #f)
      (check-equal? (find-course #:active #t #:value 1) #f)
      (check-equal? (find-course #:active #t #:value 2) c2)
      (check-equal? (find-course #:active #t #:value 3) #f)
      (check-equal? (find-course #:active #t #:value 4) c4))
    
    (test-case "find-all #:limit and #:offset"
      (check-equal? (find-courses #:limit 1) (list c1))
      (check-equal? (find-courses #:limit 2) (list c1 c2))
      (check-equal? (find-courses #:limit 2 #:offset 1) (list c2 c3)))))

; Provide statements -----------------------------

(provide make-quick-find-tests)

