#lang scheme/base

(require scheme/match
         srfi/26
         (planet untyped/unlib:3/gen)
         "snooze-api.ss"
         "test-base.ss"
         "era/era.ss"
         "sql/sql.ss")

; course
; course
; course
; course
(define-values (c1 c2 c3 c4)
  (values #f #f #f #f))

; entity-alias
(define-alias course2 course)
(define-alias course3 course)

; Tests ----------------------------------------

; test-suite
(define snooze-find-tests
  (test-suite "snooze-find-tests"
    
    #:before
    (lambda ()
      (recreate-test-tables/cache)
      (set! c1 (save! (make-course 'course1 "Course 1" 1 1.1 #f (string->time-tai "2001-01-01 01:01:01"))))
      (set! c2 (save! (make-course 'course2 "Course 2" 2 2.2 #t (string->time-tai "2002-02-02 02:02:02"))))
      (set! c3 (save! (make-course 'course3 "Course 3" 3 3.3 #f (string->time-tai "2003-03-03 03:03:03"))))
      (set! c4 (save! (make-course 'course4 "Course 4" 4 4.4 #t (string->time-tai "2004-04-04 04:04:04")))))
    
    #:after
    drop-all-tables
    
    ;(test-case "find-by-id works as expected"
    ;  (check-equal? (find-by-id course 1000) #f)
    ;  (check-equal? (find-by-id course (struct-id c1)) c1)
    ;  (check-equal? (find-by-id course (struct-id c2)) c2))
    
    (test-case "g:find in multi-item mode"
      (check-equal? (g:collect (g:find (sql (select #:what  (course)
                                                    #:from  course
                                                    #:where (= course.active #t)
                                                    #:order ((asc course.value))))))
                    (list (list c2) (list c4)))
      (check-equal? (g:collect (g:find (sql (select #:what  (course)
                                                    #:from  course
                                                    #:where (= course.active #f)
                                                    #:order ((desc course.value))))))
                    (list (list c3) (list c1)))
      (check-equal? (g:collect (g:find (sql (select #:what  (course)
                                                    #:from  course
                                                    #:where (and (= course.active #t)
                                                                 (= course.active #f))
                                                    #:order ((desc course.value))))))
                    null))
        
    (test-case "g:find in single-item mode"
      (check-equal? (g:collect (g:find (sql (select #:from  course
                                                    #:where (= course.active #t)
                                                    #:order ((asc course.value))))))
                    (list c2 c4))
      (check-equal? (g:collect (g:find (sql (select #:from  course
                                                    #:where (= course.active #f)
                                                    #:order ((desc course.value))))))
                    (list c3 c1))
      (check-equal? (g:collect (g:find (sql (select #:from  course
                                                    #:where (and (= course.active #f)
                                                                 (= course.active #t))
                                                    #:order ((desc course.value))))))
                    null))
    
    (test-case "g:find: structs are eq? to cached originals"
      (for ([index    (in-naturals)]
            [actual   (in-list (g:collect (g:find (sql (select #:what  course
                                                               #:from  course
                                                               #:order ((asc course.value)))))))]
            [expected (in-list (list c1 c2 c3 c4))])
        (with-check-info (['index index])
          (check-eq? actual expected))))

    (test-case "find-all"
      (check-equal? (find-all (sql (select #:from  course
                                           #:where (= course.active #t) 
                                           #:order ((asc course.value)))))
                    (list c2 c4))
      (check-equal? (find-all (sql (select #:from  course
                                           #:where (= course.active #f)
                                           #:order ((desc course.value)))))
                    (list c3 c1))
      (check-equal? (find-all (sql (select #:from  course
                                           #:where (and (= course.active #t)
                                                        (= course.active #f))
                                           #:order ((desc course.value)))))
                    null))
    
    (test-case "find-one"
      (check-equal? (find-one (sql (select #:from  course
                                           #:where (= course.active #t)
                                           #:order ((asc course.value)))))
                    c2)
      (check-equal? (find-one (sql (select #:from  course
                                           #:where (= course.active #f)
                                           #:order ((desc course.value)))))
                    c3)
      (check-equal? (find-one (sql (select #:from  course
                                           #:where (and (= course.active #t)
                                                        (= course.active #f))
                                           #:order ((desc course.value)))))
                    #f))
    
    (test-case "expressions in #:what clause"
      (let ([close-enough?
             (match-lambda*
               [(list (list num1 str1) (list num2 str2))
                (and (< (- num1 num2) 0.01) ; these are real numbers so they won't be exactly the same
                     (equal? str1 str2))])])
        (let-alias ([expr1 (sql (max (+ course.value course.rating)))]
                    [expr2 (sql (string-append course.code " " course.name))])
          (check-true
           (andmap close-enough?
                   (find-all (sql (select #:what  (expr1 expr2)
                                          #:from  course
                                          #:order ((desc expr1))
                                          #:group (course.guid
                                                   course.revision
                                                   course.code
                                                   course.name))))
                   (list (list 8.4 "course4 Course 4")
                         (list 6.3 "course3 Course 3")
                         (list 4.2 "course2 Course 2")
                         (list 2.1 "course1 Course 1")))))))
    
    (test-case "#:order with attributes and expressions"
      (let-alias ([expr1 (sql (max (+ course.value course.rating)))]
                  [expr2 (sql (string-append course.code " " course.name))])
        (check-not-exn
          (cut find-all (sql (select #:what  (course.guid expr1 expr2)
                                     #:from  (outer course course2)
                                     #:order ((desc expr1) (asc course.guid) (asc course2.guid))
                                     #:group (course.guid course.revision course.code course.name course2)))))
        (check-not-exn
          (cut find-all (sql (select #:what  expr1
                                     #:from  (select #:what (course.value course.rating) #:from course)
                                     #:order ((desc expr1))))))))
    
    (test-case "order of joins (this will test the special aliasing behaviour of the SQLite back-end)"
      (check-equal? (find-all (sql (select #:from (outer course (outer course2 course3)))))
                    (find-all (sql (select #:from (outer (outer course course2) course3))))
                    "attributes")
      (check-equal? (find-all (sql (select #:what (+ course.revision course2.revision course3.revision)
                                           #:from (outer course (outer course2 course3)))))
                    (find-all (sql (select #:what (+ course.revision course2.revision course3.revision)
                                           #:from (outer (outer course course2) course3))))
                    "expressions"))))

; Provide statements -----------------------------

(provide snooze-find-tests)

