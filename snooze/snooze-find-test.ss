#lang scheme/base

(require scheme/match
         srfi/26
         (planet untyped/unlib:3/gen)
         "snooze-syntax.ss"
         "test-base.ss"
         "test-data.ss"
         "test-util.ss"
         "era/era.ss"
         "sql/sql.ss")

; Tests ----------------------------------------

; snooze% -> test-suite
(define (make-snooze-find-tests snooze)
  (define-snooze-interface snooze)
  
  ; course
  ; course
  ; course
  ; course
  (define-values (c1 c2 c3 c4)
    (values (make-course 'course1 "Course 1" 1 1.1 #f (string->time-tai "2001-01-01 01:01:01"))
            (make-course 'course2 "Course 2" 2 2.2 #t (string->time-tai "2002-02-02 02:02:02"))
            (make-course 'course3 "Course 3" 3 3.3 #f (string->time-tai "2003-03-03 03:03:03"))
            (make-course 'course4 "Course 4" 4 4.4 #t (string->time-tai "2004-04-04 04:04:04"))))
  
  ; sql:entity
  (define-alias a course)
  (define-alias b course)
  (define-alias c course)
  
  ; test-suite
  (test-suite "snooze-find-tests"
    
    #:before
    (lambda ()
      (create-table entity:course)
      (create-table entity:person)
      (create-table entity:pet)
      (save! c1)
      (save! c2)
      (save! c3)
      (save! c4))
    
    #:after
    (lambda ()
      (drop-table entity:course)
      (drop-table entity:person)
      (drop-table entity:pet))
    
    (test-case "find-by-id works as expected"
      (check-equal? (find-by-id entity:course 1000) #f)
      (check-equal? (find-by-id entity:course (struct-id c1)) c1)
      (check-equal? (find-by-id entity:course (struct-id c2)) c2))
    
    (test-case "g:find in multi-item mode"
      (check-equal? (g:collect (g:find (sql:select #:what  (list a)
                                                   #:from  a
                                                   #:where (sql:= a-active #t)
                                                   #:order (list (sql:asc a-value)))))
                    (list (list c2) (list c4))
                    "check 1 - active = #t")
      (check-equal? (g:collect (g:find (sql:select #:what  (list a)
                                                   #:from  a
                                                   #:where (sql:= a-active #f)
                                                   #:order (list (sql:desc a-value)))))
                    (list (list c3) (list c1))
                    "check 2 - active = #f")
      (check-equal? (g:collect (g:find (sql:select #:what  (list a)
                                                   #:from  a
                                                   #:where (sql:and (sql:= a-active #t)
                                                                    (sql:= a-active #f))
                                                   #:order (list (sql:desc a-value)))))
                    (list)
                    "check 3 - active = #t and active = #f (i.e. no results returned)"))
    
    (test-case "g:find in single-item mode"
      (check-equal? (g:collect (g:find (sql:select #:from  a
                                                   #:where (sql:= a-active #t)
                                                   #:order (list (sql:asc a-value)))))
                    (list c2 c4)
                    "check 1 - active = #t")
      (check-equal? (g:collect (g:find (sql:select #:from  a
                                                   #:where (sql:= a-active #f)
                                                   #:order (list (sql:desc a-value)))))
                    (list c3 c1)
                    "check 2 - active = #f")
      (check-equal? (g:collect (g:find (sql:select #:what  a
                                                   #:from  a
                                                   #:where (sql:and (sql:= a-active #f) (sql:= a-active #t))
                                                   #:order (list (sql:desc a-value)))))
                    (list)
                    "check 3 - active = #t and active = #f (i.e. no results returned)"))
    
    (test-case "find-all"
      (check-equal? (find-all (sql:select #:from  a
                                          #:where (sql:= a-active #t)
                                          #:order (list (sql:asc a-value))))
                    (list c2 c4)
                    "check 1 - active = #t")
      (check-equal? (find-all (sql:select #:from  a
                                          #:where (sql:= a-active #f)
                                          #:order (list (sql:desc a-value))))
                    (list c3 c1)
                    "check 2 - active = #f")
      (check-equal? (find-all (sql:select #:from  a
                                          #:where (sql:and (sql:= a-active #t)
                                                           (sql:= a-active #f))
                                          #:order (list (sql:desc a-value))))
                    (list)
                    "check 3 - active = #t and active = #f (i.e. no results returned)"))
    
    (test-case "find-one"
      (check-equal? (find-one (sql:select #:from  a
                                          #:where (sql:= a-active #t)
                                          #:order (list (sql:asc a-value))))
                    c2
                    "check 1 - active = #t")
      (check-equal? (find-one (sql:select #:from  a
                                          #:where (sql:= a-active #f)
                                          #:order (list (sql:desc a-value))))
                    c3
                    "check 2 - active = #f")
      (check-equal? (find-one (sql:select #:from  a
                                          #:where (sql:and (sql:= a-active #t)
                                                           (sql:= a-active #f))
                                          #:order (list (sql:desc a-value))))
                    #f
                    "check 3 - active = #t and active = #f (i.e. no results returned)"))
    
    (test-case "expressions in #:what clause"
      (let ([close-enough?
             (match-lambda*
               [(list (list num1 str1) (list num2 str2))
                (and (< (- num1 num2) 0.01) ; these are real numbers so they won't be exactly the same
                     (equal? str1 str2))])])
        (let-alias ([expr1 (sql:max (sql:+ a-value a-rating))]
                    [expr2 (sql:string-append a-code " " a-name)])
          (check-true (andmap close-enough?
                              (find-all (sql:select #:what  (list expr1 expr2)
                                                    #:from  a
                                                    #:order (list (sql:desc expr1))
                                                    #:group (list a-id a-revision a-code a-name)))
                              (list (list 8.4 "course4 Course 4")
                                    (list 6.3 "course3 Course 3")
                                    (list 4.2 "course2 Course 2")
                                    (list 2.1 "course1 Course 1")))))))
    
    (test-case "#:order with attributes and expressions"
      (let-alias ([expr1 (sql:max (sql:+ a-value a-rating))]
                  [expr2 (sql:string-append a-code " " a-name)])
        (check-not-exn
          (cut find-all (sql:select #:what  (list a-id expr1 expr2)
                                    #:from  (sql:outer a b)
                                    #:order (list (sql:desc expr1) (sql:asc a-id) (sql:asc b-id))
                                    #:group (list a-id a-revision a-code a-name b))))
        (check-not-exn
          (cut find-all (sql:select #:what  expr1
                                    #:from  (sql:select #:what (list a-value a-rating) #:from a)
                                    #:order (list (sql:desc expr1)))))))
    
    (test-case "order of joins (this will test the special aliasing behaviour of the SQLite back-end)"
      (check-equal? (find-all (sql (select #:from (outer a (outer b c)))))
                    (find-all (sql (select #:from (outer (outer a b) c))))
                    "attributes")
      (check-equal? (find-all (sql (select #:what (+ a-id b-id c-id) #:from (outer a (outer b c)))))
                    (find-all (sql (select #:what (+ a-id b-id c-id) #:from (outer (outer a b) c))))
                    "expressions"))))

; Provide statements -----------------------------

(provide make-snooze-find-tests)
