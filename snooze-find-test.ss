#lang scheme/base

(require scheme/match
         (only-in srfi/1 make-list)
         srfi/26
         (planet untyped/unlib:3/gen)
         "snooze-api.ss"
         "test-base.ss"
         "core/core.ss"
         "sql/sql.ss")

; Helper data ------------------------------------
; course
; course
; course
; course
(define-values (c1 c2 c3 c4)
  (values #f #f #f #f))

; entity-alias
(define-alias course2 course)
(define-alias course3 course)

; These tables are used to test that columns are retrieved in the correct order:
(define-entity order-test1 ([col-a integer] [col-b string]) #:table-name 'ordertest)
(define-entity order-test2 ([col-b string] [col-a integer]) #:table-name 'ordertest)

; Tests ------------------------------------------

(define/provide-test-suite snooze-find-tests
  
  (test-suite "basic behaviour with no transaction cache"
    
    (test-case "find-one"
      (recreate-test-tables)
      (let* ([per1 (save! (make-person "Jon"))]
             [per2 (select-one #:from person)])
        (check-equal? per1 per2)
        (check-not-eq? per1 per2)))
    
    (test-case "g:find"
      (recreate-test-tables)
      (let* ([per1  (save! (make-person "Dave"))]
             [per2  (save! (make-person "David"))]
             [per3  (save! (make-person "Noel"))]
             [peeps (g:find (sql (select #:from person #:order ((asc person.name)))))])
        (for ([i (in-naturals)]
              [a (in-gen peeps)]
              [b (in-list (list per1 per2 per3))])
          (with-check-info (['i i])
            (check-not-eq? a b)
            (check-equal? a b)))))
    
    (test-case "find-all"
      (recreate-test-tables)
      (let* ([per1  (save! (make-person "Dave"))]
             [per2  (save! (make-person "David"))]
             [per3  (save! (make-person "Noel"))]
             [peeps (find-all (sql (select #:from person #:order ((asc person.name)))))])
        (for ([i (in-naturals)]
              [a (in-list peeps)]
              [b (in-list (list per1 per2 per3))])
          (with-check-info (['i i])
            (check-not-eq? a b)
            (check-equal? a b)))))
    
    (test-case "cross-referencing"
      (recreate-test-tables)
      (let* ([per1  (save! (make-person "Jon"))]
             [per2  (save! (make-person "Lyman"))]
             [pet1  (save! (make-pet per1 "Garfield"))]
             [pet2  (save! (make-pet per2 "Odie"))]
             [peeps (find-all (sql (select #:from (inner person pet (= person.guid pet.owner)) #:order ((asc person.name)))))])
        (match peeps
          [(list (list per1a pet1a)
                 (list per2a pet2a))
           (check-eq? (vector-ref (struct->vector pet1a) 3) per1a)
           (check-eq? (vector-ref (struct->vector pet2a) 3) per2a)]))))
  
  (test-suite "basic behaviour with transaction cache"
    
    (test-case "find-one"
      (recreate-test-tables)
      (with-transaction #:metadata null
        (let* ([per1 (save! (make-person "Jon"))]
               [per2 (select-one #:from person)])
          (check-eq? per1 per2))))
    
    (test-case "g:find"
      (recreate-test-tables)
      (with-transaction #:metadata null
        (let* ([per1  (save! (make-person "Dave"))]
               [per2  (save! (make-person "David"))]
               [per3  (save! (make-person "Noel"))]
               [peeps (g:find (sql (select #:from person #:order ((asc person.name)))))])
          (for ([i (in-naturals)]
                [a (in-gen peeps)]
                [b (in-list (list per1 per2 per3))])
            (with-check-info (['i i])
              (check-eq? a b))))))
    
    (test-case "find-all"
      (recreate-test-tables)
      (with-transaction #:metadata null
        (let* ([per1  (save! (make-person "Dave"))]
               [per2  (save! (make-person "David"))]
               [per3  (save! (make-person "Noel"))]
               [peeps (find-all (sql (select #:from person #:order ((asc person.name)))))])
          (for ([i (in-naturals)]
                [a (in-list peeps)]
                [b (in-list (list per1 per2 per3))])
            (with-check-info (['i i])
              (check-eq? a b))))))
    
    (test-case "cross-referencing"
      (recreate-test-tables)
      (with-transaction #:metadata null
        (let* ([per1  (save! (make-person "Jon"))]
               [per2  (save! (make-person "Lyman"))]
               [pet1  (save! (make-pet per1 "Garfield"))]
               [pet2  (save! (make-pet per2 "Odie"))]
               [peeps (find-all (sql (select #:from (inner person pet (= person.guid pet.owner)) #:order ((asc person.name)))))])
          (match peeps
            [(list (list per1a pet1a)
                   (list per2a pet2a))
             (check-eq? (vector-ref (struct->vector pet1a) 3) per1a)
             (check-eq? (vector-ref (struct->vector pet2a) 3) per2a)])))))
  
  ; test-suite
  (test-suite "reflection of database"
    
    #:before
    (lambda ()
      (recreate-test-tables)
      (set! c1 (save! (make-course 'course1 "Course 1" 1 0.1 #f (string->time-tai "2001-01-01 01:01:01") #f)))
      (set! c2 (save! (make-course 'course2 "Course 2" 2 0.2 #t (string->time-tai "2002-02-02 02:02:02") #f)))
      (set! c3 (save! (make-course 'course3 "Course 3" 3 0.3 #f (string->time-tai "2003-03-03 03:03:03") #f)))
      (set! c4 (save! (make-course 'course4 "Course 4" 4 0.4 #t (string->time-tai "2004-04-04 04:04:04") #f))))
    
    #:after
    drop-all-tables
        
    (test-case "g:find in multi-item mode"
      (check-equal? (g:collect (g:find (sql (select #:what  (course)
                                                    #:from  course
                                                    #:where (= course.active? #t)
                                                    #:order ((asc course.value))))))
                    (list (list c2) (list c4)))
      (check-equal? (g:collect (g:find (sql (select #:what  (course)
                                                    #:from  course
                                                    #:where (= course.active? #f)
                                                    #:order ((desc course.value))))))
                    (list (list c3) (list c1)))
      (check-equal? (g:collect (g:find (sql (select #:what  (course)
                                                    #:from  course
                                                    #:where (and (= course.active? #t)
                                                                 (= course.active? #f))
                                                    #:order ((desc course.value))))))
                    null))
    
    (test-case "g:find in single-item mode"
      (check-equal? (g:collect (g:find (sql (select #:from  course
                                                    #:where (= course.active? #t)
                                                    #:order ((asc course.value))))))
                    (list c2 c4))
      (check-equal? (g:collect (g:find (sql (select #:from  course
                                                    #:where (= course.active? #f)
                                                    #:order ((desc course.value))))))
                    (list c3 c1))
      (check-equal? (g:collect (g:find (sql (select #:from  course
                                                    #:where (and (= course.active? #f)
                                                                 (= course.active? #t))
                                                    #:order ((desc course.value))))))
                    null))
    
    (test-case "g:find: structs are equal? but not eq? to originals"
      (for ([index    (in-naturals)]
            [actual   (in-list (g:collect (g:find (sql (select #:what  course
                                                               #:from  course
                                                               #:order ((asc course.value)))))))]
            [expected (in-list (list c1 c2 c3 c4))])
        (with-check-info (['index index])
          (check-not-eq? actual expected)
          (check-equal?  actual expected))))
    
    (test-case "find-all"
      (check-equal? (find-all (sql (select #:from  course
                                           #:where (= course.active? #t) 
                                           #:order ((asc course.value)))))
                    (list c2 c4))
      (check-equal? (find-all (sql (select #:from  course
                                           #:where (= course.active? #f)
                                           #:order ((desc course.value)))))
                    (list c3 c1))
      (check-equal? (find-all (sql (select #:from  course
                                           #:where (and (= course.active? #t)
                                                        (= course.active? #f))
                                           #:order ((desc course.value)))))
                    null))
    
    (test-case "find-one"
      (check-equal? (find-one (sql (select #:from  course
                                           #:where (= course.active? #t)
                                           #:order ((asc course.value)))))
                    c2)
      (check-equal? (find-one (sql (select #:from  course
                                           #:where (= course.active? #f)
                                           #:order ((desc course.value)))))
                    c3)
      (check-equal? (find-one (sql (select #:from  course
                                           #:where (and (= course.active? #t)
                                                        (= course.active? #f))
                                           #:order ((desc course.value)))))
                    #f))
    
    (test-case "order of columns : standard find"
      (around (begin (create-table order-test1)
                     (save! (make-order-test1 1 "x"))
                     (save! (make-order-test1 2 "y"))
                     (save! (make-order-test1 3 "z")))
              ; If the columns come out in the wrong order, we'll get a parse exn:
              (begin (check-not-exn find-order-test2s))
              (begin (drop-table order-test1))))
    
    (test-case "order of columns : direct find"
      (let ([ids null])
        (around (begin (create-table order-test1)
                       (set! ids (map snooze-struct-id
                                      (list (save! (make-order-test1 1 "x"))
                                            (save! (make-order-test1 2 "y"))
                                            (save! (make-order-test1 3 "z"))))))
                ; If the columns come out in the wrong order, we'll get a parse exn:
                (begin (check-not-exn
                        (lambda ()
                          (map (cut find-by-id order-test2 <>) ids))))
                (begin (drop-table order-test1)))))
    
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
                    "expressions")))
  
  (test-suite "find-by-{id,guid,guids} and so on"
    
    (test-case "find-by-id"
      (recreate-test-tables)
      (let ([struct (save! (make-person/defaults #:name "Dave"))])
        (check-pred integer? (snooze-struct-id struct))
        (check-equal? (find-by-id person (snooze-struct-id struct)) struct)
        (check-false  (find-by-id person (add1 (snooze-struct-id struct))))
        (check-false  (find-by-id pet (snooze-struct-id struct)))))
    
    (test-case "find-by-guid"
      (recreate-test-tables)
      (let ([struct (save! (make-person/defaults #:name "Dave"))])
        (check-pred database-guid? (snooze-struct-guid struct))
        (check-equal? (find-by-guid (snooze-struct-guid struct)) struct)
        (check-exn exn:fail:contract?
          (lambda ()
            (find-by-guid (entity-make-temporary-guid person))))))
    
    (test-case "find-by-guids"
      (recreate-test-tables)
      (let ([per1 (save! (make-person/defaults #:name "Christian"))]
            [per2 (save! (make-person/defaults #:name "Dave"))]
            [per3 (save! (make-person/defaults #:name "David"))]
            [per4 (save! (make-person/defaults #:name "Matt"))]
            [per5 (save! (make-person/defaults #:name "Noel"))])
        (check-equal? (find-by-guids (map snooze-struct-guid (list per1 per2 per3 per4 per5))) (list per1 per2 per3 per4 per5))
        (check-equal? (find-by-guids (map snooze-struct-guid (list per2 per4 per1 per3 per5))) (list per2 per4 per1 per3 per5))
        (check-equal? (find-by-guids (map snooze-struct-guid (list per1 per1 per1 per2 per2))) (list per1 per1 per1 per2 per2))))
    
    (test-case "load-related!"
      (recreate-test-tables)
      (match-let* ([per1 (save! (make-person/defaults #:name "Christian"))]
                   [per2 (save! (make-person/defaults #:name "Dave"))]
                   [per3 (save! (make-person/defaults #:name "David"))]
                   [per4 (save! (make-person/defaults #:name "Matt"))]
                   [per5 (make-person/defaults #:name "Noel")]
                   [pet1 ((entity-private-constructor pet)
                          (entity-make-temporary-guid pet)
                          #f
                          (snooze-struct-guid per1)
                          "Christian's budgie")]
                   [pet2 ((entity-private-constructor pet)
                          (entity-make-temporary-guid pet)
                          #f
                          (snooze-struct-guid per2)
                          "Dave's dog")]
                   [pet3 (make-pet/defaults #:owner per3                      #:name "David's kitten")]
                   [pet4 (make-pet/defaults #:owner #f                        #:name "Stray goat")]
                   [pet5 (make-pet/defaults #:owner per5                      #:name "Noel's cat")]
                   [pets (list pet1 pet1 pet2 pet3 pet3 pet4 pet5 pet5 pet1)])
        (check-eq? (load-related! pets (attr pet owner)) pets)
        (check-equal? (pet-owner pet1) per1)
        (check-equal? (pet-owner pet2) per2)
        (check-equal? (pet-owner pet3) per3)
        (check-equal? (pet-owner pet4) #f)
        (check-equal? (pet-owner pet5) per5))))

  (test-suite "serializing / deserializing data"
    
    (test-case "backslashes and quotes in strings"
      (recreate-test-tables)
      (for ([num (in-range 0 25)])
        (let ([slashes (apply string (make-list num #\\))]
              [quotes  (apply string (make-list num #\'))])
          (for ([str (in-list (list slashes
                                    quotes
                                    (format "~aa"    slashes)
                                    (format "a~aa"   slashes)
                                    (format "a~a"    slashes)
                                    (format "~aa"    quotes)
                                    (format "a~aa"   quotes)
                                    (format "a\\~a"  quotes)
                                    (format "\\~aa"  quotes)
                                    (format "a\\~aa" quotes)
                                    (format "a\\~a"  quotes)))])
            (with-check-info (['string-data str])
              (check-not-exn
               (lambda ()
                 (with-handlers ([exn? (lambda (exn)
                                         ((error-display-handler) (exn-message exn) exn)
                                         (raise exn))])
                   (let* ([per0 (make-person str)]
                          [per1 (save! per0)]
                          [per2 (find-by-guid (snooze-struct-guid per1))])
                     (check-equal? (person-name per0) str)
                     (check-equal? (person-name per1) str)
                     (check-equal? (person-name per2) str))))))))))))
