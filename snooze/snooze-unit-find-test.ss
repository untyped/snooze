(module snooze-unit-find-test mzscheme
  
  (require (lib "unitsig.ss")
           (lib "list.ss" "srfi" "1"))
  
  (require (planet "gen.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "era.ss")
           (prefix q: (file "query-lang.ss"))
           (file "snooze-unit.ss")
           (file "snooze-sig.ss")
           (file "test-base.ss")
           (file "test-data.ss")
           (file "test-sig.ss"))
  
  (provide snooze-unit-find-tests@)
  
  (define c1 (make-course 'course1 "Course 1" 1 1.1 #f (string->time-tai "2001-01-01 01:01:01")))
  (define c2 (make-course 'course2 "Course 2" 2 2.2 #t (string->time-tai "2002-02-02 02:02:02")))
  (define c3 (make-course 'course3 "Course 3" 3 3.3 #f (string->time-tai "2003-03-03 03:03:03")))
  (define c4 (make-course 'course4 "Course 4" 4 4.4 #t (string->time-tai "2004-04-04 04:04:04")))
  
  ;; unit snooze-unit-find-tests@ : snooze^ -> test^
  (define snooze-unit-find-tests@
    (unit/sig test^
      (import snooze^)
      
      (define suite
        (test-suite
         "snooze-unit-find-test.ss"
         
         #:before
         ;;add test data to database
         (lambda ()
           (save! c1)
           (save! c2)
           (save! c3)
           (save! c4))
         
         #:after
         ;; remove test data from database"
         (lambda ()
           (delete! c1)
           (delete! c2)
           (delete! c3)
           (delete! c4))
         
         ;(test-case
         ; "find-count works as expected"
         ; (check-equal? 
         ;  (find-count (s:select (s:alias 'a entity:course)))
         ;  4)
         ; (check-equal? 
         ;  (find-count (s:select (s:alias 'a entity:course)
         ;                        (s:where (s:= (s:ref 'a 'value) 2))))
         ;  1))
         
         ;(test-case
         ; "find-all selects things in different orders"
         ; (check-equal? 
         ;  (find-all (s:select (s:alias 'a entity:course)
         ;                      (s:order (s:asc 'a 'value))))
         ;  (list c1 c2 c3 c4))
         ; (check-equal? 
         ;  (find-all (s:select (s:alias 'a entity:course)
         ;                      (s:order (s:desc 'a 'value))))
         ;  (list c4 c3 c2 c1)))
         
         ;(test-case
         ; "find-all selects things using where statements"
         ; (check-equal? 
         ;  (find-all (s:select (s:alias 'a entity:course)
         ;                      (s:where (s:= (s:ref 'a 'active) #t))
         ;                      (s:order (s:asc 'a 'value))))
         ;  (list c2 c4))
         ; (check-equal? 
         ;  (find-all (s:select (s:alias 'a entity:course)
         ;                      (s:where (s:= (s:ref 'a 'active) #f))
         ;                      (s:order (s:desc 'a 'value))))
         ;  (list c3 c1)))
         
         ;(test-case
         ; "find-one selects an existing record"
         ; (let ([do-find
         ;        (lambda (val)
         ;          (find-one (s:select (s:alias 'a entity:course)
         ;                              (s:where (s:= (s:ref 'a 'value) val)))))])
         ;   (check-equal? 
         ;    (map do-find '(1 2 3 4 5))
         ;    (list c1 c2 c3 c4 #f))))
         
         ;(test-case
         ; "find-one returns #f for a non-existant record"
         ; (check-equal? 
         ;  (find-one (s:select (s:alias 'a entity:course)
         ;                      (s:where (s:= (s:ref 'a 'value) 5))))
         ;  #f))
         
         ;(test-case
         ; "find-one selects an existing record by exact date"
         ; (let ([do-find
         ;        (lambda (val)
         ;          (find-one (s:select (s:alias 'a entity:course)
         ;                              (s:where (s:= (s:ref 'a 'start) 
         ;                                            (string->time-tai (format "200~a-0~a-0~a 0~a:0~a:0~a"
         ;                                                                      val val val val val val)))))))])
         ;   (check-equal? 
         ;    (map do-find '(1 2 3 4 5))
         ;    (list c1 c2 c3 c4 #f))))
         
         ;(test-case
         ; "find-all selects an existing record by relative date"
         ; (let ([do-find
         ;        (lambda (val)
         ;          (find-all (s:select (s:alias 'a entity:course)
         ;                              (s:where (s:< (s:ref 'a 'start) 
         ;                                            (string->time-tai (format "200~a-0~a-0~a 0~a:0~a:0~a"
         ;                                                                      val val val val val val)))))))])
         ;   (check-equal? 
         ;    (map do-find '(1 2 3 4 5))
         ;    (list null (list c1) (list c1 c2) (list c1 c2 c3) (list c1 c2 c3 c4)))))
         
         (test-case
          "find-gen works as expected (multi-item mode)"
          (check-equal? 
           (g:collect (find-gen (let ([a (q:entity entity:course)])
                                  (q:select #:what  (list a)
                                            #:from  a
                                            #:where (q:= (q:attr a 'active) #t)
                                            #:order (list (q:asc (q:attr a 'value)))))))
           (list (list c2) (list c4))
           "active #t")
          (check-equal? 
           (g:collect (find-gen (let ([a (q:entity entity:course)])
                                  (q:select #:what  (list a)
                                            #:from  a
                                            #:where (q:= (q:attr a 'active) #f)
                                            #:order (list (q:desc (q:attr a 'value)))))))
           (list (list c3) (list c1))
           "active #f")
          (check-equal? 
           (g:collect (find-gen (let ([a (q:entity entity:course)])
                                  (q:select #:what  (list a)
                                            #:from  a
                                            #:where (q:and (q:= (q:attr a 'active) #f) (q:= (q:attr a 'active) #t))
                                            #:order (list (q:desc (q:attr a 'value)))))))
           (list)
           "active #t and #f (!!)"))
         
         (test-case
          "find-gen works as expected (single-item mode)"
          (check-equal? 
           (g:collect (find-gen (let ([a (q:entity entity:course)])
                                  (q:select #:what  a
                                            #:from  a
                                            #:where (q:= (q:attr a 'active) #t)
                                            #:order (list (q:asc (q:attr a 'value)))))))
           (list c2 c4)
           "active #t")
          (check-equal? 
           (g:collect (find-gen (let ([a (q:entity entity:course)])
                                  (q:select #:what  a
                                            #:from  a
                                            #:where (q:= (q:attr a 'active) #f)
                                            #:order (list (q:desc (q:attr a 'value)))))))
           (list c3 c1)
           "active #f")
          (check-equal? 
           (g:collect (find-gen (let ([a (q:entity entity:course)])
                                  (q:select #:what  a
                                            #:from  a
                                            #:where (q:and (q:= (q:attr a 'active) #f) (q:= (q:attr a 'active) #t))
                                            #:order (list (q:desc (q:attr a 'value)))))))
           (list)
           "active #t and #f (!!)"))
         
         (test-case
          "find-by-id works as expected"
          (check-equal? (find-by-id entity:course 1000) #f)
          (check-equal? (find-by-id entity:course (get-id c1)) c1)
          (check-equal? (find-by-id entity:course (get-id c2)) c2))
         
         ))
      
      ))
  
  )
 
 