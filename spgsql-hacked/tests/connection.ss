;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module connection mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 7))
           (lib "list.ss")
           (lib "class.ss")
           "config.ss"
           "../spgsql.ss")
  (provide query-test)
  
  (define (t? x) (and x #t))
  
  (define query-test
    (test-suite "query API"
      (test-suite "low-level"
        (test-case "query-multiple"
          (call-with-connection
           (lambda (c)
             (let [(q (send c query-multiple
                            (list "select N from the_numbers"
                                  "select 5 as N")))]
               (check-pred list? q)
               (check-equal? 2 (length q))
               (check-true (andmap Recordset? q))
               (check-true (andmap (lambda (r) (list? (Recordset-data r))) q))
               (check-true (andmap (lambda (r) (list? (Recordset-info r))) q))
               (check-true (andmap (lambda (v) (= (vector-length v) 1))
                                   (Recordset-data (car q))))
               (check-equal? (Recordset-info (cadr q))
                             (list (make-FieldInfo "n")))
               (check-equal? (Recordset-data (cadr q))
                             (list (vector 5)))
               (check-true 
                (set-equal? (map car test-data)
                            (map (lambda (v) (vector-ref v 0) )
                                 (Recordset-data (car q)))))))))
        (test-case "query - select"
          (call-with-connection
           (lambda (c)
             (let [(q (send c query "select N from the_numbers"))]
               (check-pred Recordset? q)
               (check-true (set-equal? (map car test-data)
                                       (map (lambda (v) (vector-ref v 0))
                                            (Recordset-data q))))))))
        (test-case "query - update"
          (call-with-connection
           (lambda (c)
             (let [(q (send c query "update the_numbers set N = -1 where N = 1"))]
               (check-pred SimpleResult? q))))))
      (test-suite "high-level"
        (test-case "query-list"
          (call-with-connection
           (lambda (c)
             (let [(q (send c query-list "select N from the_numbers"))]
               (check-pred list? q)
               (check-true (set-equal? q (map car test-data)))))))
        (test-case "query-row"
          (call-with-connection
           (lambda (c)
             (let [(q (send c query-row
                            "select N, description from the_numbers where N = 0"))]
               (check-pred vector? q)
               (check-equal? q 
                             (list->vector (assq 0 test-data)))))))
        (test-case "query-maybe-row - row"
          (call-with-connection
           (lambda (c)
             (let [(q (send c query-maybe-row
                            "select N, description from the_numbers where N = 0"))]
               (check-pred vector? q)
               (check-equal? q 
                             (list->vector (assq 0 test-data)))))))
        (test-case "query-maybe-row - none"
          (call-with-connection
           (lambda (c)
             (let [(q (send c query-maybe-row
                            "select N, description from the_numbers where N < 0"))]
               (check-equal? q #f)))))
        (test-case "query-value"
          (call-with-connection
           (lambda (c)
             (let [(q (send c query-value 
                            "select N from the_numbers where N < 6 and N > 4"))]
               (check-equal? q 5)))))
        (test-case "query-maybe-value - value"
          (call-with-connection
           (lambda (c)
             (let [(q (send c query-maybe-value
                            "select N from the_numbers where N < 6 and N > 4"))]
               (check-equal? q 5)))))
        (test-case "query-maybe-value - none"
          (call-with-connection
           (lambda (c)
             (let [(q (send c query-maybe-value
                            "select N from the_numbers where N > 1000"))]
               (check-equal? q #f)))))
        (test-case "map"
          (call-with-connection
           (lambda (c)
             (let [(q (send c map 
                            "select N, description from the_numbers where N < 2"
                            list))]
               (check-true 
                (set-equal? q
                            (filter (lambda (p) (< (car p) 2)) test-data)))))))
        (test-case "for-each"
          (call-with-connection
           (lambda (c)
             (define a null)
             (let ([q (send c for-each
                            "select N, description from the_numbers where N < 2"
                            (lambda (N description)
                              (set! a (cons (list N description) a))))])
               (check-true 
                (set-equal? a
                            (filter (lambda (p) (< (car p) 2)) test-data)))))))
        (test-case "mapfilter - odds"
          (call-with-connection
           (lambda (c)
             (let [(q (send c mapfilter 
                            "select N, description from the_numbers"
                            list
                            (lambda (n d) (odd? n))))]
               (check-true 
                (set-equal? q
                            (filter (lambda (p) (odd? (car p))) test-data)))))))
        (test-case "fold - sum"
          (call-with-connection 
           (lambda (c)
             (let [(q (send c fold "select N from the_numbers" + 0))]
               (check-equal? q
                             (foldl + 0 (map car test-data)))))))
        (test-case "fold/query-list - sum"
          (call-with-connection
           (lambda (c)
             (let [(q (send c fold "select N from the_numbers" + 0))
                   (q2 (send c query-list "select N from the_numbers"))]
               (check-equal? q (foldl + 0 q2))))))
        (test-case "fold - max"
          (call-with-connection
           (lambda (c)
             (let [(q (send c fold
                            "select N from the_numbers where N > 0 order by N"
                            max -1000))]
               (check-equal? q (foldl max -1000 (map car test-data)))))))
        (test-case "exec - insert"
          (call-with-connection
           (lambda (c)
             (let [(q (send c exec 
                            "insert into the_numbers values(-1, 'mysterious')"))]
               (check-equal? 
                (send c query-value
                      "select description from the_numbers where N = -1")
                "mysterious")))))
        (test-case "exec - delete"
          (call-with-connection
           (lambda (c)
             (let [(q (send c exec "delete from the_numbers where N <> 0"))]
               (check-equal? (send c query-value "select count(*) from the_numbers")
                             1)
               (check-equal? (send c query-list "select N from the_numbers")
                             (list 0)))))))
      
      (test-suite "misc correctness"
        (test-case "noninterference of nested queries"
          (call-with-connection
           (lambda (c)
             (define q
               (send c map
                     "select N from the_numbers where N > 0 and N < 3 order by N"
                     (lambda (a)
                       (send c query-value 
                             (format "select description from the_numbers where N = ~s" a)))))
             (define q2 
               (send c query-list 
                     "select description from the_numbers where N > 0 and N < 3 order by N"))
             (check-equal? q q2))))
        (test-case "continuation safety"
          (call-with-connection
           (lambda (c)
             (let* [(search-id 1)
                    (k1 #f)
                    (k2 #f)
                    (todo (list 
                           (lambda ()
                             (set! search-id 4)
                             (k1 #t))
                           (lambda ()
                             (set! search-id 2)
                             (k2 #t))
                           (lambda ()
                             (set! search-id 6)
                             (k1 #t))
                           (lambda ()
                             (set! search-id 5)
                             (k2 #t))))
                    (q 
                     (let/cc return
                       (send c for-each "select N from the_numbers order by N asc"
                             (lambda (id)
                               (let/cc k
                                 (set! k2 k1)
                                 (printf "saw ~s~n" id)
                                 (when (= id search-id)
                                   (set! k1 k)
                                   (printf "found ~s~n~n" id)
                                   (return id)))))
                       (error 'search-failed "couldn't find ~s" search-id)))]
               (unless (null? todo)
                 (let [(t (car todo))]
                   (set! todo (cdr todo))
                   (t)))))))
        
        ;; Added 18 May 2003: Corrected a bug which incorrectly interleaved
        ;; nulls with returned fields.
        (test-case "nulls arrive in correct order"
          (call-with-connection
           (lambda (c)
             (check-equal? (send c query-row "select null, 1, null")
                           (vector sql-null 1 sql-null))
             (check-equal? (send c query-row "select 1, null")
                           (vector 1 sql-null))
             (check-equal? (send c query-row "select null, 1")
                           (vector sql-null 1))
             (check-equal?
              (send c query-row 
                    "select 1, 2, 3, 4, null, 6, null, 8, 9, 10, 11, 12, null, 14, 15, null, null, 18, 19, 20, null, null, null, null, null, null, 27, 28, 29, 30, null, 32, 33, null, 35")
              (vector 1 2 3 4 sql-null 6 sql-null 8 9 10 11 12 sql-null 14 15 sql-null sql-null 18 19 20 sql-null sql-null sql-null sql-null sql-null sql-null 27 28 29 30 sql-null 32 33 sql-null 35))))))
      
      ;; ERRORS
      (test-suite "Errors"
        (test-suite "low-level"
          (test-case "query-multiple - not a statement list"
            (with-connection c
              (check-exn exn:fail? (lambda () (send c query-multiple 5)))
              (check-exn exn:fail? (lambda () (send c query-multiple "select 5")))
              (check-exn exn:fail? (lambda () (send c query-multiple (list 5))))))
          (test-case "query - not a statement"
            (with-connection c
              (check-exn exn:fail? (lambda () (send c query 5)))))
          (test-case "query - multiple statements in string"
            (with-connection c
              (check-exn exn:fail? (lambda () (send c query "select 3; select 4;")))))
          (test-case "query - unowned prepared stmt"
            (with-connection c1 
              (with-connection c
                (let ([pst (send c1 prepare "select 5")])
                  (check-exn exn:fail? (lambda () (send c bind-prepared-statement pst null)))
                  (let ([stmt (send c1 bind-prepared-statement pst null)])
                    (check-exn exn:fail? (lambda () (send c query stmt))))))))
          (test-case "query errors - nonfatal"
            (with-connection c
              (check-exn exn:fail? (lambda () (send c query-value "select nonsuch")))
              (check-equal? (send c query-value "select 17") 17)))
          )))))
