;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module connection mzscheme
  (require "../spgsql.ss"
           "../private/connection-structures.ss"
           "config.ss"
           (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "list.ss")
           (lib "class.ss"))
  (provide connection-test
           extended-sql-test)

  (define (t? x) (and x #t))
  
  (define connection-test
    (test-suite "SQL - query"
      (test-case "query-general - multiple statements"
        (call-with-connection
         (lambda (c)
           (let [(q (send c query-general 
                          "select N from the_numbers; select null"))]
             (check-pred list? q)
             (check-equal? 2 (length q))
             (check-true (andmap Recordset? q))
             (check-true (andmap (lambda (r) (list? (Recordset-rows r))) q))
             (check-true (andmap (lambda (v) (= (vector-length v) 1)) 
                                  (Recordset-rows (car q))))
             (check-equal? (Recordset-rows (cadr q)) (list (vector sql-null)))
             (check set-equal?
                    '(#"1" #"0" #"2" #"4" #"5" #"6")
                    (map (lambda (v) (vector-ref v 0) )
                         (Recordset-rows (car q))))))))
      (test-case "query-general - copy-in"
        (call-with-connection
         (lambda (c)
           (let [(q (send c query-general "copy the_numbers from stdin"))]
             (check-pred procedure? q)
             (let [(q2 (q (list "17\tmy favorite number" 
                                "18\tyou can fly!")))]
               (check-pred list? q2)
               (check-equal? 1 (length q2))
               (check-pred SimpleQueryResult? (car q2))
               (check-pred (lambda (str) (regexp-match "COPY" str))
                            (SimpleQueryResult-command (car q2))))
             (let [(q3 (send c query-general "select N from the_numbers
                                                           where N > 10"))]
               (check-pred list? q3)
               (check-equal? 1 (length q3))
               (check set-equal?
                      (Recordset-rows (car q3))
                      '(#("17") #("18"))))))))
      (test-case "query - select"
        (call-with-connection
         (lambda (c)
           (let [(q (send c query "select N from the_numbers"))]
             (check-pred Recordset? q)
             (check set-equal?
                    '(#"1" #"0" #"2" #"4" #"5" #"6")
                    (map (lambda (v) (vector-ref v 0))
                         (Recordset-rows q)))))))
      (test-case "query - update"
        (call-with-connection
         (lambda (c)
           (let [(q (send c query "update the_numbers set N = -1 where N = 1"))]
             (check-pred CursorResult? q)))))
      (test-case "query-list - select"
        (call-with-connection
         (lambda (c)
           (let [(q (send c query-list "select N from the_numbers"))]
             (check-pred list? q)
             (check set-equal? q '(#"1" #"0" #"2" #"4" #"5" #"6"))))))
      (test-case "query-tuple - select"
        (call-with-connection
         (lambda (c)
           (let [(q (send c query-tuple
                          "select N, description from the_numbers where N = 0"))]
             (check-pred vector? q)

             (check-equal? q (vector #"0" #"naught"))))))
      (test-case "query-value - select"
        (call-with-connection
         (lambda (c)
           (let [(q (send c query-value 
                          "select N from the_numbers where N < 6 and N > 4"))]
             (check-equal? q #"5")))))
      ;; exec
      (test-case "exec - insert"
        (call-with-connection
         (lambda (c)
           (let [(q (send c exec 
                          "insert into the_numbers values(-1, 'mysterious')"))]
             (check-true q)
             (check-equal? 
              (send c query-value
                    "select description from the_numbers where N = -1")
              #"mysterious")))))
      (test-case "exec - delete"
        (call-with-connection
         (lambda (c)
           (let [(q (send c exec "delete from the_numbers where N <> 0"))]
             (check-true q)
             (check-equal? 
              (send c query-value "select count(*) from the_numbers")
              #"1")
             (check-equal? 
              (send c query-value "select N from the_numbers")
              #"0")))))
      (test-case "mapfilter - odds into pairs"
        (call-with-connection
         (lambda (c)
           (let [(q (send c mapfilter 
                          "select N, description from the_numbers"
                          cons
                          (lambda (n d) (odd? (string->number 
                                               (bytes->string/utf-8 n))))))]
             (check set-equal? q '((#"1" . #"unity") (#"5" . #"five")))))))
      (test-case "fold - sum"
        (call-with-connection 
         (lambda (c)
           (let [(_ (send c use-type-conversions #t))
                 (q (send c fold
                          "select N from the_numbers" + 0))]
             (check-equal? q (+ 1 0 2 4 5 6))))))
      (test-case "fold/query-list - sum again"
        (call-with-connection
         (lambda (c)
           (let* [(_ (send c use-type-conversions #t))
                  (q (send c fold
                           "select N from the_numbers" + 0))
                  (q2 (send c query-list "select N from the_numbers"))]
             (check-equal? q (foldl + 0 q2))))))
      (test-case "fold-right - copy list"
        (call-with-connection 
         (lambda (c)
           (let* [(_ (send c use-type-conversions #t))
                  (q (send c fold-right
                           "select N from the_numbers order by N"
                           (lambda (b a) (cons a b))
                           null))]
             (check-equal? q '(0 1 2 4 5 6))))))
      (test-case "fold-right - check order (least)"
        (call-with-connection
         (lambda (c)
           (let* [(_ (send c use-type-conversions #t))
                  (q (send c fold-right
                           "select N from the_numbers where N > 0 order by N"
                           (lambda (b a) (if (and b (>= b a)) a #f))
                           99))]
             (check-equal? q 1)))))
      (test-case "fold - check order again (greatest)"
        (call-with-connection
         (lambda (c)
           (let* [(_ (send c use-type-conversions #t))
                  (q (send c fold
                           "select N from the_numbers where N > 0 order by N"
                           (lambda (b a) (if (and b (<= b a)) a #f))
                           0))]
             (check-equal? q 6)))))
      (test-case "nested queries"
        (call-with-connection
         (lambda (c)
           (let* [(_ (send c use-type-conversions #t))
                  (q (send c fold-right
                           "select N from the_numbers 
                            where N > 0 and N < 3 order by N"
                           (lambda (b a)
                             (cons 
                              (send 
                               c query-value 
                               (format "select description from the_numbers
                                        where N = ~s"
                                       a))
                              b))
                           null))
                  (q2 (send 
                       c query-list 
                       "select description from the_numbers 
                        where N > 0 and N < 3 order by N"))]
             (check-equal? q q2)))))
      
      (test-case "continuation safety"
        (call-with-connection
         (lambda (c)
           (send c use-type-conversions #t)
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
                     (send c for-each
                           "select N from the_numbers order by N asc"
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
           (send c use-type-conversions #t)
           (check-equal? (send c query-tuple "select null, 1, null")
                          (vector sql-null 1 sql-null))
           (check-equal? (send c query-tuple "select 1, null")
                          (vector 1 sql-null))
           (check-equal? (send c query-tuple "select null, 1")
                          (vector sql-null 1))
           (check-equal? (send c query-tuple 
                                "select 1, 2, 3, 4, null, 6, null, 8, 9, 10, 11, 12, null, 14, 15, null, null, 18, 19, 20, null, null, null, null, null, null, 27, 28, 29, 30, null, 32, 33, null, 35")
                          (vector 1 2 3 4 sql-null 6 sql-null 8 9 10 11 12 sql-null 14 15 sql-null sql-null 18 19 20 sql-null sql-null sql-null sql-null sql-null sql-null 27 28 29 30 sql-null 32 33 sql-null 35)))))
      
      ))
  
  (define extended-sql-test
    (test-case "Connection - extended example"
      (let ([cx (connect-for-test)])
        (define (print-to-err msg)
          (fprintf (current-error-port) "~a" msg))
        (check-true (object? cx))
        (check-true (is-a? cx connection<%>))
        (send cx set-notice-handler print-to-err)
        (send cx set-notification-handler print-to-err)
        (let [(q1 (send cx exec
                        "create temporary table the_numbers 
                         (N integer primary key, description varchar(80))"))]
          (check-true q1))
        (let [(q2 (send cx query 
                        "insert into the_numbers values (1, 'unity')"))
              (q3 (send cx query 
                        "insert into the_numbers (description, N)
                         values ('the loneliest since the number one', 2)"))
              (q4 (send cx query "insert into the_numbers values (0, 'naught')"))]
          (for-each 
           (lambda (q)
             (check-true (CursorResult? q))
             (check-true (t? (regexp-match "INSERT" (CursorResult-command q))))
             (check-true (string=? "blank" (CursorResult-cursor q))))
           (list q2 q3 q4)))
        (let [(q5 (send cx query-general
                        "insert into the_numbers values (4, 'four');
                         insert into the_numbers values (5, 'five');
                         insert into the_numbers values (6, 'seven less 1')"))]
          (check-true (list? q5))
          (check-true (= 3 (length q5)))
          (check-true (andmap CursorResult? q5))
          (check-true 
           (t? (andmap 
                (lambda (q) (regexp-match "INSERT" (CursorResult-command q)))
                q5))))
        (let [(q6 (send cx query-list 
                        "select N from the_numbers where N % 2 = 0 order by N"))]
          (check set-equal? q6 '(#"0" #"2" #"4" #"6")))
        (let [(q7 (send cx query-value 
                        "select description as desc from the_numbers
                         where N = 6"))]
          (check-equal? q7 #"seven less 1"))
        (let [(q8 (send cx query 
                        "update the_numbers set description = 'six' 
                         where N = 6"))]
          (check-true (CursorResult? q8))
          (check-true (t? (regexp-match "UPDATE" (CursorResult-command q8))))
          (check-true (string=? "blank" (CursorResult-cursor q8))))
        (let [(q9 (send cx query 
                        "select description from the_numbers where N = 6"))]
          (check-true (Recordset? q9))
          (check-equal? '("description") (map car (Recordset-fields q9)))
          (check-equal? '(#"six") 
                         (map (lambda (v) (vector-ref v 0))
                              (Recordset-rows q9))))
        (let [(q10 (send cx query 
                         "delete from the_numbers where N = 0 or N = 2"))]
          (check-true (CursorResult? q10))
          (check-true (t? (regexp-match "DELETE" 
                                         (CursorResult-command q10)))))
        (let [(q11 (send cx query-general
                         "select N from the_numbers where N % 2 = 0 order by N;
                          select N from the_numbers where N = 0"))]
          (check-true (list? q11))
          (check-true (= (length q11) 2))
          (check-true (Recordset? (car q11)))
          (check-true (Recordset? (cadr q11)))
          (check set-equal?
                 '(#"4" #"6")
                 (map (lambda (v) (vector-ref v 0))
                      (Recordset-rows (car q11))))
          (check-pred null? (Recordset-rows (cadr q11)))))))
  
  )
