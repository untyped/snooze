;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module config mzscheme
  (require "../spgsql.ss"
           (lib "class.ss"))

  (provide connect-for-test
           connect-and-setup
           call-with-connection
           with-connection
           testing-connection-mixin
           
           test-data
           set-equal?)

  (define testing-connection-mixin (make-parameter values))
  
  (define (connect-for-test)
    (connect #:user "ryan"
             #:database "ryan"
             #:password (getenv "PGPASSWORD")
             #:mixin (testing-connection-mixin)))
  
  (define test-data
    '((0 "nothing")
      (1 "unity")
      (2 "the loneliest number since the number one")
      (4 "four")
      (5 "five")
      (6 "half a dozen")))
  
  (define (connect-and-setup)
    (let [(cx (connect-for-test))]
      (send cx exec "create temporary table the_numbers (N integer primary key, description text)")
      (for-each (lambda (p)
                  (send cx exec (format "insert into the_numbers values (~a, '~a')"
                                        (car p) (cadr p))))
                test-data)
      cx))
  
;        (exec "insert into the_numbers values (1, 'unity')")
;        (exec "insert into the_numbers (description, N)
;               values ('the loneliest number since the number one', 2)")
;        (exec "insert into the_numbers values (0, 'naught')")
;        (exec "insert into the_numbers values (4, 'four')")
;        (exec "insert into the_numbers values (5, 'five')")
;        (exec "insert into the_numbers values (6, 'seven less 1')"))

  ;; set-equal? : ('a list) ('a list) -> boolean
  (define (set-equal? a b)
    (and (andmap (lambda (xa) (member xa b)) a)
         (andmap (lambda (xb) (member xb a)) b)
         #t))

  (define (call-with-connection f)
    (let [(c (connect-and-setup))]
      (dynamic-wind void
                    (lambda () (f c))
                    (lambda () (send c disconnect)))))
  
  (define-syntax with-connection
    (syntax-rules ()
      [(with-connection c . body)
       (call-with-connection (lambda (c) . body))]))
  
  )
