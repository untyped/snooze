;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module exceptions mzscheme
  (require "../spgsql.ss"
           (rename "../private/connection.ss" connection% connection%)
           "config.ss")
  (require (lib "class.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide exception-test)

  (define BAD-PORT 5555)
  (define BAD-USER "johndoe")
  (define BAD-DATABASE "invaliddb")
  (define BAD-PASSWORD "snark")

  ;; NOTE:
  ;; These tests are currently very brittle. This should improve once the
  ;; new SQL error code gets implemented.

  (define exception-test
    (test-suite
     "Exceptions"
     (test-suite
      "connect"

      (test-case
       "Control"
       (let [(c (connect HOST PORT DATABASE USER PASSWORD))]
         (check-pred object? c)
         (check-true (is-a? c connection<%>))))

      (test-case
       "Bad user"
       (check-exn (pg-error? "Could not connect to server")
                  (lambda () (connect HOST BAD-PORT DATABASE USER PASSWORD))))

      (test-case
       "Bad database"
       (check-exn (pg-error? "Error after authentication")
                  (lambda () (connect HOST PORT BAD-DATABASE USER PASSWORD))))

      (test-case
       "Bad user"
       (check-exn
        (pg-error? "Authentication failed")
        (lambda () (connect HOST PORT DATABASE BAD-USER PASSWORD))))

      (test-case
       "Bad password"
       (check-exn (pg-error? "")
                  (lambda () (connect HOST PORT DATABASE USER BAD-PASSWORD))))
      (test-case
       "No password"
       (check-exn (pg-error? "")
                  (lambda () (connect HOST PORT DATABASE USER #f)))))

     (test-suite
      "connection locking"

      (test-case
       "Query while disconnected"
       (let [(cu (make-object connection%))]
         (check-exn (spgsql-error? exn:spgsql:user 'lock)
                    (lambda () (send cu query "select * from pg_class")))))

      (test-case
       "Connect while ready"
       (call-with-connection
        (lambda (c)
          (check-exn
           (spgsql-error? exn:spgsql:user 'lock)
           (lambda ()
             (send c connect HOST PORT DATABASE USER SSL SSL-ENCRYPT)))))))

     (test-suite
      "query methods"

      (test-case
       "query - multiple statements"
       (call-with-connection
        (lambda (c)
          (check-exn
           (spgsql-error? exn:spgsql:user 'expected-single-result)
           (lambda ()
             (send c query "select N from the_numbers; select null"))))))

      (test-case
       "query-list - multiple fields"
       (call-with-connection
        (lambda (c)
          (check-exn (spgsql-error? exn:spgsql:user 'expected-single-field)
                     (lambda ()
                       (send c query-list "select N, N from the_numbers"))))))

      (test-case
       "query-list - ErrorResult"
       (call-with-connection
        (lambda (c)
          (check-exn (spgsql-error? exn:spgsql:query)
                     (lambda ()
                       (send c query-list "select * from nothere"))))))

      (test-case
       "query-tuple - multiple rows"
       (call-with-connection
        (lambda (c)
          (check-exn (spgsql-error? exn:spgsql:user 'expected-single-row)
                     (lambda ()
                       (send c query-tuple
                             "select N, description from the_numbers"))))))

      (test-case
       "query-tuple - ErrorResult"
       (call-with-connection
        (lambda (c)
          (check-exn
           (spgsql-error? exn:spgsql:query)
           (lambda () (send c query-tuple "select * from nothere"))))))

      (test-case
       "query-value - multiple fields"
       (call-with-connection
        (lambda (c)
          (check-exn (spgsql-error? exn:spgsql:user 'expected-single-row)
                     (lambda ()
                       (send c query-value
                             "select N, description from the_numbers"))))))

      (test-case
       "query-value - multiple rows"
       (call-with-connection
        (lambda (c)
          (check-exn (spgsql-error? exn:spgsql:user 'expected-single-row)
                     (lambda ()
                       (send c query-value
                             "select N from the_numbers"))))))

      (test-case
       "exec - ErrorResult"
       (call-with-connection
        (lambda (c)
          (check-exn (spgsql-error? exn:spgsql:query)
                     (lambda () (send c exec "select * from nothere"))))))

      (test-case
       "exec - Check violation"
       (call-with-connection
        (lambda (c)
          (check-exn
           (spgsql-error? exn:spgsql:constraint)
           (lambda () (send c exec
                            "insert into constrained values (-1, 2)"))))))

      (test-case
       "exec - Constraint violation"
       (call-with-connection
        (lambda (c)
          (check-exn
           (spgsql-error? exn:spgsql:constraint)
           (lambda ()
             (send c exec
                   "insert into constrained values (1, 1)"))))))

      (test-case
       "mapfilter - not a recordset"
       (call-with-connection
        (lambda (c)
          (check-exn
           (spgsql-error? exn:spgsql:user 'expected-single-recordset)
           (lambda () (send c mapfilter
                            "insert into the_numbers values (1024, 'a lot')"
                            void void))))))

      (test-case
       "mapfilter - sql not a string"
       (call-with-connection
        (lambda (c)
          (check-exn
           (spgsql-error? exn:spgsql:user 'expected-sql-string)
           (lambda () (send c mapfilter void void void))))))

      (test-case
       "mapfilter - non-procedure"
       (call-with-connection
        (lambda (c)
          (check-exn (spgsql-error? exn:spgsql:user 'expected-procedure)
                     (lambda () (send c mapfilter "select 5" 'fish void)))
          (check-exn (spgsql-error? exn:spgsql:user 'expected-procedure)
                     (lambda () (send c mapfilter "select 5" void 17)))))))))
  )
