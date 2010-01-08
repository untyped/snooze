;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module sql-types mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 7))
           (lib "class.ss")
           (lib "etc.ss")
           (prefix srfi: (lib "19.ss" "srfi"))
           "config.ss"
           "../private/sql-data.ss")
  (provide sql-types-test)

  (define (sql-parse type b)
    ((hash-table-get external=>datum type (lambda _ values)) b))

  (define (sql-marshal type d)
    ((hash-table-get datum=>external/typename type (lambda _ values)) d))

  (define-syntax check-roundtrip
    (syntax-rules ()
      [(check-roundtrip c type expr)
       (check-roundtrip c type expr check-equal?)]
      [(check-roundtrip c type expr check)
       (begin
         (let ([value expr])
           (check (let ([q (string-append "select $1::" (symbol->string 'type))])
                    ((send c prepare-query-value q) value))
                  value)
           (check (let ([q (format-sql "select ~a" [type value])])
                    (send c query-value q))
                  value)))]))

  (define (check-timestamptz-equal? a b)
    (check srfi:time=?
           (srfi:date->time-utc (sql-datetime->srfi-date a))
           (srfi:date->time-utc (sql-datetime->srfi-date b))))

  (define sql-types-test
    (test-suite "SQL types"
      (test-suite "Parsing"
        (test-case "Parse boolean"
          (check-eq? #t (sql-parse 'boolin "t"))
          (check-eq? #f (sql-parse 'boolin "f"))
          (check-exn exn? (lambda () (sql-parse 'boolin "g"))))
        (test-case "Parse integer"
          (check-equal? 0 (sql-parse 'int4in "0"))
          (check-equal? 17 (sql-parse 'int4in "17"))
          (check-exn exn? (lambda () (sql-parse 'int4in "")))
          (check-exn exn? (lambda () (sql-parse 'int4in "alpha"))))
        (test-case "Parse float"
          (check-equal? 0.0 (sql-parse 'float4in "0.0"))
          (check-equal? 17.123 (sql-parse 'float4in "17.123"))
          (check-exn exn? (lambda () (sql-parse 'float4in "")))
          (check-exn exn? (lambda () (sql-parse 'float4in "alpha"))))
        (test-case "Parse date"
          (check-equal? (make-sql-date 1980 08 17)
                        (sql-parse 'date_in "1980-08-17")))
        (test-case "Parse time"
          (check-equal? (make-sql-time 12 34 56 0 #f)
                        (sql-parse 'time_in "12:34:56"))
          (check-equal? (make-sql-time 12 34 56 789000000 #f)
                        (sql-parse 'time_in "12:34:56.789"))
          (check-equal? (make-sql-time 12 34 56 000789000 #f)
                        (sql-parse 'time_in "12:34:56.000789")))
        (test-case "Parse timetz"
          (check-equal? (make-sql-time 12 34 56 0 4980)
                        (sql-parse 'timetz_in "12:34:56+0123"))
          (check-equal? (make-sql-time 12 34 56 789000000 4980)
                        (sql-parse 'timetz_in "12:34:56.789+0123"))
          (check-equal? (make-sql-time 12 34 56 000789000 -4980)
                        (sql-parse 'timetz_in "12:34:56.000789-0123")))
        (test-case "Parse timestamp"
          (check-equal?
           (make-sql-timestamp 1980 08 17 12 34 56 0 #f)
           (sql-parse 'timestamp_in "1980-08-17 12:34:56"))
          (check-equal?
           (make-sql-timestamp 1980 08 17 12 34 56 123000000 #f)
           (sql-parse 'timestamp_in "1980-08-17 12:34:56.123"))
          (check-equal?
           (make-sql-timestamp 1980 08 17 12 34 56 000123000 #f)
           (sql-parse 'timestamp_in "1980-08-17 12:34:56.000123")))
        (test-case "Parse timestamptz"
          (check-equal?
           (make-sql-timestamp 1980 08 17 12 34 56 0 4980)
           (sql-parse 'timestamptz_in "1980-08-17 12:34:56+0123"))
          (check-equal?
           (make-sql-timestamp 1980 08 17 12 34 56 123000000 4980)
           (sql-parse 'timestamptz_in "1980-08-17 12:34:56.123+0123"))
          (check-equal?
           (make-sql-timestamp 1980 08 17 12 34 56 000123000 -4980)
           (sql-parse 'timestamptz_in "1980-08-17 12:34:56.000123-0123"))))

      (test-suite "Roundtrip"
        (test-case "boolean"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c bool #t)
             (check-roundtrip c bool #f))))
        (test-case "bytea"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c bytea #"this is the time to remember")
             (check-roundtrip c bytea #"that's the way it is")
             (check-roundtrip c bytea (list->bytes (build-list 256 values))))))
        (test-case "numbers"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c int 5)
             (check-roundtrip c int -1)
             (check-roundtrip c int #x7FFFFF)
             (check-roundtrip c int #x-800000)
             (check-roundtrip c float 1.0)
             (check-roundtrip c float 1.1)
             (check-roundtrip c float -5.8)
             (check-roundtrip c float +inf.0)
             (check-roundtrip c float -inf.0)
             (check-roundtrip c float +nan.0))))
        (test-case "numeric"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c numeric 12345678901234567890)
             (check-roundtrip c numeric #e1234567890.0987654321)
             (check-roundtrip c numeric +nan.0))))
        (test-case "strings"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c text "this is the time to remember")
             (check-roundtrip c text "that's the way it is")
             (check-roundtrip c text (string #\\))
             (check-roundtrip c text (string #\'))
             (check-roundtrip c text (string #\\ #\'))
             (check-roundtrip c text "λ the ultimate")
             (check-roundtrip c text (list->string
                                      (build-list 800
                                                  (lambda (n)
                                                    (integer->char (add1 n)))))))))
        (test-case "date"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c date (make-sql-date 1980 08 17)))))
        (test-case "time"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c time (make-sql-time 12 34 56 0 #f))
             (check-roundtrip c time (make-sql-time 12 34 56 123456000 #f))
             (check-roundtrip c time (make-sql-time 12 34 56 100000000 #f))
             (check-roundtrip c time (make-sql-time 12 34 56 000001000 #f)))))
        (test-case "timetz"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c timetz (make-sql-time 12 34 56 0 3600))
             (check-roundtrip c timetz (make-sql-time 12 34 56 123456000 3600))
             (check-roundtrip c timetz (make-sql-time 12 34 56 100000000 3600))
             (check-roundtrip c timetz (make-sql-time 12 34 56 000001000 3600)))))
        (test-case "timestamp"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c timestamp
               (make-sql-timestamp 1980 08 17 12 34 56 0 #f))
             (check-roundtrip c timestamp
               (make-sql-timestamp 1980 08 17 12 34 56 123456000 #f))
             (check-roundtrip c timestamp
               (make-sql-timestamp 1980 08 17 12 34 56 100000000 #f))
             (check-roundtrip c timestamp
               (make-sql-timestamp 1980 08 17 12 34 56 000001000 #f)))))

        ;; Bizarrely, PostgreSQL converts timestamptz to a standard timezone
        ;; when returning them, but it doesn't for timetz.
        (test-case "timestamptz"
          (call-with-connection
           (lambda (c)
             (check-roundtrip c timestamptz
               (make-sql-timestamp 1980 08 17 12 34 56 0 3600)
               check-timestamptz-equal?)
             (check-roundtrip c timestamptz
               (make-sql-timestamp 1980 08 17 12 34 56 123456000 3600)
               check-timestamptz-equal?)
             (check-roundtrip c timestamptz
               (make-sql-timestamp 1980 08 17 12 34 56 100000000 3600)
               check-timestamptz-equal?)
             (check-roundtrip c timestamptz
               (make-sql-timestamp 1980 08 17 12 34 56 000001000 3600)
               check-timestamptz-equal?))))
        ))))
