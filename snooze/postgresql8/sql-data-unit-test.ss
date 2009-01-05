#lang scheme/base

(require scheme/unit
         srfi/19
         srfi/26
         (planet schematics/spgsql:2/spgsql)
         (file "../test-base.ss")
         (file "../test-data.ss")
         (file "../test-util.ss")
         (file "../era/era.ss")
         (file "sql-data-unit.ss"))

(define-values/invoke-unit/infer sql-data@)

(define time-tai1 (string->time-tai "2001-01-01 01:01:01"))
(define time-tai2 (string->time-tai "2002-02-02 02:02:02"))
(define time-tai3 (string->time-tai "9999-12-31 23:59:59"))

(define time-utc1 (string->time-utc "2001-01-01 01:01:01"))
(define time-utc2 (string->time-utc "2002-02-02 02:02:02"))
(define time-utc3 (string->time-utc "9999-12-31 23:59:59"))

(define sql-data-unit-tests
  (test-suite "sql-data-unit.ss"
    
    (test-case "escape-value : guid"
      (let ([t (make-guid-type #t #f entity:person)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t (make-guid entity:person 123)) "123")
        (check-exn exn:fail:contract? (cut escape-value t (make-guid entity:course 123)))))
    
    (test-case "escape-value : boolean"
      (let ([t (make-boolean-type #t #f)])
        (check-equal? (escape-value t #f) "false")
        (check-equal? (escape-value t #t) "true")
        (check-exn exn:fail:contract? (cut escape-value t "123"))))
    
    (test-case "escape-value : integer"
      (let ([t (make-integer-type #t #f)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t 1) "1")
        (check-equal? (escape-value t 0) "0")
        (check-equal? (escape-value t -1) "-1")
        (check-exn exn:fail:contract? (cut escape-value t "123"))))
    
    (test-case "escape-value : real"
      (let ([t (make-real-type #t #f)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t 0.00000000001) "1e-11")
        (check-equal? (escape-value t 0.0) "0.0")
        (check-equal? (escape-value t 123456789) "123456789")
        (check-exn exn:fail:contract? (cut escape-value t "123"))))
    
    (test-case "escape-value : string"
      (let ([t (make-string-type #t #f #f)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t "") "''")
        (check-equal? (escape-value t "Dave") "'Dave'")
        (check-equal? (escape-value t "Dave's stuff") "'Dave''s stuff'")
        (check-equal? (escape-value t "Dave's\\stuff") "'Dave''s\\stuff'")
        (check-equal? (escape-value t "Dave's\nstuff") "'Dave''s\nstuff'")
        (check-equal? (escape-value t "Dave's\n\rstuff") "'Dave''s\n\rstuff'")
        (check-exn exn:fail:contract? (cut escape-value t 123))))
    
    (test-case "escape-value : symbol"
      (let ([t (make-symbol-type #t #f #f)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t '||) "''")
        (check-equal? (escape-value t 'Dave) "'Dave'")
        (check-equal? (escape-value t '|Dave's stuff|) "'Dave''s stuff'")
        (check-exn exn:fail:contract? (cut escape-value t "123"))))
    
    (test-case "escape-value : time-tai"
      (let ([t (make-time-tai-type #t #f)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t (make-time time-tai 0 0)) "'1970-01-01 00:00:00.000000000'")
        (check-equal? (escape-value t (make-time time-tai 1 0)) "'1970-01-01 00:00:00.000000001'")
        (check-equal? (escape-value t (make-time time-tai 1000 0)) "'1970-01-01 00:00:00.000001000'")
        (check-equal? (escape-value t (make-time time-tai 123456789 0)) "'1970-01-01 00:00:00.123456789'")
        (check-equal? (escape-value t (make-time time-tai 123456789 1)) "'1970-01-01 00:00:01.123456789'")
        (check-equal? (escape-value t time-tai1) "'2001-01-01 01:01:01.000000000'")
        (check-equal? (escape-value t time-tai2) "'2002-02-02 02:02:02.000000000'")
        (check-equal? (escape-value t time-tai3) "'9999-12-31 23:59:59.000000000'")
        (check-exn exn:fail:contract? (cut escape-value t "123") "check 9")))
    
    (test-case "escape-value : time-utc"
      (let ([t (make-time-utc-type #t #f)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t (make-time time-utc 0 0)) "'1970-01-01 00:00:00.000000000'")
        (check-equal? (escape-value t (make-time time-utc 1 0)) "'1970-01-01 00:00:00.000000001'")
        (check-equal? (escape-value t (make-time time-utc 1000 0)) "'1970-01-01 00:00:00.000001000'")
        (check-equal? (escape-value t (make-time time-utc 123456789 0)) "'1970-01-01 00:00:00.123456789'")
        (check-equal? (escape-value t (make-time time-utc 123456789 1)) "'1970-01-01 00:00:01.123456789'")
        (check-equal? (escape-value t time-utc1) "'2001-01-01 01:01:01.000000000'")
        (check-equal? (escape-value t time-utc2) "'2002-02-02 02:02:02.000000000'")
        (check-equal? (escape-value t time-utc3) "'9999-12-31 23:59:59.000000000'")
        (check-exn exn:fail:contract? (cut escape-value t "123"))))
    
    (test-exn "escape value : unknown type"
      exn:fail:contract?
      (cut escape-value 'foo "hello"))
    
    (test-case "parse-value : boolean"
      (let ([t (make-boolean-type #t #f)])
        (check-equal? (parse-value t sql-null) #f)
        (check-equal? (parse-value t #t) #t)
        (check-equal? (parse-value t #f) #f)))
    
    (test-case "parse-value : integer"
      (let ([t (make-integer-type #t #f)])
        (check-equal? (parse-value t sql-null) #f)
        (check-equal? (parse-value t 1) 1)
        (check-equal? (parse-value t 0) 0)
        (check-equal? (parse-value t -1) -1)))
    
    (test-case "parse-value : real"
      (let ([t (make-real-type #t #f)])
        (check-equal? (parse-value t sql-null) #f)
        (check-equal? (parse-value t 0.00000000001) 0.00000000001)
        (check-equal? (parse-value t 0.0) 0.0)
        (check-equal? (parse-value t 123456789) 123456789)))
    
    (test-case "parse-value : string"
      (let ([t (make-string-type #t #f #f)])
        (check-equal? (parse-value t sql-null) #f)
        (check-equal? (parse-value t "") "")
        (check-equal? (parse-value t "Dave") "Dave")
        (check-equal? (parse-value t "Dave's stuff") "Dave's stuff")))
    
    (test-case "parse-value : symbol"
      (let ([t (make-symbol-type #t #f #f)])
        (check-equal? (parse-value t sql-null) #f)
        (check-equal? (parse-value t "") '||)
        (check-equal? (parse-value t "Dave") 'Dave)
        (check-equal? (parse-value t "Dave's stuff") '|Dave's stuff|)))
    
    (test-case "parse-value : time-tai"
      (let ([t (make-time-tai-type #t #f)])
        (check-exn exn:fail:contract?
          (cut parse-value t ""))
        (check-equal? (parse-value t (make-sql-timestamp 9999 12 31 23 59 59 0 0)) 
                      time-tai3
                      "check 2 failed")
        (check-equal? (parse-value t (make-sql-timestamp 1234 12 23 12 34 56 123456000 0))
                      (date->time-tai (make-date 123456000 56 34 12 23 12 1234 0)))))
    
    (test-case "parse-value : time-utc"
      (let ([t (make-time-utc-type #t #f)])
        (check-exn exn:fail:contract? (cut parse-value t ""))
        (check-equal? (parse-value t (make-sql-timestamp 1234 12 23 12 34 56 123456000 0))
                      (date->time-utc (make-date 123456000 56 34 12 23 12 1234 0)))))
    
    (test-exn "parse-value : unknown type"
      exn:fail:contract?
      (cut parse-value 'foo "hello"))
    
    (test-case "make-parser"
      (let ([parse (make-parser (list (make-boolean-type #t #f)
                                      (make-integer-type #t #f)
                                      (make-string-type #t #f #f)
                                      (make-symbol-type #t #f #f)))])
        (check-equal? (parse (vector #t 1 "1" "1")) (vector #t 1 "1" '|1|))
        (check-equal? (parse (vector sql-null sql-null sql-null sql-null)) (vector #f #f #f #f))))))

; Provide statements -----------------------------

(provide sql-data-unit-tests)
