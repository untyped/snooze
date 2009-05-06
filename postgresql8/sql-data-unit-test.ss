#lang scheme/base

(require scheme/unit
         srfi/19
         srfi/26
         (planet schematics/spgsql:2/spgsql)
         "../test-base.ss"
         "../test-data.ss"
         "../era/era.ss"
         "sql-data-unit.ss")

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
      (let ([t (make-guid-type #t person)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t (entity-make-guid person 123)) "123")
        (check-exn exn:fail? (cut escape-value t (entity-make-guid course 123)))))
    
    (test-case "escape-value : boolean"
      (let ([t (make-boolean-type #t)])
        (check-equal? (escape-value t #f) "false")
        (check-equal? (escape-value t #t) "true")
        (check-exn exn:fail:contract? (cut escape-value t "123"))))
    
    (test-case "escape-value : integer"
      (let ([t (make-integer-type #t)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t 1) "1")
        (check-equal? (escape-value t 0) "0")
        (check-equal? (escape-value t -1) "-1")
        (check-exn exn:fail:contract? (cut escape-value t "123"))))
    
    (test-case "escape-value : real"
      (let ([t (make-real-type #t)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t 0.00000000001) "1e-11")
        (check-equal? (escape-value t 0.0) "0.0")
        (check-equal? (escape-value t 123456789) "123456789")
        (check-exn exn:fail:contract? (cut escape-value t "123"))))
    
    (test-case "escape-value : string"
      (let ([t (make-string-type #t #f)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t "") "''")
        (check-equal? (escape-value t "Dave") "'Dave'")
        (check-equal? (escape-value t "Dave's stuff") "'Dave''s stuff'")
        (check-equal? (escape-value t "Dave's\\stuff") "'Dave''s\\stuff'")
        (check-equal? (escape-value t "Dave's\nstuff") "'Dave''s\nstuff'")
        (check-equal? (escape-value t "Dave's\n\rstuff") "'Dave''s\n\rstuff'")
        (check-exn exn:fail:contract? (cut escape-value t 123))))
    
    (test-case "escape-value : symbol"
      (let ([t (make-symbol-type #t #f)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t '||) "''")
        (check-equal? (escape-value t 'Dave) "'Dave'")
        (check-equal? (escape-value t '|Dave's stuff|) "'Dave''s stuff'")
        (check-exn exn:fail:contract? (cut escape-value t "123"))))
    
    (test-case "escape-value : time-tai"
      (let ([t (make-time-tai-type #t)])
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
      (let ([t (make-time-utc-type #t)])
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
      (let ([t (make-boolean-type #t)])
        (check-equal? (parse-value (current-snooze) t sql-null) #f)
        (check-equal? (parse-value (current-snooze) t #t) #t)
        (check-equal? (parse-value (current-snooze) t #f) #f)))
    
    (test-case "parse-value : integer"
      (let ([t (make-integer-type #t)])
        (check-equal? (parse-value (current-snooze) t sql-null) #f)
        (check-equal? (parse-value (current-snooze) t 1) 1)
        (check-equal? (parse-value (current-snooze) t 0) 0)
        (check-equal? (parse-value (current-snooze) t -1) -1)))
    
    (test-case "parse-value : real"
      (let ([t (make-real-type #t)])
        (check-equal? (parse-value (current-snooze) t sql-null) #f)
        (check-equal? (parse-value (current-snooze) t 0.00000000001) 0.00000000001)
        (check-equal? (parse-value (current-snooze) t 0.0) 0.0)
        (check-equal? (parse-value (current-snooze) t 123456789) 123456789)))
    
    (test-case "parse-value : string"
      (let ([t (make-string-type #t #f)])
        (check-equal? (parse-value (current-snooze) t sql-null) #f)
        (check-equal? (parse-value (current-snooze) t "") "")
        (check-equal? (parse-value (current-snooze) t "Dave") "Dave")
        (check-equal? (parse-value (current-snooze) t "Dave's stuff") "Dave's stuff")))
    
    (test-case "parse-value : symbol"
      (let ([t (make-symbol-type #t #f)])
        (check-equal? (parse-value (current-snooze) t sql-null) #f)
        (check-equal? (parse-value (current-snooze) t "") '||)
        (check-equal? (parse-value (current-snooze) t "Dave") 'Dave)
        (check-equal? (parse-value (current-snooze) t "Dave's stuff") '|Dave's stuff|)))
    
    (test-case "parse-value : time-tai"
      (let ([t (make-time-tai-type #t)])
        (check-exn exn:fail:contract?
          (cut parse-value t ""))
        (check-equal? (parse-value (current-snooze) t (make-sql-timestamp 9999 12 31 23 59 59 0 0)) 
                      time-tai3
                      "check 2 failed")
        (check-equal? (parse-value (current-snooze) t (make-sql-timestamp 1234 12 23 12 34 56 123456000 0))
                      (date->time-tai (make-date 123456000 56 34 12 23 12 1234 0)))))
    
    (test-case "parse-value : time-utc"
      (let ([t (make-time-utc-type #t)])
        (check-exn exn:fail:contract? (cut parse-value t ""))
        (check-equal? (parse-value (current-snooze) t (make-sql-timestamp 1234 12 23 12 34 56 123456000 0))
                      (date->time-utc (make-date 123456000 56 34 12 23 12 1234 0)))))
    
    (test-exn "parse-value : unknown type"
      exn:fail:contract?
      (cut parse-value 'foo "hello"))
    
    (test-case "make-parser"
      (let ([parse (make-parser (current-snooze)
                                (list (make-boolean-type #t)
                                      (make-integer-type #t)
                                      (make-string-type #t #f)
                                      (make-symbol-type #t #f)))])
        (check-equal? (parse (list #t 1 "1" "1"))
                      (list #t 1 "1" '|1|))
        (check-equal? (parse (list sql-null sql-null sql-null sql-null))
                      (list #f #f #f #f))))))

; Provide statements -----------------------------

(provide sql-data-unit-tests)
