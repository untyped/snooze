#lang scheme/base

(require scheme/unit
         srfi/19
         "../test-base.ss"
         "../core/core.ss"
         "sql-data-unit.ss")

; Helpers ----------------------------------------

(define-values/invoke-unit/infer sql-data@)

(define time-tai1 (string->time-tai "2001-01-01 01:01:01"))
(define time-tai2 (string->time-tai "2002-02-02 02:02:02"))
(define time-tai3 (string->time-tai "9999-12-31 23:59:59"))

(define time-utc1 (string->time-utc "2001-01-01 01:01:01"))
(define time-utc2 (string->time-utc "2002-02-02 02:02:02"))
(define time-utc3 (string->time-utc "9999-12-31 23:59:59"))

; Tests -------------------------------------------

; test-suite
(define sql-data-unit-tests
  (test-suite "sql-data-unit.ss"
    
    (test-case "escape-value : guid"
      (let ([t (entity-make-guid-type person #t)])
        (check-equal? (escape-value t #f) "NULL")
        (check-equal? (escape-value t (make-guid person 123)) "123")
        (check-exn exn:fail:contract? (cut escape-value t (make-guid course 123)))))
    
    (test-case "escape-value : boolean"
      (let ([t (make-boolean-type #t)])
        (check-equal? (escape-value t #f) "0" "check 1")
        (check-equal? (escape-value t #t) "1" "check 2")
        (check-exn exn:fail:contract? (cut escape-value t "123") "check 3")))
    
    (test-case "escape-value : integer"
      (let ([t (make-integer-type #t #f #f)])
        (check-equal? (escape-value t #f) "NULL" "check 1")
        (check-equal? (escape-value t 1) "1" "check 2")
        (check-equal? (escape-value t 0) "0" "check 3")
        (check-equal? (escape-value t -1) "-1" "check 4")
        (check-exn exn:fail:contract? (cut escape-value t "123") "check 5")))
    
    (test-case "escape-value : real"
      (let ([t (make-real-type #t #f #f)])
        (check-equal? (escape-value t #f) "NULL" "check 1")
        (check-equal? (escape-value t 0.00000000001) "1e-11" "check 2")
        (check-equal? (escape-value t 0.0) "0.0" "check 3")
        (check-equal? (escape-value t 123456789) "123456789" "check 4")
        (check-exn exn:fail:contract? (cut escape-value t "123") "check 5")))
    
    (test-case "escape-value : string"
      (let ([t (make-string-type #t #f)])
        (check-equal? (escape-value t #f) "NULL" "check 1")
        (check-equal? (escape-value t "") "''" "check 2")
        (check-equal? (escape-value t "Dave") "'Dave'" "check 3")
        (check-equal? (escape-value t "Dave's stuff") "'Dave''s stuff'" "check 4")
        (check-exn exn:fail:contract? (cut escape-value t 123) "check 5")))
    
    (test-case "escape-value : symbol"
      (let ([t (make-symbol-type #t #f)])
        (check-equal? (escape-value t #f) "NULL" "check 1")
        (check-equal? (escape-value t '||) "''" "check 2")
        (check-equal? (escape-value t 'Dave) "'Dave'" "check 3")
        (check-equal? (escape-value t '|Dave's stuff|) "'Dave''s stuff'" "check 4")
        (check-exn exn:fail:contract? (cut escape-value t "123") "check 5")))
    
    (test-case "escape-value : time-tai"
      (let ([t (make-time-tai-type #t)])
        (check-equal? (escape-value t #f) "NULL" "check 1")
        (check-equal? (escape-value t (make-time time-tai 0 0)) "0000000000" "check 2")
        (check-equal? (escape-value t (make-time time-tai 1 0)) "0000000001" "check 3")
        (check-equal? (escape-value t (make-time time-tai 123456789 0)) "0123456789" "check 4")
        (check-equal? (escape-value t (make-time time-tai 123456789 1)) "1123456789" "check 5")
        (check-equal? (escape-value t time-tai1)  "978310893000000000" "check 6")
        (check-equal? (escape-value t time-tai2) "1012615354000000000" "check 7")
        (check-exn exn:fail:contract? (cut escape-value t "123") "check 8")))
    
    (test-case "escape-value : time-utc"
      (let ([t (make-time-utc-type #t)])
        (check-equal? (escape-value t #f) "NULL" "check 1")
        (check-equal? (escape-value t (make-time time-utc 0 0)) "0000000000" "check 2")
        (check-equal? (escape-value t (make-time time-utc 1 0)) "0000000001" "check 3")
        (check-equal? (escape-value t (make-time time-utc 123456789 0)) "0123456789" "check 4")
        (check-equal? (escape-value t (make-time time-utc 123456789 1)) "1123456789" "check 5")
        (check-equal? (escape-value t time-utc1)  "978310861000000000" "check 6")
        (check-equal? (escape-value t time-utc2) "1012615322000000000" "check 7")
        (check-exn exn:fail:contract? (cut escape-value t "123") "check 8")))
    
    (test-exn "escape value : unknown type"
      exn:fail:contract?
      (cut escape-value 'foo "hello"))
    
    (test-case "parse-value : boolean"
      (let ([t (make-boolean-type #t)])
        (check-equal? (parse-value t "0") #f "check 1")
        (check-equal? (parse-value t "1") #t "check 2")
        (check-equal? (parse-value t #f) #f "check 3")))
    
    (test-case "parse-value : integer"
      (let ([t (make-integer-type #t #f #f)])
        (check-equal? (parse-value t #f) #f "check 1")
        (check-equal? (parse-value t "1") 1 "check 2")
        (check-equal? (parse-value t "0") 0 "check 3")
        (check-equal? (parse-value t "-1") -1 "check 4")))
    
    (test-case "parse-value : real"
      (let ([t (make-real-type #t #f #f)])
        (check-equal? (parse-value t #f) #f "check 1")
        (check-equal? (parse-value t "0.00000000001") 0.00000000001 "check 2")
        (check-equal? (parse-value t "0.0") 0.0 "check 3")
        (check-equal? (parse-value t "123456789") 123456789 "check 4")))
    
    (test-case "parse-value : string"
      (let ([t (make-string-type #t #f)])
        (check-equal? (parse-value t #f) #f "check 1")
        (check-equal? (parse-value t "") "" "check 2")
        (check-equal? (parse-value t "Dave") "Dave" "check 3")
        (check-equal? (parse-value t "Dave's stuff") "Dave's stuff" "check 4")))
    
    (test-case "parse-value : symbol"
      (let ([t (make-symbol-type #t #f)])
        (check-equal? (parse-value t #f) #f "check 1")
        (check-equal? (parse-value t "") '|| "check 2")
        (check-equal? (parse-value t "Dave") 'Dave "check 3")
        (check-equal? (parse-value t "Dave's stuff") '|Dave's stuff| "check 4")))
    
    (test-case "parse-value : time-tai"
      (let ([t (make-time-tai-type #t)])
        (check-equal? (parse-value t "") (make-time time-tai 0 0) "check 1")
        (check-equal? (parse-value t "0") (make-time time-tai 0 0) "check 2")
        (check-equal? (parse-value t "000000000") (make-time time-tai 0 0) "check 3")
        (check-equal? (parse-value t "0000000000") (make-time time-tai 0 0) "check 4")
        (check-equal? (parse-value t "1000000000") (make-time time-tai 0 1) "check 5")
        (check-equal? (parse-value t "123456789") (make-time time-tai 123456789 0) "check 6")
        (check-equal? (parse-value t "0123456789") (make-time time-tai 123456789 0) "check 7")
        (check-equal? (parse-value t "1123456789") (make-time time-tai 123456789 1) "check 8")
        (check-equal? (parse-value t (escape-value t time-tai1)) time-tai1 "check 9")
        (check-equal? (parse-value t (escape-value t time-tai2)) time-tai2 "check 10")
        (check-equal? (parse-value t (escape-value t time-tai3)) time-tai3 "check 11")))
    
    (test-case "parse-value : time-utc"
      (let ([t (make-time-utc-type #t)])
        (check-equal? (parse-value t "") (make-time time-utc 0 0) "check 1")
        (check-equal? (parse-value t "0") (make-time time-utc 0 0) "check 2")
        (check-equal? (parse-value t "000000000") (make-time time-utc 0 0) "check 3")
        (check-equal? (parse-value t "0000000000") (make-time time-utc 0 0) "check 4")
        (check-equal? (parse-value t "1000000000") (make-time time-utc 0 1) "check 5")
        (check-equal? (parse-value t "123456789") (make-time time-utc 123456789 0) "check 6")
        (check-equal? (parse-value t "0123456789") (make-time time-utc 123456789 0) "check 7")
        (check-equal? (parse-value t "1123456789") (make-time time-utc 123456789 1) "check 8")
        (check-equal? (parse-value t (escape-value t time-utc1)) time-utc1 "check 9")
        (check-equal? (parse-value t (escape-value t time-utc2)) time-utc2 "check 10")
        (check-equal? (parse-value t (escape-value t time-utc3)) time-utc3 "check 11")))
    
    (test-exn "parse-value : unknown type"
      exn:fail:contract?
      (cut parse-value 'foo "hello"))
    
    (test-case "make-parser"
      (let ([parse (make-parser (list (make-boolean-type #t)
                                      (make-integer-type #t #f #f)
                                      (make-string-type #t #f)
                                      (make-symbol-type #t #f)))])
        (check-equal? (parse (vector "1" "1" "1" "1")) (vector #t 1 "1" '|1|) "check 1")
        (check-equal? (parse (vector #f #f #f #f)) (vector #f #f #f #f) "check 1")))))

; Provide statements -----------------------------

(provide sql-data-unit-tests)
