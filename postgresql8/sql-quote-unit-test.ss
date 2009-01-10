(module sql-quote-unit-test mzscheme
  
  (require (lib "unitsig.ss")
           (lib "string.ss" "srfi" "13")
           (lib "time.ss"   "srfi" "19"))
  
  (require (file "../era.ss")
           (file "../test-base.ss")
           (file "../test-data.ss")
           (file "../type.ss")
           (file "../generic/sql-sig.ss")
           (file "spgsql-ssl/spgsql.ss")
           (file "sql-quote-unit.ss"))
  
  (provide sql-quote-unit-tests)

  (define-values/invoke-unit/sig sql-quote^ sql-quote@)
    
  (define sql-quote-unit-tests
    (test-suite
     "sql-quote-unit.ss"
     
     (test-equal?
      "quote-id wraps symbol id in \"double quotes\""
      (quote-id 'my-id)
      "\"my-id\"")
     
     (test-equal?
      "quote-id wraps string id in \"double quotes\""
      (quote-id "my-id")
      "\"my-id\"")
     
     (test-equal?
      "quote-data quotes writes strings 'in quotes'"
      (quote-data type:text "Hi Mom!")
      "'Hi Mom!'")
     
     (test-equal?
      "quote-data escapes ' characters in quoted strings"
      (quote-data type:text "'There's too much confusion!'")
      "'''There''s too much confusion!'''")
     
     (test-equal?
      "quote-data converts numbers to strings"
      (quote-data type:integer/1 2)
      "2")

     (test-equal?
      "quote-data converts boolean #t correctly"
      (quote-data type:boolean/t #t)
      "true")
     
     (test-equal?
      "quote-data converts boolean #f correctly"
      (quote-data type:boolean/t #f)
      "false")

     (test-equal?
      "quote-data converts #f id to NULL"
      (quote-data type:id #f)
      "NULL")

     (test-equal?
      "quote-data converts number id to string"
      (quote-data type:id 2)
      "2")
     
     (test-equal?
      "quote-data converts #f revision to NULL"
      (quote-data type:revision #f)
      "NULL")

     (test-equal?
      "quote-data converts number revision to string"
      (quote-data type:revision 2)
      "2")
     
     (test-case
      "quote-data quotes time-tai data correctly"
      (check-equal? (quote-data type:time-tai                                  #f)                            "NULL" "Test 1 failed")
      (check-equal? (quote-data type:time-tai (make-time time-tai         0    0)) "'1970-01-01 00:00:00.000000000'" "Test 2 failed")
      (check-equal? (quote-data type:time-tai (make-time time-tai         1    0)) "'1970-01-01 00:00:00.000000001'" "Test 3 failed")
      (check-equal? (quote-data type:time-tai (make-time time-tai      1000    0)) "'1970-01-01 00:00:00.000001000'" "Test 4 failed")
      (check-equal? (quote-data type:time-tai (make-time time-tai 123456789    0)) "'1970-01-01 00:00:00.123456789'" "Test 5 failed")
      (check-equal? (quote-data type:time-tai (make-time time-tai 123456789    1)) "'1970-01-01 00:00:01.123456789'" "Test 6 failed")
      (check-equal? (quote-data type:time-tai                               time1) "'2001-01-01 01:01:01.000000000'" "Test 7 failed")
      (check-equal? (quote-data type:time-tai                               time2) "'2002-02-02 02:02:02.000000000'" "Test 8 failed")
      (check-equal? (quote-data type:time-tai                               time3) "'9999-01-01 01:01:01.000000000'" "Test 8 failed"))
     
     (test-case
      "quote-data raises exn on type mismatch: id"
      (check-exn exn:fail:snooze? (lambda () (quote-data type:id "123")))
      (check-not-exn              (lambda () (quote-data type:id 123)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:id 'abc)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:id #t)))
      (check-not-exn              (lambda () (quote-data type:id #f)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:id time1))))
     
     (test-case
      "quote-data raises exn on type mismatch: revision"
      (check-exn exn:fail:snooze? (lambda () (quote-data type:revision "123")))
      (check-not-exn              (lambda () (quote-data type:revision 123)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:revision 'abc)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:revision #t)))
      (check-not-exn              (lambda () (quote-data type:revision #f)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:revision time1))))
     
     (test-case
      "quote-data raises exn on type mismatch: text"
      (check-not-exn              (lambda () (quote-data type:text "123")))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:text 123)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:text 'abc)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:text #t)))
      (check-not-exn              (lambda () (quote-data type:text #f)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:text time1))))
     
     (test-case
      "quote-data raises exn on type mismatch: integer"
      (check-exn exn:fail:snooze? (lambda () (quote-data type:integer "123")))
      (check-not-exn              (lambda () (quote-data type:integer 123)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:integer 'abc)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:integer #t)))
      (check-not-exn              (lambda () (quote-data type:integer #f)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:integer time1))))
     
     (test-case
      "quote-data raises exn on type mismatch: symbol"
      (check-exn exn:fail:snooze? (lambda () (quote-data type:symbol "123")))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:symbol 123)))
      (check-not-exn              (lambda () (quote-data type:symbol 'abc)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:symbol #t)))
      (check-not-exn              (lambda () (quote-data type:symbol #f)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:symbol #t)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:symbol time1))))
     
     (test-case
      "quote-data raises exn on type mismatch: boolean"
      (check-exn exn:fail:snooze? (lambda () (quote-data type:boolean "123")))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:boolean 123)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:boolean 'abc)))
      (check-not-exn              (lambda () (quote-data type:boolean #t)))
      (check-not-exn              (lambda () (quote-data type:boolean #f)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:boolean time1))))
     
     (test-case
      "quote-data raises exn on type mismatch: time-tai"
      (check-exn exn:fail:snooze? (lambda () (quote-data type:time-tai "123")))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:time-tai 123)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:time-tai 'abc)))
      (check-exn exn:fail:snooze? (lambda () (quote-data type:time-tai #t)))
      (check-not-exn              (lambda () (quote-data type:time-tai #f)))
      (check-not-exn              (lambda () (quote-data type:time-tai time1))))
     
     (test-exn
      "quote-data raises exception with unknown type"
      exn:fail:contract?
      (lambda () (quote-data 'foo "hello")))
     
     (test-exn
      "unquote-data raises exception with unknown type"
      exn:fail:contract?
      (lambda () (unquote-data 'foo "hello")))

     (test-case
      "unquote-data converts normal data correctly"
      (check equal? (unquote-data type:text "foo") "foo")
      (check equal? (unquote-data type:integer/1 "2") 2)
      (check equal? (unquote-data type:id "2") 2)
      (check equal? (unquote-data type:revision "2") 2)
      (check equal? (unquote-data type:symbol "abc") 'abc)
      (check equal? (unquote-data type:boolean/t "t") #t)
      (check equal? (unquote-data type:boolean/t "f") #f))

     (test-case
      "unquote-data converts normal time-tai data correctly"
      (check-exn exn:fail:snooze? (lambda () (unquote-data type:time-tai "")) "Test 1 failed")
      (check-equal? (unquote-data type:time-tai           #"9999-01-01 01:01:01") time3 "Test 2 failed")
      (check-equal? (unquote-data type:time-tai    #"9999-01-01 01:01:01.000000") time3 "Test 3 failed")
      (check-equal? (unquote-data type:time-tai    #"9999-01-01 01:01:01.000000") time3 "Test 4 failed")
      (check-equal? (unquote-data type:time-tai #"9999-01-01 01:01:01.000000000") time3 "Test 5 failed")
      (check-equal? (unquote-data type:time-tai (string-trim-both (quote-data type:time-tai time1) #\')) time1 "Test 6 failed")
      (check-equal? (unquote-data type:time-tai (string-trim-both (quote-data type:time-tai time2) #\')) time2 "Test 7 failed")
      (check-equal? (unquote-data type:time-tai (string-trim-both (quote-data type:time-tai time3) #\')) time3 "Test 8 failed")
      (check-equal? (unquote-data type:time-tai #"1234-12-23 12:34:56.123456789")
                    (date->time-tai (make-srfi:date 123456789 56 34 12 23 12 1234 0))
                    "Test 9 failed"))
     
     (test-case
      "unquote-data converts blank and null data correctly"
      (check equal? (unquote-data type:text #"")           "")
      (check equal? (unquote-data type:text sql-null)      #f)
      (check equal? (unquote-data type:integer/1 #"")      #f)
      (check equal? (unquote-data type:integer/1 sql-null) #f)
      (check equal? (unquote-data type:id #"")             #f)
      (check equal? (unquote-data type:revision sql-null)  #f)
      (check equal? (unquote-data type:revision #"")       #f)
      (check equal? (unquote-data type:id sql-null)        #f)
      (check equal? (unquote-data type:symbol #"")         '||)
      (check equal? (unquote-data type:symbol sql-null)    #f)
      (check equal? (unquote-data type:boolean/t #"")      #f)
      (check equal? (unquote-data type:boolean/t sql-null) #f)
      (check-exn exn:fail:snooze? (lambda () (unquote-data type:time-tai #"")))
      (check equal? (unquote-data type:time-tai sql-null)  #f))

     (test-equal?
      "data-unquoter works on non-null values"
      (let ([unquote (make-data-unquoter (list type:id type:revision type:text type:integer/1 type:symbol type:boolean/t type:time-tai))])
        (unquote (vector "1" "2" "abc" "3" "def" "t" #"1234-12-23 12:34:56.123456789")))
      (vector 1 2 "abc" 3 'def #t (date->time-tai (make-srfi:date 123456789 56 34 12 23 12 1234 0))))

     (test-equal?
      "data-unquoter works on null values"
      (let ([unquote (make-data-unquoter (list type:id type:revision type:text type:integer/1 type:symbol type:boolean/t type:time-tai))])
        (unquote (vector sql-null sql-null sql-null sql-null sql-null sql-null sql-null)))
      (vector #f #f #f #f #f #f #f))

     ))
  
  )
