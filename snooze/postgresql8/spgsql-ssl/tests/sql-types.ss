;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module sql-types mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           "config.ss"
           "../private/exceptions.ss"
           "../private/sql-types.ss")
  (provide sql-types-test)
  
  (define sql-types-test
    (test-suite "SQL types"
      (test-suite "Parsing"
        (test-case "Parse boolean"
          (check-eq? #t (sql-parse 'boolin #"t"))
          (check-eq? #f (sql-parse 'boolin #"f"))
          (check-exn (spgsql-error? exn:spgsql:user 'sql-parse)
                      (lambda () (sql-parse 'boolin #"g"))))
        (test-case "Parse integer"
          (check-equal? 0 (sql-parse 'int4in #"0"))
          (check-equal? 17 (sql-parse 'int4in #"17"))
          (check-exn (spgsql-error? exn:spgsql:user 'sql-parse)
                      (lambda () (sql-parse 'int4in #"")))
          (check-exn (spgsql-error? exn:spgsql:user 'sql-parse)
                      (lambda () (sql-parse 'int4in #"alpha"))))
        (test-case "Parse float"
          (check-equal? 0.0 (sql-parse 'float4in #"0.0"))
          (check-equal? 17.123 (sql-parse 'float4in #"17.123"))
          (check-exn (spgsql-error? exn:spgsql:user 'sql-parse)
                      (lambda () (sql-parse 'float4in #"")))
          (check-exn (spgsql-error? exn:spgsql:user 'sql-parse)
                      (lambda () (sql-parse 'float4in #"alpha"))))
        ;; Date parsing... when implemented
        )
      (test-suite "Marshaling"
        (test-case "String formatting, unchanged"
          (check-equal? "'this is the time'"
                         (sql-marshal 'string "this is the time"))
          (check-equal? "'I am called... \"Tim\"'"
                         (sql-marshal 'text "I am called... \"Tim\"")))
        (test-case "String formatting, escaped quotes"
          (check-equal? "'this is the \\'time\\''"
                         (sql-marshal 'string "this is the 'time'"))
          (check-equal? "'nothing\\'s new under the sun'"
                         (sql-marshal 'string
                                      "nothing's new under the sun"))))))
  )