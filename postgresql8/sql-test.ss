#lang scheme/base

(require "../test-base.ss")

(require scheme/serialize
         srfi/19
         "../spgsql-hacked/spgsql.ss"
         "../core/core.ss"
         "../sql/sql.ss"
         "sql.ss")

; Helpers --------------------------------------

; (U database<%> #f)
(define database #f)

; (a b c ... output-port -> void) a b c ... -> string
;
; Calls the supplied SQL display procedure, captures its output
; in an output-string, and returns it as a string.
(define (capture-sql proc . args)
  (define out (open-output-string))
  (apply proc (append args (list out)))
  (get-output-string out))

; time-tai
(define time-tai1 (string->time-tai "2001-01-01 01:01:01"))
(define time-tai2 (string->time-tai "2002-02-02 02:02:02"))
(define time-tai3 (string->time-tai "9999-12-31 23:59:59"))

; time-utc
(define time-utc1 (string->time-utc "2001-01-01 01:01:01"))
(define time-utc2 (string->time-utc "2002-02-02 02:02:02"))
(define time-utc3 (string->time-utc "9999-12-31 23:59:59"))

(define (escape-sql-name    . args) (send/apply database escape-sql-name    args))
(define (escape-sql-value   . args) (send/apply database escape-sql-value   args))
(define (parse-value        . args) (send/apply database parse-value        args))
(define (make-parser        . args) (send/apply database make-parser        args))
(define (query-sql          . args) (send/apply database query-sql          args))
(define (display-query      . args) (send/apply database display-query      args))
(define (display-distinct   . args) (send/apply database display-distinct   args))
(define (display-what       . args) (send/apply database display-what       args))
(define (display-from       . args) (send/apply database display-from       args))
(define (display-group      . args) (send/apply database display-group      args))
(define (display-order      . args) (send/apply database display-order      args))
(define (display-expression . args) (send/apply database display-expression args))

; any (listof column) -> string
(define (distinct-sql val imported)   (capture-sql display-distinct val imported))
(define (what-sql val imported)       (capture-sql display-what val imported))
(define (from-sql val imported)       (capture-sql display-from val imported))
(define (group-sql val imported)      (capture-sql display-group val imported))
(define (order-sql val imported)      (capture-sql display-order val imported))
(define (expression-sql val imported) (capture-sql display-expression val imported))

; entity-alias attribute-alias ...
(define-alias p1 person)
(define-alias p2 person)

; expression-alias
(define count-star    (sql:alias 'count-star (sql:count*)))
(define count-p1      (sql:alias 'count-p1 (sql:count* p1)))
(define count-p1-id   (sql:alias 'count-p1-id (sql:count (sql p1.guid))))
(define count-p2-id   (sql:alias 'count-p2-id (sql:count (sql p2.guid))))
(define sum-revisions (sql:alias 'sum-revisions (sql:+ (sql p1.revision) (sql p2.revision))))

; Tests ------------------------------------------

(define sql-escape-tests
  (test-suite "sql-escape<%>"
    
    (test-equal? "escape-sql-name"
      (escape-sql-name 'my-id)
      "\"my-id\"")
    
    (test-case "escape-sql-value : guid"
      (let ([t (entity-make-guid-type person #t)])
        (check-equal? (escape-sql-value t #f) "NULL")
        (check-equal? (escape-sql-value t (entity-make-guid person 123)) "123")
        (check-exn exn:fail? (cut escape-sql-value t (entity-make-guid course 123)))))
    
    (test-case "escape-sql-value : boolean"
      (let ([t (make-boolean-type #t)])
        (check-equal? (escape-sql-value t #f) "false")
        (check-equal? (escape-sql-value t #t) "true")
        (check-exn exn:fail:contract? (cut escape-sql-value t "123"))))
    
    (test-case "escape-sql-value : integer"
      (let ([t (make-integer-type #t #f #f)])
        (check-equal? (escape-sql-value t #f) "NULL")
        (check-equal? (escape-sql-value t 1) "1")
        (check-equal? (escape-sql-value t 0) "0")
        (check-equal? (escape-sql-value t -1) "-1")
        (check-exn exn:fail:contract? (cut escape-sql-value t "123"))))
    
    (test-case "escape-sql-value : real"
      (let ([t (make-real-type #t #f #f)])
        (check-equal? (escape-sql-value t #f) "NULL")
        (check-equal? (escape-sql-value t 0.00000000001) "1e-11")
        (check-equal? (escape-sql-value t 0.0) "0.0")
        (check-equal? (escape-sql-value t 123456789) "123456789")
        (check-exn exn:fail:contract? (cut escape-sql-value t "123"))))
    
    (test-case "escape-sql-value : string"
      (let ([t (make-string-type #t #f)])
        (check-equal? (escape-sql-value t #f) "NULL")
        (check-equal? (escape-sql-value t "") "''")
        (check-equal? (escape-sql-value t "Dave") "'Dave'")
        (check-equal? (escape-sql-value t "Dave's stuff") "'Dave''s stuff'")
        (check-equal? (escape-sql-value t "Dave's\\stuff") "'Dave''s\\stuff'")
        (check-equal? (escape-sql-value t "Dave's\nstuff") "'Dave''s\nstuff'")
        (check-equal? (escape-sql-value t "Dave's\n\rstuff") "'Dave''s\n\rstuff'")
        (check-exn exn:fail:contract? (cut escape-sql-value t 123))))
    
    (test-case "escape-sql-value : symbol"
      (let ([t (make-symbol-type #t #f)])
        (check-equal? (escape-sql-value t #f) "NULL")
        (check-equal? (escape-sql-value t '||) "''")
        (check-equal? (escape-sql-value t 'Dave) "'Dave'")
        (check-equal? (escape-sql-value t '|Dave's stuff|) "'Dave''s stuff'")
        (check-exn exn:fail:contract? (cut escape-sql-value t "123"))))
    
    (test-case "escape-sql-value : time-tai"
      (let ([t (make-time-tai-type #t)])
        (check-equal? (escape-sql-value t #f) "NULL")
        (check-equal? (escape-sql-value t (make-time time-tai 0 0)) "'1970-01-01 00:00:00.000000000'")
        (check-equal? (escape-sql-value t (make-time time-tai 1 0)) "'1970-01-01 00:00:00.000000001'")
        (check-equal? (escape-sql-value t (make-time time-tai 1000 0)) "'1970-01-01 00:00:00.000001000'")
        (check-equal? (escape-sql-value t (make-time time-tai 123456789 0)) "'1970-01-01 00:00:00.123456789'")
        (check-equal? (escape-sql-value t (make-time time-tai 123456789 1)) "'1970-01-01 00:00:01.123456789'")
        (check-equal? (escape-sql-value t time-tai1) "'2001-01-01 01:01:01.000000000'")
        (check-equal? (escape-sql-value t time-tai2) "'2002-02-02 02:02:02.000000000'")
        (check-equal? (escape-sql-value t time-tai3) "'9999-12-31 23:59:59.000000000'")
        (check-exn exn:fail:contract? (cut escape-sql-value t "123") "check 9")))
    
    (test-case "escape-sql-value : time-utc"
      (let ([t (make-time-utc-type #t)])
        (check-equal? (escape-sql-value t #f) "NULL")
        (check-equal? (escape-sql-value t (make-time time-utc 0 0)) "'1970-01-01 00:00:00.000000000'")
        (check-equal? (escape-sql-value t (make-time time-utc 1 0)) "'1970-01-01 00:00:00.000000001'")
        (check-equal? (escape-sql-value t (make-time time-utc 1000 0)) "'1970-01-01 00:00:00.000001000'")
        (check-equal? (escape-sql-value t (make-time time-utc 123456789 0)) "'1970-01-01 00:00:00.123456789'")
        (check-equal? (escape-sql-value t (make-time time-utc 123456789 1)) "'1970-01-01 00:00:01.123456789'")
        (check-equal? (escape-sql-value t time-utc1) "'2001-01-01 01:01:01.000000000'")
        (check-equal? (escape-sql-value t time-utc2) "'2002-02-02 02:02:02.000000000'")
        (check-equal? (escape-sql-value t time-utc3) "'9999-12-31 23:59:59.000000000'")
        (check-exn exn:fail:contract? (cut escape-sql-value t "123"))))
    
    (test-case "escape-sql-value : binary"
      (let ([t (make-binary-type #t)])
        (check-equal? (escape-sql-value t #f) "NULL")
        (check-equal? (escape-sql-value t #t) "CAST( E'((2) 0 () 0 () () #t)' AS bytea) ")
        (check-equal? (escape-sql-value t '(a b c)) "CAST( E'((2) 0 () 0 () () (c a c b c c))' AS bytea) ")
        (check-equal? (escape-sql-value t "Dave's stuff") "CAST( E'((2) 0 () 0 () () \"Dave\\'s stuff\")' AS bytea) ")
        (check-exn exn:fail:contract? (cut escape-sql-value t (lambda (x) (add1 x))))))
    
    (test-exn "escape-sql-value : unknown type"
      exn:fail:contract?
      (cut escape-sql-value 'foo "hello"))))

(define parse-tests
  (test-suite "parse<%>"
    
    (test-case "parse-value : boolean"
      (let ([t (make-boolean-type #t)])
        (check-equal? (parse-value t sql-null) #f)
        (check-equal? (parse-value t #t) #t)
        (check-equal? (parse-value t #f) #f)))
    
    (test-case "parse-value : integer"
      (let ([t (make-integer-type #t #f #f)])
        (check-equal? (parse-value t sql-null) #f)
        (check-equal? (parse-value t 1) 1)
        (check-equal? (parse-value t 0) 0)
        (check-equal? (parse-value t -1) -1)))
    
    (test-case "parse-value : real"
      (let ([t (make-real-type #t #f #f)])
        (check-equal? (parse-value t sql-null) #f)
        (check-equal? (parse-value t 0.00000000001) 0.00000000001)
        (check-equal? (parse-value t 0.0) 0.0)
        (check-equal? (parse-value t 123456789) 123456789)))
    
    (test-case "parse-value : string"
      (let ([t (make-string-type #t #f)])
        (check-equal? (parse-value t sql-null) #f)
        (check-equal? (parse-value t "") "")
        (check-equal? (parse-value t "Dave") "Dave")
        (check-equal? (parse-value t "Dave's stuff") "Dave's stuff")))
    
    (test-case "parse-value : symbol"
      (let ([t (make-symbol-type #t #f)])
        (check-equal? (parse-value t sql-null) #f)
        (check-equal? (parse-value t "") '||)
        (check-equal? (parse-value t "Dave") 'Dave)
        (check-equal? (parse-value t "Dave's stuff") '|Dave's stuff|)))
    
    (test-case "parse-value : time-tai"
      (let ([t (make-time-tai-type #t)])
        (check-exn exn:fail:contract?
          (cut parse-value t ""))
        (check-equal? (parse-value t (make-sql-timestamp 9999 12 31 23 59 59 0 0)) 
                      time-tai3
                      "check 2 failed")
        (check-equal? (parse-value t (make-sql-timestamp 1234 12 23 12 34 56 123456000 0))
                      (date->time-tai (make-date 123456000 56 34 12 23 12 1234 0)))))
    
    (test-case "parse-value : time-utc"
      (let ([t (make-time-utc-type #t)])
        (check-exn exn:fail:contract? (cut parse-value t ""))
        (check-equal? (parse-value t (make-sql-timestamp 1234 12 23 12 34 56 123456000 0))
                      (date->time-utc (make-date 123456000 56 34 12 23 12 1234 0)))))
    
    (test-case "parse-value : time-utc : 10/01/27"
      (let ([t (make-time-utc-type #t)])
        (check-equal? (escape-sql-value t (date->time-utc (string->date "27/01/10+0" "~d/~m/~Y~z")))
                      "'0010-01-27 00:00:00.000000000'")))
    
    (test-case "parse-value : binary"
      (let ([t (make-binary-type #t)])
        (check-equal? (parse-value t sql-null) #f)
        (check-equal? (parse-value t #"((2) 0 () 0 () () #t)") #t)
        (check-equal? (parse-value t #"((2) 0 () 0 () () (c a c b c c))") '(a b c))
        (check-equal? (parse-value t #"((2) 0 () 0 () () \"Dave\\'s stuff\")") "Dave's stuff")))

    (test-exn "parse-value : unknown type"
      exn:fail:contract?
      (cut parse-value 'foo "hello"))
    
    (test-case "make-parser"
      (let ([parse (make-parser (list (make-boolean-type #t)
                                      (make-integer-type #t #f #f)
                                      (make-string-type  #t #f)
                                      (make-symbol-type  #t #f)))])
        (check-equal? (parse (list #t 1 "1" "1"))
                      (list #t 1 "1" '|1|))
        (check-equal? (parse (list sql-null sql-null sql-null sql-null))
                      (list #f #f #f #f))))))

; Tests ----------------------------------------

; test-suite
(define sql-query-tests
  (test-suite "sql-query<%>"
    
    (test-case "display-distinct"
      (check-equal? (distinct-sql (list) (list)) 
                    "DISTINCT "
                    "no expressions")
      (check-equal? (distinct-sql (list (sql:= (sql p1.guid) 123)) (list (sql p1.guid)))
                    #<<ENDSQL
DISTINCT ON (("p1-guid" = 123)) 
ENDSQL
                    "single expression")
      (check-equal? (distinct-sql (list (sql:= (sql p1.guid) 123) (sql:= (sql p1.revision) 123)) (list (sql p1.guid)))
                    #<<ENDSQL
DISTINCT ON (("p1-guid" = 123), ("p1"."revision" = 123)) 
ENDSQL
                    "multiple expressions"))
    
    (test-case "display-what"
      (check-equal? (what-sql (list (sql p1.guid) (sql p1.revision) (sql p1.name)) (list (sql p1.name)))
                    #<<ENDSQL
"p1"."guid" AS "p1-guid", "p1"."revision" AS "p1-revision", "p1-name"
ENDSQL
                    "attribute aliases")
      
      (check-equal? (what-sql (list count-star count-p1 count-p1-id count-p2-id sum-revisions) (list count-p2-id))
                    #<<ENDSQL
count(*) AS "count-star", count("p1".*) AS "count-p1", count("p1"."guid") AS "count-p1-id", "count-p2-id", ("p1"."revision" + "p2"."revision") AS "sum-revisions"
ENDSQL
                    "expression aliases"))
    
    (test-case "display-from"
      (check-equal? (from-sql p1 null)
                    #<<ENDSQL
"person" AS "p1"
ENDSQL
                    "entity")
      
      (check-equal? (from-sql (sql:alias 'subq (sql:select #:from p1)) null)
                    #<<ENDSQL
(SELECT "p1"."guid" AS "p1-guid", "p1"."revision" AS "p1-revision", "p1"."name" AS "p1-name" FROM "person" AS "p1") AS "subq"
ENDSQL
                    "subquery")
      
      (check-equal? (from-sql (sql:inner p1 (sql:alias 'subq (sql:select #:from p2)) (sql:= (sql p1.guid) (sql p2.guid)))
                              (list (sql p2.guid) (sql p2.revision) (sql p2.name)))
                    #<<ENDSQL
("person" AS "p1" INNER JOIN (SELECT "p2"."guid" AS "p2-guid", "p2"."revision" AS "p2-revision", "p2"."name" AS "p2-name" FROM "person" AS "p2") AS "subq" ON ("p1"."guid" = "p2-guid"))
ENDSQL
                    "inner join"))
    
    (test-case "display-group"
      (check-equal? (group-sql (list (sql p1.guid) (sql p1.revision) (sql p1.name)) (list (sql p1.guid) (sql p1.revision) (sql p1.name)))
                    #<<ENDSQL
"p1-guid", "p1-revision", "p1-name"
ENDSQL
                    "attribute aliases")
      
      (check-equal? (group-sql (list count-p1-id count-p2-id sum-revisions) (list count-p1-id sum-revisions count-p2-id))
                    #<<ENDSQL
"count-p1-id", "count-p2-id", "sum-revisions"
ENDSQL
                    "expression aliases"))
    
    (test-case "display-order"
      (check-equal? (order-sql (list (sql:asc (sql p1.guid)) (sql:desc (sql p1.revision)) (sql:order (sql p1.name) 'asc)) (list (sql p1.name)))
                    #<<ENDSQL
"p1"."guid" ASC, "p1"."revision" DESC, "p1-name" ASC
ENDSQL
                    "attribute aliases")
      
      (check-equal? (order-sql (list (sql:asc count-p1-id) (sql:desc count-p2-id) (sql:order sum-revisions 'asc)) (list count-p1-id count-p2-id sum-revisions))
                    #<<ENDSQL
"count-p1-id" ASC, "count-p2-id" DESC, "sum-revisions" ASC
ENDSQL
                    "expression aliases")
      
      (check-equal? (order-sql (list (sql:asc (sql:+ (sql p1.revision) (sql p2.revision)))) null)
                    #<<ENDSQL
("p1"."revision" + "p2"."revision") ASC
ENDSQL
                    "expressions"))
    
    (test-case "display-expression"
      (check-equal? (expression-sql (sql:and (sql:= (sql p1.guid) 123) 
                                             (sql:= (sql:string-append (sql p1.name) " of Loxley") "Robin of Loxley"))
                                    (list (sql p1.name)))
                    "((\"p1\".\"guid\" = 123) AND ((\"p1-name\" || ' of Loxley') = 'Robin of Loxley'))"
                    "nested expressions")
      (check-equal? (expression-sql (sql:and) null) "true" "argumentless and")
      (check-equal? (expression-sql (sql:or) null) "false" "argumentless or")
      (check-equal? (expression-sql (sql:+) null) "0" "argumentless +")
      (check-equal? (expression-sql (sql:*) null) "1" "argumentless *")
      (check-equal? (expression-sql (sql:-) null) "0" "argumentless -")
      (check-equal? (expression-sql (sql:in (sql p1.guid) (sql:select #:what (sql p1.guid) #:from p1)) null)
                    "(\"p1\".\"guid\" IN (SELECT \"p1\".\"guid\" AS \"p1-guid\" FROM \"person\" AS \"p1\"))"
                    "sql:in")
      (check-equal? (expression-sql (sql:regexp-replace     "a" "b" "c") null)    "(regexp_replace('a', 'b', 'c'))"       "sql:regexp-replace")
      (check-equal? (expression-sql (sql:regexp-replace-ci  "a" "b" "c") null)    "(regexp_replace('a', 'b', 'c', 'i'))"  "sql:regexp-replace-ci")
      (check-equal? (expression-sql (sql:regexp-replace*    "a" "b" "c") null)    "(regexp_replace('a', 'b', 'c', 'g'))"  "sql:regexp-replace*")
      (check-equal? (expression-sql (sql:regexp-replace*-ci "a" "b" "c") null)    "(regexp_replace('a', 'b', 'c', 'gi'))" "sql:regexp-replace*-ci")
      (check-equal? (expression-sql (sql:->string           "a" "b")     null)    "(to_char('a', 'b'))"                   "sql:->string")
      (check-equal? (expression-sql (sql:->symbol           "a" "b")     null)    "(to_char('a', 'b'))"                   "sql:->symbol")
      (check-equal? (expression-sql (sql:cond [#t "a"] [#f "b"]) null)            "(CASE WHEN true THEN 'a' WHEN false THEN 'b' ELSE NULL END)" "sql:cond, no else")
      (check-equal? (expression-sql (sql:cond [#t "a"] [#f "b"] [else "c"]) null) "(CASE WHEN true THEN 'a' WHEN false THEN 'b' ELSE 'c' END)"  "sql:cond, with else"))
    
    (test-case "query-sql"
      (define-alias a person)
        (define-alias b pet)
        (define-alias expr (sql:count* b))
        
        (define query1
          (sql:select #:what   (list a b)
                      #:from   (sql:inner a b (sql:= (sql a.guid) (sql b.owner)))
                      #:where  (sql:= (sql a.name) "Jon Arbuckle")
                      #:order  (list (sql:asc (sql a.name))
                                     (sql:asc (sql b.name)))
                      #:limit  10
                      #:offset 20))
        
        (define query2
          (sql:select #:what   (list a b)
                      #:from   (sql:inner (sql:alias 'subq (sql:select #:from a)) b (sql:= (sql a.guid) (sql b.owner)))
                      #:where  (sql:= (sql a.name) (sql b.name))
                      #:order  (list (sql:asc (sql a.name))
                                     (sql:asc (sql b.name)))))
        
        (define query3
          (sql:select #:what  (list a expr)
                      #:from  (sql:inner a b (sql:= (sql a.guid) (sql b.owner)))
                      #:group (list a)))
        
        (define sql1
          #<<ENDSQL
SELECT "a"."guid" AS "a-guid", "a"."revision" AS "a-revision", "a"."name" AS "a-name", "b"."guid" AS "b-guid", "b"."revision" AS "b-revision", "b"."owner" AS "b-owner", "b"."name" AS "b-name" FROM ("person" AS "a" INNER JOIN "pet" AS "b" ON ("a"."guid" = "b"."owner")) WHERE ("a"."name" = 'Jon Arbuckle') ORDER BY "a-name" ASC, "b-name" ASC LIMIT 10 OFFSET 20
ENDSQL
          )
        
        (define sql2
          #<<ENDSQL
SELECT "a-guid", "a-revision", "a-name", "b"."guid" AS "b-guid", "b"."revision" AS "b-revision", "b"."owner" AS "b-owner", "b"."name" AS "b-name" FROM ((SELECT "a"."guid" AS "a-guid", "a"."revision" AS "a-revision", "a"."name" AS "a-name" FROM "person" AS "a") AS "subq" INNER JOIN "pet" AS "b" ON ("a-guid" = "b"."owner")) WHERE ("a-name" = "b"."name") ORDER BY "a-name" ASC, "b-name" ASC
ENDSQL
          )
        
        (define sql3
          #<<ENDSQL
SELECT "a"."guid" AS "a-guid", "a"."revision" AS "a-revision", "a"."name" AS "a-name", count("b".*) AS "expr" FROM ("person" AS "a" INNER JOIN "pet" AS "b" ON ("a"."guid" = "b"."owner")) GROUP BY "a-guid", "a-revision", "a-name"
ENDSQL
          )
        
        (check-equal? (capture-sql display-query query1) sql1 "display-query-sql of query1")
        (check-equal? (query-sql query1) (string-append sql1 ";") "query-sql of query1")
        (check-equal? (capture-sql display-query query2) sql2 "display-query-sql of query2")
        (check-equal? (query-sql query2) (string-append sql2 ";") "query-sql of query2")
        (check-equal? (capture-sql display-query query3) sql3 "display-query-sql of query3")
        (check-equal? (query-sql query3) (string-append sql3 ";") "query-sql of query3"))))

(define sql-tests
  (test-suite "sql"
    
    #:before
    (lambda ()
      (set! database (send (current-snooze) get-database)))
    
    sql-escape-tests
    sql-query-tests
    parse-tests))

; Provide statements -----------------------------

(provide sql-tests)
