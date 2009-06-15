#lang scheme/base

(require "../test-base.ss")

(require srfi/19
         (sqlite-in sqlite)
         "../core/core.ss"
         "../sql/sql.ss"
         "sql.ss")

; Helpers --------------------------------------

; (U database<%> #f)
(define database #f)

; time-tai
(define time-tai1 (string->time-tai "2001-01-01 01:01:01"))
(define time-tai2 (string->time-tai "2002-02-02 02:02:02"))
(define time-tai3 (string->time-tai "9999-12-31 23:59:59"))

; time-utc
(define time-utc1 (string->time-utc "2001-01-01 01:01:01"))
(define time-utc2 (string->time-utc "2002-02-02 02:02:02"))
(define time-utc3 (string->time-utc "9999-12-31 23:59:59"))

; (a b c ... output-port -> void) a b c ... -> string
;
; Calls the supplied SQL display procedure, captures its output
; in an output-string, and returns it as a string.
(define (capture-sql proc . args)
  (define out (open-output-string))
  (apply proc (append args (list out)))
  (get-output-string out))

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
(define (distinct-sql val imported)   (capture-sql display-distinct   val imported))
(define (what-sql val imported)       (capture-sql display-what       val imported))
(define (from-sql val imported)       (capture-sql display-from       val imported))
(define (group-sql val imported)      (capture-sql display-group      val imported))
(define (order-sql val imported)      (capture-sql display-order      val imported))
(define (expression-sql val imported) (capture-sql display-expression val imported))

; entity-alias attribute-alias ...
(define-alias p1 person)
(define-alias p2 person)
(define-alias p3 person)

; expression-alias
(define-alias count-star    (sql (count*)))
(define-alias count-p1      (sql (count* p1)))
(define-alias count-p1-guid (sql (count p1.guid)))
(define-alias count-p2-guid (sql (count p2.guid)))
(define-alias sum-revisions (sql (+ p1.revision p2.revision)))

; Tests ------------------------------------------

(define sql-escape-tests
  (test-suite "sql-escape-<%>"
    
    (test-equal? "escape-sql-name"
      (escape-sql-name 'my-id)
      "[my-id]")
    
    (test-case "escape-sql-value : guid"
      (let ([t (make-guid-type #t #f person)])
        (check-equal? (escape-sql-value t #f) "NULL")
        (check-equal? (escape-sql-value t (entity-make-vanilla-guid person 123)) "123")
        (check-exn exn:fail:contract? (cut escape-sql-value t (entity-make-vanilla-guid course 123)))))
    
    (test-case "escape-sql-value : boolean"
      (let ([t (make-boolean-type #t #f)])
        (check-equal? (escape-sql-value t #f) "0" "check 1")
        (check-equal? (escape-sql-value t #t) "1" "check 2")
        (check-exn exn:fail:contract? (cut escape-sql-value t "123") "check 3")))
    
    (test-case "escape-sql-value : integer"
      (let ([t (make-integer-type #t #f)])
        (check-equal? (escape-sql-value t #f) "NULL" "check 1")
        (check-equal? (escape-sql-value t 1) "1" "check 2")
        (check-equal? (escape-sql-value t 0) "0" "check 3")
        (check-equal? (escape-sql-value t -1) "-1" "check 4")
        (check-exn exn:fail:contract? (cut escape-sql-value t "123") "check 5")))
    
    (test-case "escape-sql-value : real"
      (let ([t (make-real-type #t #f)])
        (check-equal? (escape-sql-value t #f) "NULL" "check 1")
        (check-equal? (escape-sql-value t 0.00000000001) "1e-11" "check 2")
        (check-equal? (escape-sql-value t 0.0) "0.0" "check 3")
        (check-equal? (escape-sql-value t 123456789) "123456789" "check 4")
        (check-exn exn:fail:contract? (cut escape-sql-value t "123") "check 5")))
    
    (test-case "escape-sql-value : string"
      (let ([t (make-string-type #t #f #f)])
        (check-equal? (escape-sql-value t #f) "NULL" "check 1")
        (check-equal? (escape-sql-value t "") "''" "check 2")
        (check-equal? (escape-sql-value t "Dave") "'Dave'" "check 3")
        (check-equal? (escape-sql-value t "Dave's stuff") "'Dave''s stuff'" "check 4")
        (check-exn exn:fail:contract? (cut escape-sql-value t 123) "check 5")))
    
    (test-case "escape-sql-value : symbol"
      (let ([t (make-symbol-type #t #f #f)])
        (check-equal? (escape-sql-value t #f) "NULL" "check 1")
        (check-equal? (escape-sql-value t '||) "''" "check 2")
        (check-equal? (escape-sql-value t 'Dave) "'Dave'" "check 3")
        (check-equal? (escape-sql-value t '|Dave's stuff|) "'Dave''s stuff'" "check 4")
        (check-exn exn:fail:contract? (cut escape-sql-value t "123") "check 5")))
    
    (test-case "escape-sql-value : time-tai"
      (let ([t (make-time-tai-type #t #f)])
        (check-equal? (escape-sql-value t #f) "NULL" "check 1")
        (check-equal? (escape-sql-value t (make-time time-tai 0 0)) "0000000000" "check 2")
        (check-equal? (escape-sql-value t (make-time time-tai 1 0)) "0000000001" "check 3")
        (check-equal? (escape-sql-value t (make-time time-tai 123456789 0)) "0123456789" "check 4")
        (check-equal? (escape-sql-value t (make-time time-tai 123456789 1)) "1123456789" "check 5")
        (check-equal? (escape-sql-value t time-tai1)  "978310893000000000" "check 6")
        (check-equal? (escape-sql-value t time-tai2) "1012615354000000000" "check 7")
        (check-exn exn:fail:contract? (cut escape-sql-value t "123") "check 8")))
    
    (test-case "escape-sql-value : time-utc"
      (let ([t (make-time-utc-type #t #f)])
        (check-equal? (escape-sql-value t #f) "NULL" "check 1")
        (check-equal? (escape-sql-value t (make-time time-utc 0 0)) "0000000000" "check 2")
        (check-equal? (escape-sql-value t (make-time time-utc 1 0)) "0000000001" "check 3")
        (check-equal? (escape-sql-value t (make-time time-utc 123456789 0)) "0123456789" "check 4")
        (check-equal? (escape-sql-value t (make-time time-utc 123456789 1)) "1123456789" "check 5")
        (check-equal? (escape-sql-value t time-utc1)  "978310861000000000" "check 6")
        (check-equal? (escape-sql-value t time-utc2) "1012615322000000000" "check 7")
        (check-exn exn:fail:contract? (cut escape-sql-value t "123") "check 8")))
    
    (test-exn "escape-sql-value : unknown type"
      exn:fail:contract?
      (cut escape-sql-value 'foo "hello"))))

(define parse-tests
  (test-suite "parse<%>"
    (test-case "parse-value : boolean"
      (let ([t (make-boolean-type #t #f)])
        (check-equal? (parse-value t "0") #f "check 1")
        (check-equal? (parse-value t "1") #t "check 2")
        (check-equal? (parse-value t #f) #f "check 3")))
    
    (test-case "parse-value : integer"
      (let ([t (make-integer-type #t #f)])
        (check-equal? (parse-value t #f) #f "check 1")
        (check-equal? (parse-value t "1") 1 "check 2")
        (check-equal? (parse-value t "0") 0 "check 3")
        (check-equal? (parse-value t "-1") -1 "check 4")))
    
    (test-case "parse-value : real"
      (let ([t (make-real-type #t #f)])
        (check-equal? (parse-value t #f) #f "check 1")
        (check-equal? (parse-value t "0.00000000001") 0.00000000001 "check 2")
        (check-equal? (parse-value t "0.0") 0.0 "check 3")
        (check-equal? (parse-value t "123456789") 123456789 "check 4")))
    
    (test-case "parse-value : string"
      (let ([t (make-string-type #t #f #f)])
        (check-equal? (parse-value t #f) #f "check 1")
        (check-equal? (parse-value t "") "" "check 2")
        (check-equal? (parse-value t "Dave") "Dave" "check 3")
        (check-equal? (parse-value t "Dave's stuff") "Dave's stuff" "check 4")))
    
    (test-case "parse-value : symbol"
      (let ([t (make-symbol-type #t #f #f)])
        (check-equal? (parse-value t #f) #f "check 1")
        (check-equal? (parse-value t "") '|| "check 2")
        (check-equal? (parse-value t "Dave") 'Dave "check 3")
        (check-equal? (parse-value t "Dave's stuff") '|Dave's stuff| "check 4")))
    
    (test-case "parse-value : time-tai"
      (let ([t (make-time-tai-type #t #f)])
        (check-equal? (parse-value t "") (make-time time-tai 0 0) "check 1")
        (check-equal? (parse-value t "0") (make-time time-tai 0 0) "check 2")
        (check-equal? (parse-value t "000000000") (make-time time-tai 0 0) "check 3")
        (check-equal? (parse-value t "0000000000") (make-time time-tai 0 0) "check 4")
        (check-equal? (parse-value t "1000000000") (make-time time-tai 0 1) "check 5")
        (check-equal? (parse-value t "123456789") (make-time time-tai 123456789 0) "check 6")
        (check-equal? (parse-value t "0123456789") (make-time time-tai 123456789 0) "check 7")
        (check-equal? (parse-value t "1123456789") (make-time time-tai 123456789 1) "check 8")
        (check-equal? (parse-value t (escape-sql-value t time-tai1)) time-tai1 "check 9")
        (check-equal? (parse-value t (escape-sql-value t time-tai2)) time-tai2 "check 10")
        (check-equal? (parse-value t (escape-sql-value t time-tai3)) time-tai3 "check 11")))
    
    (test-case "parse-value : time-utc"
      (let ([t (make-time-utc-type #t #f)])
        (check-equal? (parse-value t "") (make-time time-utc 0 0) "check 1")
        (check-equal? (parse-value t "0") (make-time time-utc 0 0) "check 2")
        (check-equal? (parse-value t "000000000") (make-time time-utc 0 0) "check 3")
        (check-equal? (parse-value t "0000000000") (make-time time-utc 0 0) "check 4")
        (check-equal? (parse-value t "1000000000") (make-time time-utc 0 1) "check 5")
        (check-equal? (parse-value t "123456789") (make-time time-utc 123456789 0) "check 6")
        (check-equal? (parse-value t "0123456789") (make-time time-utc 123456789 0) "check 7")
        (check-equal? (parse-value t "1123456789") (make-time time-utc 123456789 1) "check 8")
        (check-equal? (parse-value t (escape-sql-value t time-utc1)) time-utc1 "check 9")
        (check-equal? (parse-value t (escape-sql-value t time-utc2)) time-utc2 "check 10")
        (check-equal? (parse-value t (escape-sql-value t time-utc3)) time-utc3 "check 11")))
    
    (test-exn "parse-value : unknown type"
      exn:fail:contract?
      (cut parse-value 'foo "hello"))
    
    (test-case "make-parser"
      (let ([parse (make-parser (list (make-boolean-type #t #f)
                                      (make-integer-type #t #f)
                                      (make-string-type #t #f #f)
                                      (make-symbol-type #t #f #f)))])
        (check-equal? (parse (vector "1" "1" "1" "1")) (vector #t 1 "1" '|1|) "check 1")
        (check-equal? (parse (vector #f #f #f #f)) (vector #f #f #f #f) "check 1")))))

(define sql-query-tests
  (test-suite "sql-query<%>"
    
    (test-case "display-distinct"
      (check-equal? (distinct-sql (list) (list)) 
                    "DISTINCT "
                    "no expressions")
      (check-equal? (distinct-sql (list (sql (= p1.guid 123)))
                                  (list (sql p1.id)))
                    #<<ENDSQL
DISTINCT ON (([p1-guid] = 123)) 
ENDSQL
                    "single expression")
      (check-equal? (distinct-sql (list (sql (= p1.guid 123))
                                        (sql (= p1.revision 123)))
                                  (list (sql p1.guid)))
                    #<<ENDSQL
DISTINCT ON (([p1-guid] = 123), ([p1].[revision] = 123)) 
ENDSQL
                    "multiple expressions"))
    
    (test-case "display-what"
      (check-equal? (what-sql (list (sql p1.guid)
                                    (sql p1.revision)
                                    (sql p1.name))
                              (list (sql p1.name)))
                    #<<ENDSQL
[p1].[guid] AS [p1-guid], [p1].[revision] AS [p1-revision], [p1-name]
ENDSQL
                    "attribute aliases")
      
      (check-equal? (what-sql (list count-star
                                    count-p1
                                    count-p1-guid
                                    count-p2-guid
                                    sum-revisions)
                              (list count-p2-guid))
                    #<<ENDSQL
count(*) AS [count-star], count([p1].*) AS [count-p1], count([p1].[guid]) AS [count-p1-guid], [count-p2-guid], ([p1].[guid] + [p2].[guid]) AS [sum-ids]
ENDSQL
                    "expression aliases"))
    
    (test-case "display-from"
      (check-equal? (from-sql p1 null)
                    #<<ENDSQL
[Person] AS [p1]
ENDSQL
                    "entity")
      
      (check-equal? (from-sql (sql:alias 'subq (sql:select #:from p1)) null)
                    #<<ENDSQL
(SELECT [p1].[guid] AS [p1-guid], [p1].[revision] AS [p1-revision], [p1].[name] AS [p1-name] FROM [Person] AS [p1]) AS [subq]
ENDSQL
                    "subquery")
      
      (check-equal? (from-sql (sql (inner p1
                                          ,(sql:alias 'subq (sql (select #:from p2)))
                                          (= p1.guid p2.guid)))
                              (list (sql p2.guid)
                                    (sql p2.revision)
                                    (sql p2.name)))
                    #<<ENDSQL
[Person] AS [p1] INNER JOIN (SELECT [p2].[guid] AS [p2-guid], [p2].[revision] AS [p2-revision], [p2].[name] AS [p2-name] FROM [Person] AS [p2]) AS [subq] ON ([p1].[guid] = [p2-guid])
ENDSQL
                    "inner join")
      
      (check-equal? (from-sql (sql (outer (outer p1 p2) p3)) null)
                    #<<ENDSQL
[Person] AS [p1] CROSS JOIN [Person] AS [p2] CROSS JOIN [Person] AS [p3]
ENDSQL
                    "unparenthesised nested join")
      
      (check-equal? (from-sql (sql (outer p1 (outer p2 p3))) null)
                    #<<ENDSQL
((SELECT [p1].[guid] AS [p1-guid], [p1].[revision] AS [p1-revision], [p1].[name] AS [p1-name] FROM [Person] AS [p1]) CROSS JOIN ((SELECT [p2].[guid] AS [p2-guid], [p2].[revision] AS [p2-revision], [p2].[name] AS [p2-name] FROM [Person] AS [p2]) CROSS JOIN (SELECT [p3].[guid] AS [p3-id], [p3].[revision] AS [p3-revision], [p3].[name] AS [p3-name] FROM [Person] AS [p3])))
ENDSQL
                    "parenthesised nested join"))
    
    (test-case "display-group"
      (check-equal? (group-sql (list (sql p1.guid)
                                     (sql p1.revision)
                                     (sql p1.name))
                               (list (sql p1.guid)
                                     (sql p1.revision)
                                     (sql p1.name)))
                    #<<ENDSQL
[p1-guid], [p1-revision], [p1-name]
ENDSQL
                    "attribute aliases")
      
      (check-equal? (group-sql (list count-p1-guid
                                     count-p2-guid
                                     sum-revisions)
                               (list count-p1-guid
                                     sum-revisions
                                     count-p2-guid))
                    #<<ENDSQL
[count-p1-guid], [count-p2-guid], [sum-ids]
ENDSQL
                    "expression aliases"))
    
    (test-case "display-order"
      (check-equal? (order-sql (list (sql (asc   p1.guid))
                                     (sql (desc  p1.revision))
                                     (sql (order p1.name 'asc)))
                               (list (sql p1.name)))
                    #<<ENDSQL
[p1].[guid] ASC, [p1].[revision] DESC, [p1-name] ASC
ENDSQL
                    "attribute aliases")
      
      (check-equal? (order-sql (list (sql (asc   count-p1-guid))
                                     (sql (desc  count-p2-guid))
                                     (sql (order sum-revisions 'asc)))
                               (list count-p1-guid
                                     count-p2-guid
                                     sum-revisions))
                    #<<ENDSQL
[count-p1-guid] ASC, [count-p2-guid] DESC, [sum-ids] ASC
ENDSQL
                    "expression aliases")
      
      (check-equal? (order-sql (list (sql (asc (+ p1.guid p2.guid))))
                               null)
                    #<<ENDSQL
([p1].[guid] + [p2].[guid]) ASC
ENDSQL
                    "expressions"))
    
    (test-case "display-expression"
      (check-equal? (expression-sql (sql (and)) null) "true")
      (check-equal? (expression-sql (sql (or))  null) "false")
      (check-equal? (expression-sql (sql (+))   null) "0")
      (check-equal? (expression-sql (sql (*))   null) "1")
      (check-equal? (expression-sql (sql (-))   null) "0")
      (check-equal? (expression-sql (sql (regexp-replace     "a" "b" "c")) null)    "(regexp_replace('a', 'b', 'c'))")
      (check-equal? (expression-sql (sql (regexp-replace-ci  "a" "b" "c")) null)    "(regexp_replace('a', 'b', 'c', 'i'))")
      (check-equal? (expression-sql (sql (regexp-replace*    "a" "b" "c")) null)    "(regexp_replace('a', 'b', 'c', 'g'))")
      (check-equal? (expression-sql (sql (regexp-replace*-ci "a" "b" "c")) null)    "(regexp_replace('a', 'b', 'c', 'gi'))")
      (check-equal? (expression-sql (cond [#t "a"] [#f "b"]) null)            "(CASE WHEN 1 THEN 'a' WHEN 0 THEN 'b' ELSE NULL END)")
      (check-equal? (expression-sql (cond [#t "a"] [#f "b"] [else "c"]) null) "(CASE WHEN 1 THEN 'a' WHEN 0 THEN 'b' ELSE 'c' END)")
      (check-equal? (expression-sql (sql (in p1.guid (select #:what p1.guid #:from p1))) null)
                    "([p1].[guid] IN (SELECT [p1].[guid] AS [p1-guid] FROM [Person] AS [p1]))")
      (check-equal? (expression-sql (sql (and (= p1.guid 123)
                                              (= (string-append p1.name " of Loxley")
                                                 "Robin of Loxley")))
                                    (list (sql p1.name)))
                    "(([p1].[guid] = 123) AND (([p1-name] || ' of Loxley') = 'Robin of Loxley'))"))
    
    (test-case "query-sql"
      (define-alias a person)
      (define-alias b pet)
      (define-alias expr (sql (count* b)))
      
      (define-alias expr2 
        (sql (+ p1.guid p2.guid p3.guid)))
      
      (define query1
        (sql (select #:what   (a b)
                     #:from   (inner a b (= a.guid b.owner))
                     #:where  (= a.name "Jon Arbuckle")
                     #:order  ((asc a.name)
                               (asc b.name))
                     #:limit  10
                     #:offset 20)))
      
      (define query2
        (sql (select #:what   (a b)
                     #:from   (inner ,(sql:alias 'subq (sql (select #:from a))) b (= a.guid b.owner))
                     #:where  (= a.name b.name)
                     #:order  ((asc a.name)
                               (asc b.name)))))
      
      (define query3
        (sql (select #:what   (a expr)
                     #:from   (inner a b (= a.guid b.owner))
                     #:group  (a))))
      
      ; Test the special aliasing behaviour with right-nested joins:
      (define query4
        (sql (select #:what (p1.guid p2.guid p3.guid expr2)
                     #:from (outer p1 (outer p2 p3)))))
      
      (define sql1
        #<<ENDSQL
SELECT [a].[guid] AS [a-guid], [a].[revision] AS [a-revision], [a].[name] AS [a-name], [b].[guid] AS [b-guid], [b].[revision] AS [b-revision], [b].[owner] AS [b-owner], [b].[name] AS [b-name] FROM [Person] AS [a] INNER JOIN [Pet] AS [b] ON ([a].[guid] = [b].[owner]) WHERE ([a].[name] = 'Jon Arbuckle') ORDER BY [a-name] ASC, [b-name] ASC LIMIT 10 OFFSET 20
ENDSQL
        )
      
      (define sql2
        #<<ENDSQL
SELECT [a-guid], [a-revision], [a-name], [b].[guid] AS [b-guid], [b].[revision] AS [b-revision], [b].[owner] AS [b-owner], [b].[name] AS [b-name] FROM (SELECT [a].[guid] AS [a-guid], [a].[revision] AS [a-revision], [a].[name] AS [a-name] FROM [Person] AS [a]) AS [subq] INNER JOIN [Pet] AS [b] ON ([a-guid] = [b].[owner]) WHERE ([a-name] = [b].[name]) ORDER BY [a-name] ASC, [b-name] ASC
ENDSQL
        )
      
      (define sql3
        #<<ENDSQL
SELECT [a].[guid] AS [a-guid], [a].[revision] AS [a-revision], [a].[name] AS [a-name], count([b].*) AS [expr] FROM [Person] AS [a] INNER JOIN [Pet] AS [b] ON ([a].[guid] = [b].[owner]) GROUP BY [a-guid], [a-revision], [a-name]
ENDSQL
        )
      
      ; Test the special aliasing behaviour with right-nested joins:
      (define sql4
        #<<ENDSQL
SELECT [p1-guid], [p2-guid], [p3-id], ([p1-guid] + [p2-guid] + [p3-id]) AS [expr2] FROM ((SELECT [p1].[guid] AS [p1-guid], [p1].[revision] AS [p1-revision], [p1].[name] AS [p1-name] FROM [Person] AS [p1]) CROSS JOIN ((SELECT [p2].[guid] AS [p2-guid], [p2].[revision] AS [p2-revision], [p2].[name] AS [p2-name] FROM [Person] AS [p2]) CROSS JOIN (SELECT [p3].[guid] AS [p3-id], [p3].[revision] AS [p3-revision], [p3].[name] AS [p3-name] FROM [Person] AS [p3])))
ENDSQL
        )
      
      (check-equal? (capture-sql display-query query1) sql1)
      (check-equal? (capture-sql display-query query2) sql2)
      (check-equal? (capture-sql display-query query3) sql3)
      (check-equal? (capture-sql display-query query4) sql4)
      (check-equal? (query-sql query1) (string-append sql1 ";"))
      (check-equal? (query-sql query2) (string-append sql2 ";"))
      (check-equal? (query-sql query3) (string-append sql3 ";"))
      (check-equal? (query-sql query4) (string-append sql4 ";")))))

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
