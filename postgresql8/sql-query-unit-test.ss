#lang scheme/base

(require mzlib/etc
         scheme/unit)

(require (file "../snooze.ss")
         (file "../test-base.ss")
         (file "../test-data.ss")
         (file "../generic/sql-query-helpers-sig.ss")
         (file "../generic/sql-query-unit.ss")
         (file "../generic/sql-query-sig.ss")
         (file "sql-data-unit.ss")
         (file "sql-name-unit.ss"))

; Unit invocations -----------------------------

(define-compound-unit/infer query@
  (import)
  (export sql-query^ sql-query-helpers^)
  (link sql-name@ sql-data@ sql-query@))

(define-values/invoke-unit/infer query@)

; Helpers --------------------------------------

; (a b c ... output-port -> void) a b c ... -> string
;
; Calls the supplied SQL display procedure, captures its output
; in an output-string, and returns it as a string.
(define (capture-sql proc . args)
  (define out (open-output-string))
  (apply proc (append args (list out)))
  (get-output-string out))

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

; expression-alias
(define count-star (sql:alias 'count-star (sql:count*)))
(define count-p1 (sql:alias 'count-p1 (sql:count* p1)))
(define count-p1-id (sql:alias 'count-p1-id (sql:count p1-id)))
(define count-p2-id (sql:alias 'count-p2-id (sql:count p2-id)))
(define sum-ids (sql:alias 'sum-ids (sql:+ p1-id p2-id)))

; Tests ----------------------------------------

; test-suite
(define sql-query-unit-tests
  (test-suite "sql-query-unit.ss"
    
    (test-case "display-distinct"
      (check-equal? (distinct-sql (list) (list)) 
                    "DISTINCT "
                    "no expressions")
      (check-equal? (distinct-sql (list (sql:= p1-id 123)) (list p1-id))
                    #<<ENDSQL
DISTINCT ON ("p1-id" = 123) 
ENDSQL
                    "single expression")
      (check-equal? (distinct-sql (list (sql:= p1-id 123) (sql:= p1-revision 123)) (list p1-id))
                    #<<ENDSQL
DISTINCT ON ("p1-id" = 123), ("p1"."revision" = 123) 
ENDSQL
                    "multiple expressions"))
    
    (test-case "display-what"
      (check-equal? (what-sql (list p1-id p1-revision p1-name) (list p1-name))
                    #<<ENDSQL
"p1"."id" AS "p1-id", "p1"."revision" AS "p1-revision", "p1-name"
ENDSQL
                    "attribute aliases")
      
      (check-equal? (what-sql (list count-star count-p1 count-p1-id count-p2-id sum-ids) (list count-p2-id))
                    #<<ENDSQL
COUNT(*) AS "count-star", COUNT("p1".*) AS "count-p1", COUNT("p1"."id") AS "count-p1-id", "count-p2-id", ("p1"."id" + "p2"."id") AS "sum-ids"
ENDSQL
                    "expression aliases"))
    
    (test-case "display-from"
      (check-equal? (from-sql p1 null)
                    #<<ENDSQL
"Person" AS "p1"
ENDSQL
                    "entity")
      
      (check-equal? (from-sql (sql:alias 'subq (sql:select #:from p1)) null)
                    #<<ENDSQL
(SELECT "p1"."id" AS "p1-id", "p1"."revision" AS "p1-revision", "p1"."name" AS "p1-name" FROM "Person" AS "p1") AS "subq"
ENDSQL
                    "subquery")
      
      (check-equal? (from-sql (sql:inner p1 (sql:alias 'subq (sql:select #:from p2)) (sql:= p1-id p2-id))
                              (list p2-id p2-revision p2-name))
                    #<<ENDSQL
("Person" AS "p1" INNER JOIN (SELECT "p2"."id" AS "p2-id", "p2"."revision" AS "p2-revision", "p2"."name" AS "p2-name" FROM "Person" AS "p2") AS "subq" ON ("p1"."id" = "p2-id"))
ENDSQL
                    "inner join"))
    
    (test-case "display-group"
      (check-equal? (group-sql (list p1-id p1-revision p1-name) (list p1-id p1-revision p1-name))
                    #<<ENDSQL
"p1-id", "p1-revision", "p1-name"
ENDSQL
                    "attribute aliases")
      
      (check-equal? (group-sql (list count-p1-id count-p2-id sum-ids) (list count-p1-id sum-ids count-p2-id))
                    #<<ENDSQL
"count-p1-id", "count-p2-id", "sum-ids"
ENDSQL
                    "expression aliases"))
    
    (test-case "display-order"
      (check-equal? (order-sql (list (sql:asc p1-id) (sql:desc p1-revision) (sql:order p1-name 'asc)) (list p1-name))
                    #<<ENDSQL
"p1"."id" ASC, "p1"."revision" DESC, "p1-name" ASC
ENDSQL
                    "attribute aliases")
      
      (check-equal? (order-sql (list (sql:asc count-p1-id) (sql:desc count-p2-id) (sql:order sum-ids 'asc)) (list count-p1-id count-p2-id sum-ids))
                    #<<ENDSQL
"count-p1-id" ASC, "count-p2-id" DESC, "sum-ids" ASC
ENDSQL
                    "expression aliases")
      
      (check-equal? (order-sql (list (sql:asc (sql:+ p1-id p2-id))) null)
                    #<<ENDSQL
("p1"."id" + "p2"."id") ASC
ENDSQL
                    "expressions"))
    
    (test-case "display-expression"
      (check-equal? (expression-sql (sql:and (sql:= p1-id 123) 
                                             (sql:= (sql:append p1-name " of Loxley") "Robin of Loxley"))
                                    (list p1-name))
                    "((\"p1\".\"id\" = 123) AND ((\"p1-name\" || ' of Loxley') = 'Robin of Loxley'))"
                    "nested expressions")
      (check-equal? (expression-sql (sql:in p1-id (sql:select #:what p1-id #:from p1)) null)
                    "(\"p1\".\"id\" IN (SELECT \"p1\".\"id\" AS \"p1-id\" FROM \"Person\" AS \"p1\"))"
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
      (begin-with-definitions
        
        (define-alias a person)
        (define-alias b pet)
        (define-alias expr (sql:count* b))
        
        (define query1
          (sql:select #:what   (list a b)
                      #:from   (sql:inner a b (sql:= a-id b-owner-id))
                      #:where  (sql:= a-name "Jon Arbuckle")
                      #:order  (list (sql:asc a-name)
                                     (sql:asc b-name))
                      #:limit  10
                      #:offset 20))
        
        (define query2
          (sql:select #:what   (list a b)
                      #:from   (sql:inner (sql:alias 'subq (sql:select #:from a)) b (sql:= a-id b-owner-id))
                      #:where  (sql:= a-name b-name)
                      #:order  (list (sql:asc a-name)
                                     (sql:asc b-name))))
        
        (define query3
          (sql:select #:what  (list a expr)
                      #:from  (sql:inner a b (sql:= a-id b-owner-id))
                      #:group (list a)))
        
        (define sql1
          #<<ENDSQL
SELECT "a"."id" AS "a-id", "a"."revision" AS "a-revision", "a"."name" AS "a-name", "b"."id" AS "b-id", "b"."revision" AS "b-revision", "b"."ownerID" AS "b-owner-id", "b"."name" AS "b-name" FROM ("Person" AS "a" INNER JOIN "Pet" AS "b" ON ("a"."id" = "b"."ownerID")) WHERE ("a"."name" = 'Jon Arbuckle') ORDER BY "a-name" ASC, "b-name" ASC LIMIT 10 OFFSET 20
ENDSQL
          )
        
        (define sql2
          #<<ENDSQL
SELECT "a-id", "a-revision", "a-name", "b"."id" AS "b-id", "b"."revision" AS "b-revision", "b"."ownerID" AS "b-owner-id", "b"."name" AS "b-name" FROM ((SELECT "a"."id" AS "a-id", "a"."revision" AS "a-revision", "a"."name" AS "a-name" FROM "Person" AS "a") AS "subq" INNER JOIN "Pet" AS "b" ON ("a-id" = "b"."ownerID")) WHERE ("a-name" = "b"."name") ORDER BY "a-name" ASC, "b-name" ASC
ENDSQL
          )
        
        (define sql3
          #<<ENDSQL
SELECT "a"."id" AS "a-id", "a"."revision" AS "a-revision", "a"."name" AS "a-name", COUNT("b".*) AS "expr" FROM ("Person" AS "a" INNER JOIN "Pet" AS "b" ON ("a"."id" = "b"."ownerID")) GROUP BY "a-id", "a-revision", "a-name"
ENDSQL
          )
        
        (check-equal? (capture-sql display-query query1) sql1 "display-query-sql of query1")
        (check-equal? (query-sql query1) (string-append sql1 ";") "query-sql of query1")
        (check-equal? (capture-sql display-query query2) sql2 "display-query-sql of query2")
        (check-equal? (query-sql query2) (string-append sql2 ";") "query-sql of query2")
        (check-equal? (capture-sql display-query query3) sql3 "display-query-sql of query3")
        (check-equal? (query-sql query3) (string-append sql3 ";") "query-sql of query3")))))

; Provide statements -----------------------------

(provide sql-query-unit-tests)
