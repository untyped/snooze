#lang scheme/base

(require mzlib/etc
         scheme/unit)

(require "../snooze.ss"
         "../test-base.ss"
         "../test-data.ss"
         "../generic/sql-query-helpers-sig.ss"
         "../generic/sql-query-unit.ss"
         "../generic/sql-query-sig.ss"
         "sql-data-unit.ss"
         "sql-name-unit.ss")

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
(define count-p1-id (sql:alias 'count-p1-id (sql:count (sql p1.guid))))
(define count-p2-id (sql:alias 'count-p2-id (sql:count (sql p2.guid))))
(define sum-ids (sql:alias 'sum-ids (sql:+ (sql p1.guid) (sql p2.guid))))

; Tests ----------------------------------------

; test-suite
(define sql-query-unit-tests
  (test-suite "sql-query-unit.ss"
    
    (test-case "display-distinct"
      (check-equal? (distinct-sql (list) (list)) 
                    "DISTINCT "
                    "no expressions")
      (check-equal? (distinct-sql (list (sql:= (sql p1.guid) 123)) (list (sql p1.guid)))
                    #<<ENDSQL
DISTINCT ON ("p1-id" = 123) 
ENDSQL
                    "single expression")
      (check-equal? (distinct-sql (list (sql:= (sql p1.guid) 123) (sql:= (sql p1.revision) 123)) (list (sql p1.guid)))
                    #<<ENDSQL
DISTINCT ON ("p1-id" = 123), ("p1"."revision" = 123) 
ENDSQL
                    "multiple expressions"))
    
    (test-case "display-what"
      (check-equal? (what-sql (list (sql p1.guid) (sql p1.revision) (sql p1.name)) (list (sql p1.name)))
                    #<<ENDSQL
"p1"."id" AS "p1-id", "p1"."revision" AS "p1-revision", "p1-name"
ENDSQL
                    "attribute aliases")
      
      (check-equal? (what-sql (list count-star count-p1 count-p1-id count-p2-id sum-ids) (list count-p2-id))
                    #<<ENDSQL
count(*) AS "count-star", count("p1".*) AS "count-p1", count("p1"."id") AS "count-p1-id", "count-p2-id", ("p1"."id" + "p2"."id") AS "sum-ids"
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
      
      (check-equal? (from-sql (sql:inner p1 (sql:alias 'subq (sql:select #:from p2)) (sql:= (sql p1.guid) (sql p2.guid)))
                              (list (sql p2.guid) (sql p2.revision) (sql p2.name)))
                    #<<ENDSQL
("Person" AS "p1" INNER JOIN (SELECT "p2"."id" AS "p2-id", "p2"."revision" AS "p2-revision", "p2"."name" AS "p2-name" FROM "Person" AS "p2") AS "subq" ON ("p1"."id" = "p2-id"))
ENDSQL
                    "inner join"))
    
    (test-case "display-group"
      (check-equal? (group-sql (list (sql p1.guid) (sql p1.revision) (sql p1.name)) (list (sql p1.guid) (sql p1.revision) (sql p1.name)))
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
      (check-equal? (order-sql (list (sql:asc (sql p1.guid)) (sql:desc (sql p1.revision)) (sql:order (sql p1.name) 'asc)) (list (sql p1.name)))
                    #<<ENDSQL
"p1"."id" ASC, "p1"."revision" DESC, "p1-name" ASC
ENDSQL
                    "attribute aliases")
      
      (check-equal? (order-sql (list (sql:asc count-p1-id) (sql:desc count-p2-id) (sql:order sum-ids 'asc)) (list count-p1-id count-p2-id sum-ids))
                    #<<ENDSQL
"count-p1-id" ASC, "count-p2-id" DESC, "sum-ids" ASC
ENDSQL
                    "expression aliases")
      
      (check-equal? (order-sql (list (sql:asc (sql:+ (sql p1.guid) (sql p2.guid)))) null)
                    #<<ENDSQL
("p1"."id" + "p2"."id") ASC
ENDSQL
                    "expressions"))
    
    (test-case "display-expression"
      (check-equal? (expression-sql (sql:and (sql:= (sql p1.guid) 123) 
                                             (sql:= (sql:string-append (sql p1.name) " of Loxley") "Robin of Loxley"))
                                    (list (sql p1.name)))
                    "((\"p1\".\"id\" = 123) AND ((\"p1-name\" || ' of Loxley') = 'Robin of Loxley'))"
                    "nested expressions")
      (check-equal? (expression-sql (sql:and) null) "true" "argumentless and")
      (check-equal? (expression-sql (sql:or) null) "false" "argumentless or")
      (check-equal? (expression-sql (sql:+) null) "0" "argumentless +")
      (check-equal? (expression-sql (sql:*) null) "1" "argumentless *")
      (check-equal? (expression-sql (sql:-) null) "0" "argumentless -")
      (check-equal? (expression-sql (sql:in (sql p1.guid) (sql:select #:what (sql p1.guid) #:from p1)) null)
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
                      #:from   (sql:inner a b (sql:= (sql a.guid) (sql b.owner-id)))
                      #:where  (sql:= (sql a.name) "Jon Arbuckle")
                      #:order  (list (sql:asc (sql a.name))
                                     (sql:asc (sql b.name)))
                      #:limit  10
                      #:offset 20))
        
        (define query2
          (sql:select #:what   (list a b)
                      #:from   (sql:inner (sql:alias 'subq (sql:select #:from a)) b (sql:= (sql a.guid) (sql b.owner-id)))
                      #:where  (sql:= (sql a.name) (sql b.name))
                      #:order  (list (sql:asc (sql a.name))
                                     (sql:asc (sql b.name)))))
        
        (define query3
          (sql:select #:what  (list a expr)
                      #:from  (sql:inner a b (sql:= (sql a.guid) (sql b.owner-id)))
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
SELECT "a"."id" AS "a-id", "a"."revision" AS "a-revision", "a"."name" AS "a-name", count("b".*) AS "expr" FROM ("Person" AS "a" INNER JOIN "Pet" AS "b" ON ("a"."id" = "b"."ownerID")) GROUP BY "a-id", "a-revision", "a-name"
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
