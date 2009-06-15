#lang scheme/base

(require mzlib/etc
         scheme/unit)

(require "../snooze.ss"
         "../test-base.ss"
         "../common/sql-query-helpers-sig.ss"
         "../common/sql-query-sig.ss"
         "sql-data-unit.ss"
         "sql-name-unit.ss"
         "sql-query-unit.ss")

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
(define-alias p3 person)

; expression-alias
(define-alias count-star    (sql (count*)))
(define-alias count-p1      (sql (count* p1)))
(define-alias count-p1-guid (sql (count p1.guid)))
(define-alias count-p2-guid (sql (count p2.guid)))
(define-alias sum-revisions (sql (+ p1.revision p2.revision)))

; Tests ----------------------------------------

; test-suite
(define sql-query-unit-tests
  (test-suite "sql-query-unit.ss"
    
    (test-case "display-distinct"
      (check-equal? (distinct-sql (list) (list)) 
                    "DISTINCT "
                    "no expressions")
      (check-equal? (distinct-sql (list (sql (= p1.guid 123)))
                                  (list (sql p1.id)))
                    #<<ENDSQL
DISTINCT ON ([p1-guid] = 123) 
ENDSQL
                    "single expression")
      (check-equal? (distinct-sql (list (sql (= p1.guid 123))
                                        (sql (= p1.revision 123)))
                                  (list (sql p1.guid)))
                    #<<ENDSQL
DISTINCT ON ([p1-guid] = 123), ([p1].[revision] = 123) 
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
      (begin-with-definitions
        
        (define-alias a person)
        (define-alias b pet)
        (define-alias expr (sql (count* b)))
        
        (define-alias expr2 
          (sql (+ p1.guid p2.guid p3.guid)))
        
        (define query1
          (sql (select #:what   (a b)
                       #:from   (inner a b (= a.guid b.owner-id))
                       #:where  (= a.name "Jon Arbuckle")
                       #:order  ((asc a.name)
                                 (asc b.name))
                       #:limit  10
                       #:offset 20)))
        
        (define query2
          (sql (select #:what   (a b)
                       #:from   (inner ,(sql:alias 'subq (sql (select #:from a))) b (= a.guid b.owner-id))
                       #:where  (= a.name b.name)
                       #:order  ((asc a.name)
                                 (asc b.name)))))
        
        (define query3
          (sql (select #:what   (a expr)
                       #:from   (inner a b (= a.guid b.owner-id))
                       #:group  (a))))
        
        ; Test the special aliasing behaviour with right-nested joins:
        (define query4
          (sql (select #:what (p1.guid p2.guid p3.guid expr2)
                       #:from (outer p1 (outer p2 p3)))))
        
        (define sql1
          #<<ENDSQL
SELECT [a].[guid] AS [a-guid], [a].[revision] AS [a-revision], [a].[name] AS [a-name], [b].[guid] AS [b-guid], [b].[revision] AS [b-revision], [b].[ownerID] AS [b-owner-id], [b].[name] AS [b-name] FROM [Person] AS [a] INNER JOIN [Pet] AS [b] ON ([a].[guid] = [b].[ownerID]) WHERE ([a].[name] = 'Jon Arbuckle') ORDER BY [a-name] ASC, [b-name] ASC LIMIT 10 OFFSET 20
ENDSQL
          )
        
        (define sql2
          #<<ENDSQL
SELECT [a-guid], [a-revision], [a-name], [b].[guid] AS [b-guid], [b].[revision] AS [b-revision], [b].[ownerID] AS [b-owner-id], [b].[name] AS [b-name] FROM (SELECT [a].[guid] AS [a-guid], [a].[revision] AS [a-revision], [a].[name] AS [a-name] FROM [Person] AS [a]) AS [subq] INNER JOIN [Pet] AS [b] ON ([a-guid] = [b].[ownerID]) WHERE ([a-name] = [b].[name]) ORDER BY [a-name] ASC, [b-name] ASC
ENDSQL
          )
        
        (define sql3
          #<<ENDSQL
SELECT [a].[guid] AS [a-guid], [a].[revision] AS [a-revision], [a].[name] AS [a-name], count([b].*) AS [expr] FROM [Person] AS [a] INNER JOIN [Pet] AS [b] ON ([a].[guid] = [b].[ownerID]) GROUP BY [a-guid], [a-revision], [a-name]
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
        (check-equal? (query-sql query4) (string-append sql4 ";"))))))

; Provide statements -----------------------------

(provide sql-query-unit-tests)
