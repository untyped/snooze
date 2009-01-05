(module sql-select-internals-unit-test mzscheme
  
  (require (lib "unitsig.ss"))
  
  (require (prefix q: (file "../query-lang.ss"))
           (file "../test-base.ss")
           (file "../test-data.ss")
           (file "../type.ss")
           (file "sql-quote-unit.ss")
           (file "sql-select-internals-unit.ss")
           (file "sql-sig.ss"))
  
  (provide sql-select-internals-unit-tests)

  (define-values/invoke-unit/sig sql-select-internals^ 
    (compound-unit/sig
      (import)
      (link (quote : sql-quote^ (sql-quote@))
            (internals : sql-select-internals^ (sql-select-internals@ quote)))
      (export (open internals))))
  
  ; Test suite -----------------------------------
  
  ;; get-sql : (a b c ... output-port -> void) a b c ... -> string
  ;;
  ;; Calls the supplied SQL display procedure, captures its output
  ;; in an output-string, and returns it as a string.
  (define (get-sql proc . args)
    (define out
      (open-output-string))
    (apply proc (append args (list out)))
    (get-output-string out))
  
  (define sql-select-internals-unit-tests
    (test-suite
     "sql-select-internals-unit.ss"

     (test-case
      "WHAT"
      (let* ([table1     (q:table  't1 'table1)]
             [field1     (q:field  'f1 table1 'field1 type:text)]
             [aggregate1 (q:count  'a1 field1)]
             [aggregate2 (q:count* 'a2 table1)]
             [aggregate3 (q:count* 'a3 (q:select #:alias 'subq #:what (list field1) #:from table1))])
        (check-equal? (get-sql display-what-sql (list field1) null)
                      "`t1`.`field1` AS `f1`"
                      "local field")
        (check-equal? (get-sql display-what-sql (list field1) (list field1))
                      "`f1`"
                      "subquery field")
        (check-equal? (get-sql display-what-sql (list aggregate1) null)
                      "COUNT(`t1`.`field1`) AS `a1`"
                      "local aggregate")
        (check-equal? (get-sql display-what-sql (list aggregate1) (list aggregate1))
                      "`a1`"
                      "subquery aggregate")
        (check-equal? (get-sql display-what-sql (list aggregate1) (list field1))
                      "COUNT(`f1`) AS `a1`"
                      "local aggregate of subquery field")
        (check-equal? (get-sql display-what-sql (list aggregate2) (list field1))
                      "COUNT(`t1`.*) AS `a2`"
                      "COUNT(*) of a table")
        (check-equal? (get-sql display-what-sql (list aggregate3) null)
                      "COUNT(`subq`.*) AS `a3`"
                      "COUNT(*) of a subquery")))
     
     (test-case
      "FROM"
      (let* ([table1 (q:table 't1 'table1)]
             [table2 (q:table 't2 'table2)]
             [field1 (q:field 'f1 table1 'field1 type:integer)]
             [field2 (q:field 'f2 table2 'field2 type:integer)]
             [on1    (q:= field1 field2)])
        (check-equal? (get-sql display-from-sql table1 null)
                      "`table1` AS `t1`"
                      "table")
        (check-equal? (get-sql display-from-sql (q:outer table1 table2) null)
                      "(`table1` AS `t1` OUTER JOIN `table2` AS `t2`)"
                      "outer join")
        (check-equal? (get-sql display-from-sql (q:inner table1 table2 on1) null)
                      "(`table1` AS `t1` INNER JOIN `table2` AS `t2` ON (`t1`.`field1` = `t2`.`field2`))"
                      "inner join")
        (check-equal? (get-sql display-from-sql (q:left table1 table2 on1) null)
                      "(`table1` AS `t1` LEFT JOIN `table2` AS `t2` ON (`t1`.`field1` = `t2`.`field2`))"
                      "left join")
        (check-equal? (get-sql display-from-sql (q:right table1 table2 on1) null)
                      "(`table1` AS `t1` RIGHT JOIN `table2` AS `t2` ON (`t1`.`field1` = `t2`.`field2`))"
                      "right join")
        (check-equal? (get-sql display-from-sql (q:inner table1 table2 on1) (list field1))
                      "(`table1` AS `t1` INNER JOIN `table2` AS `t2` ON (`f1` = `t2`.`field2`))"
                      "inner join with fields from subqueries")))
     
     (test-case
      "WHERE"
      (let* ([table1 (q:table 't1 'table1)]
             [table2 (q:table 't2 'table2)]
             [field1 (q:field 'f1 table1 'field1 type:integer)]
             [field2 (q:field 'f2 table2 'field2 type:integer)]
             [on1    (q:= field1 field2)])
        (check-equal? (get-sql display-expr-sql (q:= field1 field2) (list field1))
                      "(`f1` = `t2`.`field2`)"
                      "local and subquery fields")
        (check-equal? (get-sql display-expr-sql (q:<> field1 "Rock 'n' roll.") (list field1))
                      "(`f1` <> 'Rock ''n'' roll.')"
                      "string literal with quotes in it")
        (check-equal? (get-sql display-expr-sql (q:in field1 (q:select #:what (list field2) #:from table2)) (list field1))
                      "(`f1` IN (SELECT `t2`.`field2` AS `f2` FROM `table2` AS `t2`))"
                      "IN statement")))
     
     (test-case
      "GROUP"
      (let* ([table1     (q:table   't1 'table1)]
             [table2     (q:table   't2 'table2)]
             [field1     (q:field   'f1 table1 'field1 type:integer)]
             [field2     (q:field   'f2 table2 'field2 type:integer)]
             [aggregate1 (q:max     'a1 field1)]
             [aggregate2 (q:min     'a2 field2)]
             [aggregate3 (q:average 'a3 field1)])
        (check-equal? (get-sql display-group-sql (list field1 field2) (list field1))
                      "`f1`, `t2`.`field2`"
                      "local and subquery fields")
        (check-equal? (get-sql display-group-sql
                               (list aggregate1 aggregate2 aggregate3)
                               (list aggregate1 field1))
                      "`a1`, MIN(`t2`.`field2`), AVERAGE(`f1`)"
                      "local and subquery aggregates")))
     
     (test-case
      "ORDER"
      (let* ([table1     (q:table   't1 'table1)]
             [table2     (q:table   't2 'table2)]
             [field1     (q:field   'f1 table1 'field1 type:integer)]
             [field2     (q:field   'f2 table2 'field2 type:integer)]
             [aggregate1 (q:max     'a1 field1)]
             [aggregate2 (q:min     'a2 field2)]
             [aggregate3 (q:average 'a3 field1)])
        (check-equal? (get-sql display-order-sql (list (q:asc field1) (q:desc field2)) (list field1))
                      "`f1` ASC, `t2`.`field2` DESC"
                      "local and subquery fields")
        (check-equal? (get-sql display-order-sql
                               (list (q:asc aggregate1) (q:desc aggregate2) (q:desc aggregate3))
                               (list aggregate1 field1))
                      "`a1` ASC, MIN(`t2`.`field2`) DESC, AVERAGE(`f1`) DESC"
                      "local and subquery aggregates")))
     
     (test-case
      "SELECT"
      (let* ([a (q:entity 'a entity:programme)]
             [b (q:entity 'b entity:programme-structure)])
        (check-equal? (get-sql display-select-sql 
                               (q:select #:what   (list a b)
                                         #:from   (q:inner a b (q:= (q:attr a 'code) (q:attr b 'programme-code)))
                                         #:where  (q:like (q:attr a 'name) "Biology%")
                                         #:order  (list (q:asc (q:attr a 'code)) (q:asc (q:attr b 'year)))
                                         #:limit  10
                                         #:offset 20))
                      #<<ENDSQL
SELECT `a`.`id` AS `a-id`, `a`.`revision` AS `a-revision`, `a`.`code` AS `a-code`, `a`.`name` AS `a-name`, `b`.`id` AS `b-id`, `b`.`revision` AS `b-revision`, `b`.`course-code` AS `b-course-code`, `b`.`programme-code` AS `b-programme-code`, `b`.`year` AS `b-year`, `b`.`compulsory` AS `b-compulsory` FROM (`programme` AS `a` INNER JOIN `programme-structure` AS `b` ON (`a`.`code` = `b`.`programme-code`)) WHERE (`a`.`name` LIKE 'Biology%') ORDER BY `a`.`code` ASC, `b`.`year` ASC LIMIT 10 OFFSET 20
ENDSQL
                      )))
     
     (test-case
      "SELECT with subquery"
      (let* ([a (q:entity 'a entity:programme)]
             [b (q:entity 'b entity:programme-structure)])
        (check-equal? (get-sql display-select-sql 
                               (q:select #:what   (list a b)
                                         #:from   (q:inner (q:select #:alias  'subq
                                                                     #:what   (list a)
                                                                     #:from   a
                                                                     #:limit  10
                                                                     #:offset 20)
                                                           b
                                                           (q:= (q:attr a 'code) (q:attr b 'programme-code)))   
                                         #:where  (q:like (q:attr a 'name) "Biology%")
                                         #:order  (list (q:asc (q:attr a 'code)) (q:asc (q:attr b 'year)))))
                      #<<ENDSQL
SELECT `a-id`, `a-revision`, `a-code`, `a-name`, `b`.`id` AS `b-id`, `b`.`revision` AS `b-revision`, `b`.`course-code` AS `b-course-code`, `b`.`programme-code` AS `b-programme-code`, `b`.`year` AS `b-year`, `b`.`compulsory` AS `b-compulsory` FROM ((SELECT `a`.`id` AS `a-id`, `a`.`revision` AS `a-revision`, `a`.`code` AS `a-code`, `a`.`name` AS `a-name` FROM `programme` AS `a` LIMIT 10 OFFSET 20) AS `subq` INNER JOIN `programme-structure` AS `b` ON (`a-code` = `b`.`programme-code`)) WHERE (`a-name` LIKE 'Biology%') ORDER BY `a-code` ASC, `b`.`year` ASC
ENDSQL
                      )))

     ))

  )
 