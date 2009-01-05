(module query-lang-test mzscheme
  
  (require (lib "cut.ss" "srfi" "26"))
  
  (require (file "query-core.ss")
           (prefix q: (file "query-lang.ss"))
           (file "test-base.ss")
           (file "test-data.ss")
           (file "type.ss"))
  
  (provide query-lang-tests)
  
  ;; check-alias : regexp string -> (U list #f)
  (define-simple-check (check-alias regexp named)
    (regexp-match regexp (symbol->string (named-alias named))))
  
  ;; query-lang-tests : test-suite
  (define query-lang-tests
    (test-suite
     "query-lang.ss"

     (test-case
      "appropriate auto-aliases generated"
      (check-alias #rx"table[0-9]+"  (q:table 'table)         "table")
      (check-alias #rx"course[0-9]+" (q:entity entity:course) "entity"))
     
     (test-case
      "join: ON fields have to be defined on tables / fields from the arguments"
      (let ([table1 (q:table 'table1)]
            [table2 (q:table 'table2)]
            [table3 (q:table 'table3)])
        (check-not-exn
         (cut q:inner table1 table2
              (q:= (q:field table1 'field1 type:text)
                   (q:field table2 'field2 type:text))))
        (check-exn
         exn:fail:snooze?
         (cut q:inner table1 table2
              (q:= (q:field table1 'field1 type:text)
                   (q:field table3 'field2 type:text))))))
     
     (test-case
      "select: WHAT fields have to be defined on tables from the FROM clause"
      (let ([table1 (q:table 'table1)]
            [table2 (q:table 'table2)])
        (check-not-exn
         (cut q:select
              #:what (list (q:field table1 'field1 type:text))
              #:from table1)
         "check 1")
        (check-not-exn
         (cut q:select
              #:what (list (q:field table1 'field1 type:text))
              #:from (q:outer table1 table2))
         "check 2")
        (check-exn
         exn:fail:snooze?
         (cut q:select
              #:what (list (q:field table1 'field1 type:text))
              #:from table2)
         "check 3")
        ; Aggregates and fields from subqueries:
        (check-not-exn
         (lambda ()
           (let* ([field1 (q:field table1 'table1 type:integer/1)]
                  [agg1a  (q:count field1)]
                  [agg1b  (q:count* table1)]
                  [field2 (q:field table2 'field2 type:integer/1)]
                  [agg2a  (q:count field2)]
                  [agg2b  (q:count* table2)]
                  [subq   (q:select #:what (list field2 agg2a agg2b)
                                    #:from table2)])
             (q:select #:what (list field1 agg1a agg1b field2 agg2a agg2b)
                       #:from (q:outer table1 subq))))
         "check 4")))
     
     (test-case
      "select: single-item mode is equivalent to multi-item mode"
      (let ([table1 (q:table 'table1)]
            [entity1 (q:entity entity:course)])
        (let ([select1a (q:select
                         #:what (list (q:field table1 'field1 type:text))
                         #:from table1)]
              [select1b (q:select
                         #:what (q:field table1 'field1 type:text)
                         #:from table1)]
              [select2a (q:select
                         #:what (list entity1)
                         #:from entity1)]
              [select2b (q:select
                         #:what entity1
                         #:from entity1)])
          (check-false (select-single-item? select1a) "select1a is in multi-item mode")
          (check-true (select-single-item? select1b)  "select1b is in single-item mode")
          (check-false (select-single-item? select2a) "select2a is in multi-item mode")
          (check-true (select-single-item? select2b)  "select2b is in single-item mode")
          (check-equal? (select-what select1a) 
                        (select-what select1b)
                        "select1a and select1b have the same WHAT")
          (check-equal? (select-what select2a)
                        (select-what select2b)
                        "select2a and select2b have the same WHAT")
          (check-equal? (select-what-entities select1a) 
                        (select-what-entities select1b)
                        "select1a and select1b have the same WHAT entities")
          (check-equal? (select-what-entities select2a)
                        (select-what-entities select2b)
                        "select2a and select2b have the same WHAT entities"))))
     
     (test-case
      "select: omitting #:what causes it to default to #:from"
      (let* ([course  (q:entity entity:course)]
             [table   (q:table 'table)]
             [select1 (q:select #:from course)])
        (check-equal? (select-what-entities select1) 
                      (list entity:course)
                      "select-what-entities matches #:from")
        (check-exn exn:fail:contract?
                   (cut q:select #:from table)
                   "cannot omit #:what when #:from is not an entity")))
     
     (test-case
      "select: WHERE fields have to be from tables or subqueries in the FROM clause"
      (let* ([table1   (q:table 'table1)]
             [table2   (q:table 'table2)]
             [field1-1 (q:field table1 'field1 type:text)]
             [field1-2 (q:field table1 'field2 type:text)]
             [field2-1 (q:field table2 'field1 type:text)]
             [field2-2 (q:field table2 'field2 type:text)])
        (check-not-exn
         (cut q:select
              #:what  (list field1-1)
              #:from  table1
              #:where (q:= field1-2 1))
         "check 1")
        (check-exn
         exn:fail:snooze?
         (cut q:select
              #:what  (list field1-1)
              #:from  table1
              #:where (q:= field2-1 1))
         "check 2")
        (check-not-exn
         (cut q:select
              #:what  (list field1-1 field2-1)
              #:from  (q:outer table1 (q:select #:what (list field2-1) #:from table2))
              #:where (q:and (q:= field1-1 1)
                             (q:= field1-2 1)
                             (q:= field2-1 1)))
         "check 3")
        (check-exn
         exn:fail:snooze?
         (cut q:select
              #:what  (list field1-1 field2-1)
              #:from  (q:outer table1 (q:select #:what (list field2-1) #:from table2))
              #:where (q:and (q:= field1-1 1)
                             (q:= field1-2 1)
                             (q:= field2-1 1)
                             (q:= field2-2 1)))
         "check 4")))
     
     (test-case
      "select: WHERE aggregates have to be from subqueries in the FROM clause"
      (let* ([table1       (q:table 'table1)]
             [table2       (q:table 'table2)]
             [field1-1     (q:field table1 'field1 type:text)]
             [field1-2     (q:field table1 'field2 type:text)]
             [field2-1     (q:field table2 'field1 type:text)]
             [field2-2     (q:field table2 'field2 type:text)]
             [aggregate1-1 (q:count field1-1)])
        ; Selecting a field from a table in FROM is okay:
        (check-not-exn
         (cut q:select
              #:what  (list aggregate1-1)
              #:from  table1
              #:where (q:= field1-1 1))
         "check 1")
        ; Selecting an aggregate is not (no aggregates allowed in WHERE clauses):
        (check-exn
         exn:fail:snooze?
         (cut q:select
              #:what  (list aggregate1-1)
              #:from  table1
              #:where (q:= aggregate1-1 1))
         "check 2")
        ; Selecting an aggregate from a subquery is (the results have been cached, and
        ; the generated SQL uses the column alias rather than the aggregate function):
        (check-not-exn
         (cut q:select
              #:what  (list aggregate1-1)
              #:from  (q:select #:what (list aggregate1-1) #:from table1)
              #:where (q:= aggregate1-1 1))
         "check 3")))
     
     (test-case
      "select: ORDER fields have to be defined on tables or subqueries in the FROM clause"
      (let* ([table1   (q:table 'table1)]
             [table2   (q:table 'table2)]
             [field1-1 (q:field table1 'field1 type:text)]
             [field1-2 (q:field table1 'field2 type:text)]
             [field2-1 (q:field table2 'field1 type:text)]
             [field2-2 (q:field table2 'field2 type:text)])
        (check-not-exn
         (cut q:select
              #:what  (list field1-1)
              #:from  table1
              #:order (list (q:asc field1-2)))
         "check 1")
        (check-exn
         exn:fail:snooze?
         (cut q:select
              #:what  (list field1-1)
              #:from  table1
              #:order (list (q:asc field2-1)))
         "check 2")
        (check-not-exn
         (cut q:select
              #:what  (list field1-1)
              #:from  (q:outer table1 (q:select #:what (list field2-1) #:from table2))
              #:order (list (q:asc field1-2) (q:asc field2-1)))
         "check 3")
        (check-exn
         exn:fail:snooze?
         (cut q:select
              #:what  (list field1-1)
              #:from  (q:outer table1 (q:select #:what (list field2-1) #:from table2))
              #:order (list (q:asc field1-2) (q:asc field2-1) (q:asc field2-2)))
         "check 4")))
     
     (test-case
      "select: ORDER aggregates have to be from tables or subqueries in the FROM clause"
      (let* ([table1       (q:table 'table1)]
             [table2       (q:table 'table2)]
             [field1-1     (q:field table1 'field1 type:text)]
             [field1-2     (q:field table1 'field2 type:text)]
             [field2-1     (q:field table2 'field1 type:text)]
             [field2-2     (q:field table2 'field2 type:text)]
             [aggregate1-1 (q:count field1-1)]
             [aggregate2-1 (q:count field2-1)]
             [aggregate2-2 (q:count field2-2)])
        ; Ordering by an aggregate is okay:
        (check-not-exn
         (cut q:select
              #:what  (list aggregate1-1)
              #:from  table1
              #:order (list (q:asc aggregate1-1)))
         "check 1")
        ; Ordering by an aggregate from a different table is not:
        (check-exn
         exn:fail:snooze?
         (cut q:select
              #:what  (list aggregate1-1)
              #:from  table1
              #:order (list (q:asc aggregate2-1)))
         "check 2")
        ; Ordering by an aggregate from a subquery is okay:
        (check-not-exn
         (cut q:select
              #:what  (list aggregate1-1)
              #:from  (q:outer table1 (q:select #:what (list aggregate2-1) #:from table2))
              #:order (list (q:asc aggregate2-1)))
         "check 3")
        ; ... as long as it's an aggregate in the WHAT clause:
        (check-exn
         exn:fail:snooze?
         (cut q:select
              #:what  (list aggregate1-1)
              #:from  (q:outer table1 (q:select #:what (list aggregate2-1) #:from table2))
              #:order (list (q:asc aggregate2-2)))
         "check 4")))
     
     (test-case
      "select-what-types"
      (let ([table1  (q:table 'table1)]
            [entity1 (q:entity entity:programme)])
        (check-equal? (select-what-types (q:select #:what (list (q:field 'f1 table1 'field1 type:id)
                                                                (q:field 'f2 table1 'field2 type:revision)
                                                                (q:field 'f3 table1 'field3 type:text)
                                                                (q:field 'f4 table1 'field4 type:integer/1)
                                                                (q:field 'f5 table1 'field5 type:real)
                                                                (q:field 'f6 table1 'field6 type:symbol)
                                                                (q:field 'f7 table1 'field7 type:boolean/t)
                                                                (q:field 'f8 table1 'field8 type:time-tai))
                                                   #:from table1))
                      (list type:id type:revision type:text type:integer/1 type:real type:symbol type:boolean/t type:time-tai)
                      "q:fields")
        (check-equal? (select-what-types (q:select #:what (list entity1) #:from entity1))
                      (list type:id type:revision type:symbol type:text)
                      "q:entity")
        (check-equal? (select-what-types (q:select #:what entity1 #:from entity1))
                      (list type:id type:revision type:symbol type:text)
                      "q:entity in single-item mode")
        (check-equal? (select-what-types (q:select #:what (list (q:count   (q:field 'f4 table1 'field4 type:integer/1))
                                                                (q:count*  table1)
                                                                (q:max     (q:field 'f4 table1 'field4 type:integer/1))
                                                                (q:min     (q:field 'f4 table1 'field4 type:integer/1))
                                                                (q:max     (q:field 'f5 table1 'field5 type:real/1))
                                                                (q:min     (q:field 'f5 table1 'field5 type:real/1))
                                                                (q:average (q:field 'f4 table1 'field4 type:integer/1))
                                                                (q:average (q:field 'f5 table1 'field5 type:real/1)))
                                                   #:from table1))
                      (list type:integer type:integer type:integer type:integer type:real type:real type:real type:real)
                      "q:aggregates")))
     
     (test-case
      "attributes from subquery are recognised when used in enclosing query"
      (let* ([a     (q:entity entity:course)]
             [b     (q:entity entity:programme-structure)]
             [subq  (q:select #:what (list a) 
                              #:from a
                              #:limit  100
                              #:offset 200)])
        ; This should work as (q:attr a 'code) is part of subq:
        (check-not-exn 
         (lambda ()
           (q:select #:what (list a b)
                     #:from (q:inner subq b (q:= (q:attr a 'code) (q:attr b 'course-code))))))
        ; This shouldn't work as (q:attr a 'blah) isn't part of subq:
        (check-exn 
         exn:fail:snooze?
         (lambda ()
           (q:select #:what (list a b)
                     #:from (q:inner subq b (q:= (q:attr a 'blah) (q:attr b 'course-code))))))))
     
     ))
  
  )