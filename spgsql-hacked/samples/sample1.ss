;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

;; The following program demonstrates how to connect to a PostgreSQL backend
;; using spgsql and how to do simple queries on the connection.

(require "../spgsql.ss"
         (lib "class.ss"))

;; Replace these values with the appropriate values for your 
;; configuration.
(define cx (connect #:user "ryan"
                    #:database "ryan"
                    #:password (getenv "PGPASSWORD")))
cx
; => connection object

;; We'll do all our work with a temporary table.
;; It will be dropped as soon as we disconnect.

;; Use the 'exec' method when you want to execute a SQL statement that
;; doesn't return a recordset.

(send cx exec
      "create temporary table the_numbers 
       (n integer, description varchar(80))")

(send cx exec "insert into the_numbers values (0, 'nothing')")

;; You can use 'exec' to perform several queries at once.
(send cx exec
  "insert into the_numbers (n, description) values (1, 'unity')"
  "insert into the_numbers values (2, 'the loneliest number since the number 1')")

;; The 'query' method gives you the most information, but it's the least
;; pleasant to use.

(send cx query "select n, description from the_numbers where n % 2 = 0")
;; => 
;; (make-Recordset
;;   (list (make-FieldInfo "n") (make-FieldInfo "description"))
;;   (list
;;    (vector 2 "the loneliest number since the number 1")
;;    (vector 0 "nothing")))

;; If you know a query returns exactly one row, you can use 'query-rows'
;; to get just that row.

(send cx query-row "select * from the_numbers where n = 0")
;; => (vector 0 "nothing")

;; If you know that a query returns exactly one column, you can use
;; 'query-list' to get just the list of values.

(send cx query-list "select description from the_numbers order by n")
;; => (list "nothing" "unity" "the loneliest number since the number 1")

;; If you know that a query returns just a single value (one row, 
;; one column), then you get use 'query-value'.

(send cx query-value "select count(*) from the_numbers")
;; => 3

(send cx query-value "select now()")
;; well, it depends, doesn't it?

;; If you aren't sure whether a row exists, you can use 'query-maybe-row'
;; or 'query-maybe-value'.

(send cx query-maybe-row "select * from the_numbers where n = 1")
;; => (vector 1 "unity")

(send cx query-maybe-row "select * from the_numbers where n = 5")
;; => #f
;; The 'query-row' method would have raised an error.

(send cx query-maybe-value "select description from the_numbers where n = 5")
;; => #f
;; The 'query-value' method would have raised an error here, too.

;; Errors in queries are generally non-fatal.

(begin (with-handlers [(exn:fail?
                        (lambda (e) (printf "~a~n" (exn-message e))))]
         (send cx query-value "select NoSuchField from NoSuchTable"))
       (send cx query-value "select 'okay to proceed!'"))

;; There are higher-order query methods to help process results.

(send cx map "select n1.n, n2.n from the_numbers n1, the_numbers n2"
      (lambda (x y) (list x y)))
;; => the cartesian products of {0,1,2} with itself

(send cx mapfilter "select n1.n, n2.n from the_numbers n1, the_numbers n2"
      list
      (lambda (x y) (= 2 (+ x y))))
;; => all ordered pairs from {0,1,2} that sum to 2

(send cx fold "select n from the_numbers" + 0)
;; => 3

;; You can create parameterized queries and apply them to values later.

(define all-less-than
  (send cx prepare-query-list "select n from the_numbers where n < $1"))

(define next-largest
  (send cx prepare-query-maybe-value 
        "select n from the_numbers where n < $1 order by n desc limit 1"))

(all-less-than 4)
;; => (list 0 1 2)

(all-less-than 1)
;; => (list 0)

(next-largest 4)
;; => 2

(next-largest 0)
;; => #f

;; There's another way to do that, of course:

(define next-largest2
  (send cx prepare-query-value
        "select max(n) from the_numbers where n < $1"))

(next-largest2 4)
;; => 2

;; But if there are no numbers less than the one given, 'max' returns NULL.

(next-largest2 0)
;; => sql-null

;; If you want to construct a query from Scheme data, you should probably 
;; use a parameterized query.

;; But spgsql still provides a way of constructing SQL strings.

(define (next-largest3 cut-off)
  (send cx query-value
        (format-sql "select max(n) from the_numbers where n < ~a"
                    [int cut-off])))

(next-largest3 4)
;; => 2

;; Sometimes you want to put things in a SQL string that aren't 
;; scalars. Here's an example of inserting the field name to be returned.

(send cx query-list
      (format-sql "select ~a from the_numbers" [#:name "description"]))
;; => (list "nothing" "unity" "the loneliest number since the number 1")

;; You can also splice in an entire piece of SQL code.

(send cx query-list
      (format-sql "select n from the_numbers ~a"
                  [#:sql (if 'order-descending
                             "order by n desc"
                             "")]))
;; => (list 2 1 0)

;; If you like, you can declare cursors to fetch data incrementally.
;; Usually, you must be inside of a transaction to create a cursor.

(send cx exec
      "begin transaction"
      "declare MC cursor for select * from the_numbers order by n"
      "move forward 1 in MC")
(send cx query-row "fetch 1 in MC")
;; => (vector 1 "unity")
(send cx exec
      "close MC"
      "commit transaction")

;; You should disconnect when you're done to close the communication ports.
(send cx disconnect)
