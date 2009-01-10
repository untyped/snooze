;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

;; The following program demonstrates how to connect to a PostgreSQL backend
;; using spgsql and how to do simple queries on the connection.

(require "../spgsql.ss"
         (lib "class.ss"))
(print-struct #t)

;; Replace these values with the appropriate values for your 
;; configuration.
(define HOST "localhost")
(define PORT 5432)
(define DATABASE "ryan")
(define USER "ryan")
(define PASSWORD "secret")

(define cx (connect HOST PORT DATABASE USER PASSWORD))
; => connection% object, already connected

;; Set the connection object to display any messages it receives.
(send cx set-notice-handler display)

;; We'll do all our testing in a temporary table; will be dropped as soon
;; as we disconnect, and will not interfere with real tables, even of same 
;; name.

(define q-table-create
  (send cx exec
        "create temporary table the_numbers 
         (name integer, description varchar(80))"))

(send cx exec "insert into the_numbers values (0, 'nothing')")

;; A series of statements may be executed in one batch by separating them 
;; with semicolons.  Note that this has nothing to do with transactions.
(send cx exec
      "insert into the_numbers (name, description)
       values (1, 'multiplicative identity');
       insert into the_numbers
       values (2, 'the loneliest number since the number 1')")


(define q-select-even
  (send cx query
        "select name, description from the_numbers
        where name % 2 = 0"))
q-select-even
;; => 
;; (make-Recordset
;;   "SELECT"
;;   "blank"
;;   #f
;;   (list (list "name" 23) (list "description" 1043))
;;   (list
;;    (vector "2" "the loneliest number since the number 1")
;;    (vector "0" "nothing")))

(define q-fetch
  (send cx query-general
        "begin transaction;
         declare MC cursor for select * from the_numbers;
         move forward 1 in MC;
         fetch 1 in MC;
         commit transaction"))
q-fetch

;; You can have values automatically converted from strings into 
;; appropriate Scheme datatypes:

(send cx query-value "select 17")
;; => "17"
(send cx use-type-conversions #t)
(send cx query-value "select 17")
;; => 17

;; You can select single values, lists, etc
(send cx query-list "select name from the_numbers order by name")
(send cx query-value "select count(name) from the_numbers")
(send cx query-value "select now()")

;; Errors in queries are generally non-fatal:
(begin (with-handlers [(exn:spgsql? 
                        (lambda (e) (printf "~a~n" (exn-message e))))]
         (send cx query-value "select foo from NoSuchTable"))
       (send cx query-value "select 'okay to proceed!'"))

;; It's polite to disconnect, and probably helps avoid dangling tcp ports
(send cx disconnect)
