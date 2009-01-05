#lang scheme/unit

(require scheme/match
         (planet untyped/unlib:3/time)
         (planet untyped/unlib:3/profile)
         "../base.ss"
         "../era/era.ss"
         "../generic/sql-data-sig.ss"
         "../generic/sql-name-sig.ss"
         "../generic/sql-query-helpers-sig.ss"
         "../generic/sql-query-sig.ss"
         (only-in "../sql/sql.ss" sql:select)
         "../sql/sql-struct.ss")

; SQLite has an irritating feature where it rewrites parenthesised FROM statements as "SELECT * FROM ...".
; This means it loses aliases on the joined tables. We get around this using two strategies:
;
; The simplest strategy is to avoid parentheses in FROM clauses.
; This is only possible if there are no right-nested joins.
;
; If there *are* right-nested joins, we adopt a different approach. We wrap each entity- and query-alias
; in a SELECT statement (which is what SQLite does behind the scenes with parenthesised joins anyway),
; and we alias all the columns there and then. This effectively means we're importing all columns from all
; subqueries and reproviding them from the main query.

(import sql-data^
        sql-name^)

(export sql-query^
        sql-query-helpers^)

; Public -----------------------------------------

; query -> string
(define (query-sql query)
  (let ([out (open-output-string)])
    (display-query query out)
    (display ";" out)
    (get-output-string out)))

; Helpers ----------------------------------------

; query output-port -> void
(define (display-query query out)
  (define what      (query-what     query))
  (define distinct  (query-distinct query))
  (define from      (query-from     query))
  (define where     (query-where    query))
  (define group     (query-group    query))
  (define order     (query-order    query))
  (define having    (query-having   query))
  (define limit     (query-limit    query))
  (define offset    (query-offset   query))
  ; Determine which aliasing strategy we're going to use
  ; (see the comments at the top of the file):
  (define parenthesised?
    (parenthesise-join? from))
  ; If were using the parenthesised strategy, we're effectively
  ; importing all bindings from entities as well as subqueries:
  (define imported  
    (if parenthesised?
        (append (query-local-columns query)
                (query-imported-columns query))
        (query-imported-columns query)))
  (define imported* (append what imported))
  (display "SELECT " out)
  (when distinct
    (display-distinct distinct imported* out))
  (display-what what imported out)
  (display " FROM " out)
  (display-from from imported out parenthesised?)
  (when where
    (display " WHERE " out)
    (display-expression where imported out))
  (unless (null? group)
    (display " GROUP BY " out)
    (display-group group imported* out))
  (when having
    (display " HAVING " out)
    (display-expression imported* out))
  (unless (null? order)
    (display " ORDER BY " out)
    (display-order order imported* out))
  (when limit
    (display " LIMIT " out)
    (display limit out))
  (when offset
    (display " OFFSET " out)
    (display offset out)))

; DISTINCT clause --------------------------------

; (U expression boolean) (listof column) output-port -> void
(define (display-distinct distinct imported out)
  (if (null? distinct)
      (begin (display "DISTINCT " out))
      (begin (display "DISTINCT ON " out)
             (display-expression (car distinct) imported out)
             (let loop ([distinct (cdr distinct)])
               (unless (null? distinct)
                 (display ", " out)
                 (display-expression (car distinct) imported out)
                 (loop (cdr distinct))))
             (display " " out))))

; WHAT clause ------------------------------------

; (listof column) (listof column) output-port -> void
;
; Displays an SQL fragment for a "WHAT" statement (the bit between "SELECT" and "FROM").
; Doesn't include the words "SELECT" or "FROM".
;
; The "declared" argument is a list of fields that have been declared in subqueries.
(define (display-what what imported out)
  (let loop ([what what] [first? #t])
    (unless (null? what)
      (unless first?
        (display ", " out))
      (display-what-item (car what) imported out)
      (loop (cdr what) #f))))

; column (listof column) output-port -> void
(define (display-what-item column imported out)
  (unless (member column imported)
    (if (attribute-alias? column)
        (display-expression column imported out)
        (display-expression (expression-alias-value column) imported out))
    (display " AS " out))
  (display (escape-name (column-name column)) out))

; FROM clause ------------------------------------

; source (listof column) output-port boolean -> void
;
; Displays an SQL fragment for a FROM statement. Doesn't include the word "FROM".
;
; The parenthesise? argument indicates which aliasing strategy we're using
; (see the comments at the top of the file).
(define (display-from from imported out [parenthesise? (parenthesise-join? from)])
  (cond [(join? from)         (display-from/join from imported out parenthesise?)]
        [(entity-alias? from) (display-from/entity from out parenthesise?)]
        [(query-alias? from)  (display-from/query from out parenthesise?)]
        [else          (raise-exn exn:fail:contract
                         (format "Expected source, received ~a" from))]))

; join (listof column) output-port -> void
;
; The parenthesise? argument indicates which aliasing strategy we're using
; (see the comments at the top of the file).
(define (display-from/join the-join imported out parenthesise?)
  (match the-join
    [(struct join (op left right on))
     (when parenthesise?
       (display "(" out))
     (display-from left imported out parenthesise?)
     (cond [(eq? op 'inner) (display " INNER JOIN " out)]
           [(eq? op 'left)  (display " LEFT JOIN "  out)]
           [(eq? op 'right) (display " RIGHT JOIN " out)]
           [(eq? op 'outer) (display " CROSS JOIN " out)]
           [else            (raise-exn exn:fail:contract
                              (format "Join operator: expected (U 'inner 'outer 'left 'right), received ~a" op))])
     (display-from right imported out parenthesise?)
     (unless (eq? op 'outer)
       (display " ON " out)
       (display-expression on imported out))
     (when parenthesise?
       (display ")" out))]))

; entity-alias output-port boolean -> void
;
; The parenthesise? argument indicates which aliasing strategy we're using
; (see the comments at the top of the file).
(define (display-from/entity alias out parenthesise?)
  (match alias
    [(struct entity-alias (id entity))
     (when parenthesise?
       (display "(SELECT " out)
       (display-what (map (cut make-attribute-alias alias <>)
                          (entity-attributes entity))
                     null out)
       (display " FROM " out))
     (display (escape-name (entity-table-name entity)) out)
     (display " AS " out)
     (display (escape-name id) out)
     (when parenthesise?
       (display ")" out))]))

; query-alias output-port boolean -> void
;
; The parenthesise? argument indicates which aliasing strategy we're using
; (see the comments at the top of the file).
(define (display-from/query alias out parenthesise?)
  (match alias
    [(struct query-alias (id query))
     (display "(" out)
     (display-query query out)
     (display ")" out)
     (unless parenthesise?
       (display " AS " out)
       (display (escape-name id) out))]))

; source -> boolean
(define (parenthesise-join? from)
  (and (join? from)
       (or (join? (join-right from))
           (parenthesise-join? (join-left from))
           (parenthesise-join? (join-right from)))))

; GROUP clause -----------------------------------

; (listof column) (listof column) output-port -> void
;
; Displays an SQL fragment for an GROUP BY statement. Doesn't include the words "GROUP BY".
(define (display-group group imported out)
  (let loop ([group group] [first? #t])
    (unless (null? group)
      (unless first?
        (display ", " out))
      (display-expression (car group) imported out)
      (loop (cdr group) #f))))

; ORDER clause -----------------------------------

; (listof order) (listof column) output-port -> void
;
; Displays an SQL fragment for an ORDER BY statement. Doesn't include the word "ORDER BY".
(define (display-order order imported out)
  (let loop ([order order] [first? #t])
    (unless (null? order)
      (unless first?
        (display ", " out))
      (display-order-item (car order) imported out)
      (loop (cdr order) #f))))

; order (listof column) output-port -> void
(define (display-order-item the-order imported out)
  (match the-order
    [(struct order (expr dir))
     (display-expression expr imported out)
     (if (eq? dir 'asc)
         (display " ASC" out)
         (display " DESC" out))]))

; Helpers ----------------------------------------

; expression (listof column) output-port -> void
(define (display-expression expr imported out)
  (cond [(literal? expr)          (display-expression/literal          expr out)]
        [(aggregate? expr)        (display-expression/aggregate        expr imported out)]
        [(function? expr)         (display-expression/function         expr imported out)]
        [(attribute-alias? expr)  (display-expression/attribute-alias  expr imported out)]
        [(expression-alias? expr) (display-expression/expression-alias expr imported out)]
        [(source-alias? expr)     (display-expression/source-alias     expr out)]
        [else                     (raise-exn exn:fail:contract (format "Expected expression, received ~a" expr))]))

; literal output-port -> void
(define (display-expression/literal lit out)
  (match lit
    [(struct literal (type val))
     (display (escape-value type val) out)]))

; expression (listof column) output-port -> void
(define (display-expression/aggregate agg imported out)
  (match agg
    [(struct aggregate (type op args))
     (case op
       [(count)   (display "count(" out)]
       [(count*)  (display "count(" out)]
       [(max)     (display "max(" out)]
       [(min)     (display "min(" out)]
       [(average) (display "average(" out)]
       [else      (raise-exn exn:fail:contract (format "Unknown aggregate operator: ~a" op))])
     (case op
       [(count*)  (if (null? args)
                      (begin (display "*)" out))
                      (begin (display-expression (car args) imported out)
                             (display ".*)" out)))]
       [else      (display-expression (car args) imported out)
                  (display ")" out)])]))

; function (listof column) output-port -> void
(define (display-expression/function func imported out)
  (match func
    [(struct function (type op args))
     (case op
       [(and)                (display-expression/infix  op args " AND " "true" imported out)]
       [(or)                 (display-expression/infix  op args " OR " "false" imported out)]
       [(not)                (display-expression/outfix op args "NOT " #f imported out)]
       [(+)                  (display-expression/infix  op args " + " "0" imported out)]
       [(-)                  (display-expression/infix  op args " - " "0" imported out)]
       [(*)                  (display-expression/infix  op args " * " "1" imported out)]
       [(/)                  (display-expression/infix  op args " / " #f imported out)]
       [(abs)                (display-expression/outfix op args "abs(" ")" imported out)]
       [(floor)              (display-expression/outfix op args "floor(" ")" imported out)]
       [(ceiling)            (display-expression/outfix op args "ceiling(" ")" imported out)]
       [(round)              (display-expression/outfix op args "round(" ")" imported out)]
       [(=)                  (display-expression/infix  op args " = " #f imported out)]
       [(<>)                 (display-expression/infix  op args " <> " #f imported out)]
       [(<)                  (display-expression/infix  op args " < " #f imported out)]
       [(>)                  (display-expression/infix  op args " > " #f imported out)]
       [(<=)                 (display-expression/infix  op args " <= " #f imported out)]
       [(>=)                 (display-expression/infix  op args " >= " #f imported out)]
       [(like)               (display-expression/infix  op args " LIKE " #f imported out)]
       [(regexp-match)       (display-expression/infix  op args " ~ " #f imported out)]
       [(regexp-match-ci)    (display-expression/infix  op args " ~* " #f imported out)]
       [(regexp-replace)     (display-expression/outfix op args "regexp_replace(" ")" imported out)]
       [(regexp-replace-ci)  (display-expression/outfix op args "regexp_replace(" ", 'i')" imported out)]
       [(regexp-replace*)    (display-expression/outfix op args "regexp_replace(" ", 'g')" imported out)]
       [(regexp-replace*-ci) (display-expression/outfix op args "regexp_replace(" ", 'gi')" imported out)]
       [(string-append)      (display-expression/infix  op args " || " "''" imported out)]
       [(string-replace)     (display-expression/outfix op args "replace(" ")" imported out)]
       [(null?)              (display-expression/outfix op args #f " IS NULL" imported out)]
       [(coalesce)           (display-expression/outfix op args "coalesce(" ")" imported out)]
       [(->string)           (display-expression/outfix op args "to_char(" ")" imported out)]
       [(->symbol)           (display-expression/outfix op args "to_char(" ")" imported out)]
       [(in)                 (display-expression/in     (car args) (cadr args) imported out)]
       [(if)                 (display-expression/if     (car args) (cadr args) (caddr args) imported out)]
       [else                 (raise-exn exn:fail:contract (format "Unknown function operator: ~a" op))])]))

; symbol (listof expression) string (U string #f) (listof column) output-port -> void
(define (display-expression/infix op args delim default imported out)
  (if (pair? args)
      (begin
        (display "(" out)
        (let loop ([args args] [first? #t])
          (unless (null? args)
            (unless first?
              (display delim out))
            (display-expression (car args) imported out)
            (loop (cdr args) #f)))
        (display ")" out))
      (if default
          (display default out)
          (error (format "~a: expected one or more arguments, received ~s" op args)))))
  
; symbol (listof expression) string string (listof column) output-port -> void
(define (display-expression/outfix op args prefix suffix imported out)
  (display "(" out)
  (when prefix
    (display prefix out))
  (let loop ([args args])
    (unless (null? args)
      (display-expression (car args) imported out)
      (unless (null? (cdr args))
        (display ", " out))
      (loop (cdr args))))
  (when suffix
    (display suffix out))
  (display ")" out))

; expression (U query (listof expression)) (listof column) output-port -> void
(define (display-expression/in item items imported out)
  (match items
    [(? null?)
     (display "(false)" out)]
    [(? pair?)
     (display "(" out)
     (display-expression item imported out)
     (display " IN (" out)
     (let loop ([items items] [first? #t])
       (unless (null? items)
         (unless first?
           (display ", " out))
         (display-expression (car items) imported out)
         (loop (cdr items) #f)))
     (display "))" out)]
    [(? query?) 
     (display "(" out)
     (display-expression item imported out)
     (display " IN (" out)
     (display-query items out)
     (display "))" out)]
    [(? query-alias?) 
     (display "(" out)
     (display-expression item imported out)
     (display " IN " out)
     (display-expression items imported out)
     (display ")" out)]))

; expression expression expression (listof column) output-port -> void
(define (display-expression/if test pos neg imported out)
  (display "(CASE" out)
  (let loop ([test test] [pos pos] [neg neg])
    (display " WHEN " out)
    (display-expression test imported out)
    (display " THEN " out)
    (display-expression pos imported out)
    (if (and (function? neg) (eq? (function-op neg) 'if))
        (begin (apply loop (function-args neg)))
        (begin (display " ELSE " out)
               (display-expression neg imported out))))
  (display " END)" out))

; attribute-alias (listof column) output-port -> void
(define (display-expression/attribute-alias alias imported out)
  (if (member alias imported)
      (begin (display (escape-name (column-name alias)) out))
      (begin (display (escape-name (source-alias-name (attribute-alias-entity alias))) out)
             (display "." out)
             (display (escape-name (attribute-column-name (attribute-alias-attribute alias))) out))))

; attribute-alias (listof column) output-port -> void
(define (display-expression/expression-alias alias imported out)
  (if (member alias imported)
      (display (escape-name (column-name alias)) out)
      (raise-exn exn:fail:contract
        (format "Expression alias not in scope: ~s" alias))))

; source-alias output-port -> void
(define (display-expression/source-alias alias out)
  (display (escape-name (source-alias-name alias)) out))
