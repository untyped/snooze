#lang scheme/base

(require "../base.ss")

(require (unlib-in time profile)
         "../core/struct.ss"
         "../common/common.ss"
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

(define sqlite3-sql-query-mixin
  (mixin (generic-database<%> sql-escape<%> sql-query<%>) (sql-query<%>)
    
    ; query output-port -> void
    (define/override (display-query query out)
      (let* ([what           (query-what     query)]
             [distinct       (query-distinct query)]
             [from           (query-from     query)]
             [where          (query-where    query)]
             [group          (query-group    query)]
             [order          (query-order    query)]
             [having         (query-having   query)]
             [limit          (query-limit    query)]
             [offset         (query-offset   query)]
             ; Determine which aliasing strategy we're going to use
             ; (see the comments at the top of the file):
             [parenthesised? (parenthesise-join? from)]
             ; If were using the parenthesised strategy, we're effectively
             ; importing all bindings from entities as well as subqueries:
             [imported       (if parenthesised?
                                 (append (query-local-columns query)
                                         (query-imported-columns query))
                                 (query-imported-columns query))]
             [imported*      (append what imported)])
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
          (display offset out))))
    
    ; FROM clause ------------------------------------
    
    ; source (listof column) output-port boolean -> void
    ;
    ; Displays an SQL fragment for a FROM statement. Doesn't include the word "FROM".
    ;
    ; The parenthesise? argument indicates which aliasing strategy we're using
    ; (see the comments at the top of the file).
    (define/override (display-from from imported out [parenthesise? (parenthesise-join? from)])
      (cond [(join? from)         (display-from/join from imported out parenthesise?)]
            [(entity-alias? from) (display-from/entity from out parenthesise?)]
            [(query-alias? from)  (display-from/query from out parenthesise?)]
            [else          (raise-exn exn:fail:contract
                             (format "Expected source, received ~a" from))]))
    
    ; join (listof column) output-port -> void
    ;
    ; The parenthesise? argument indicates which aliasing strategy we're using
    ; (see the comments at the top of the file).
    (define/public (display-from/join the-join imported out parenthesise?)
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
    (define/public (display-from/entity alias out parenthesise?)
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
    (define/public (display-from/query alias out parenthesise?)
      (match alias
        [(struct query-alias (id query))
         (display "(" out)
         (display-query query out)
         (display ")" out)
         (unless parenthesise?
           (display " AS " out)
           (display (escape-name id) out))]))
    
    ; source -> boolean
    (define/public (parenthesise-join? from)
      (and (join? from)
           (or (join? (join-right from))
               (parenthesise-join? (join-left from))
               (parenthesise-join? (join-right from)))))))

(provide sqlite3-sql-query-mixin)
