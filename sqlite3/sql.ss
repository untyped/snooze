#lang scheme/base

(require "../base.ss")

(require srfi/13
         srfi/19
         "../era/core.ss"
         "../era/snooze-struct.ss"
         "../generic/generic.ss"
         "../sql/sql-struct.ss")

(define sqlite3-sql-mixin
  (mixin (generic-database<%>) (sql-escape<%> parse<%> sql-create<%> sql-drop<%> sql-insert<%> sql-update<%> sql-delete<%>)

    (inspect #f)
    
    (inherit get-snooze)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; symbol -> string
    (define/public (escape-sql-name name)
      (string-append "[" (symbol->string name) "]"))
    
    ; type any -> string
    (define/public (escape-sql-value type value)
      (cond [(boolean-type? type)  (guard type value boolean?  "boolean")         (if value "1" "0")]
            [(not value)           "NULL"]
            [(guid-type? type)     (guard type value guid?     "(U guid #f)")     (cond [(not (eq? (guid-entity value) (guid-type-entity type)))
                                                                                         (error (format "wrong guid entity: expected ~a, received ~a."
                                                                                                        (entity-name (guid-entity value))
                                                                                                        (entity-name (guid-type-entity type))))]
                                                                                        [(guid-id value) => number->string]
                                                                                        [else "NULL"])]
            [(integer-type? type)  (guard type value integer?  "(U integer #f)")  (number->string value)]
            [(real-type? type)     (guard type value real?     "(U real #f)")     (number->string value)]
            [(string-type? type)   (guard type value string?   "(U string #f)")   (string-append "'" (regexp-replace* #rx"'" value "''") "'")]
            [(symbol-type? type)   (guard type value symbol?   "(U symbol #f)")   (string-append "'" (regexp-replace* #rx"'" (symbol->string value) "''") "'")]
            [(time-tai-type? type) (guard type value time-tai? "(U time-tai #f)") (escape-time time-tai value)]
            [(time-utc-type? type) (guard type value time-utc? "(U time-utc #f)") (escape-time time-utc value)]
            [else                  (raise-type-error #f "unrecognised type" type)]))
    
    ; srfi19-time-type (U time-tai time-utc) -> string
    (define/public (escape-time time-type time)
      (string-append (number->string (time-second time))
                     (string-pad (number->string (time-nanosecond time)) 9 #\0)))
        
    ; entity -> string
    (define/public (create-table-sql entity)
      (format "CREATE TABLE ~a (~a);"
              (escape-sql-name (entity-table-name entity)) 
              (string-join (list* (string-append (escape-sql-name 'id) " INTEGER PRIMARY KEY")
                                  (string-append (escape-sql-name 'revision) " INTEGER NOT NULL DEFAULT 0")
                                  (map (cut column-definition-sql <>)
                                       (cddr (entity-attributes entity))))
                           ", ")))
    
    ; attribute -> string
    (define/public (column-definition-sql attr)
      (let ([type (attribute-type attr)]
            [name (attribute-column-name attr)])
        (string-append
         (escape-sql-name name)
         (match type
           [(? guid-type?)      (format " INTEGER REFERENCES ~a.~a" 
                                        (escape-sql-name (entity-name (guid-type-entity type)))
                                        (escape-sql-name 'id))]
           [(? boolean-type?)   " INTEGER"]
           [(? integer-type?)   " INTEGER"]
           [(? real-type?)      " REAL"]
           [(? character-type?) (if (character-type-max-length type)
                                    (format " CHARACTER VARYING (~a)" (character-type-max-length type))
                                    " TEXT")]
           [(? temporal-type?)  " INTEGER"])
         (if (type-allows-null? type) "" " NOT NULL")
         " DEFAULT " (escape-sql-value type (attribute-default attr)))))
    
    ; (U entity symbol) -> string
    (define/public (drop-table-sql table)
      (let ([table-name (cond [(entity? table) (entity-table-name table)]
                              [(symbol? table) table]
                              [else            (raise-type-error 'drop-table-sql "(U entity symbol)" table)])])
        (format "DROP TABLE IF EXISTS ~a;" (escape-sql-name table-name))))
    
    ; snooze-struct -> string
    (define/public (insert-sql struct)
      (let* ([include-id? (and (struct-guid struct) #t)]
             [entity      (struct-entity struct)]
             [attrs       (entity-attributes entity)]
             [vals        (snooze-struct-ref* struct)]
             [table-name  (escape-sql-name (entity-table-name entity))]
             [col-names   (string-join (for/list ([attr (in-list (if include-id? attrs (cddr attrs)))])
                                         (escape-sql-name (attribute-column-name attr)))
                                       ", ")]
             [col-values  (string-join (for/list ([attr (in-list (if include-id? attrs (cddr attrs)))]
                                                  [val  (in-list (if include-id? vals  (cddr vals)))])
                                         (escape-sql-value (attribute-type attr) val))
                                       ", ")])
        (format "INSERT INTO ~a (~a) VALUES (~a);" table-name col-names col-values)))
    
    ; snooze-struct -> string
    (define/public (update-sql struct)
      (let* ([entity (struct-entity struct)]
             [exprs  (for/list ([attr (in-list (entity-attributes entity))]
                                [val  (in-list (snooze-struct-ref* struct))])
                       (string-append (escape-sql-name (attribute-column-name attr))
                                      " = "
                                      (escape-sql-value (attribute-type attr) val)))])
        (if (struct-saved? struct)
            (format "UPDATE ~a SET ~a WHERE ~a;"
                    (escape-sql-name (entity-table-name entity))
                    (string-join (cdr exprs) ", ")
                    (car exprs))
            (error "struct not in database" struct))))
    
    ; guid -> string
    (define/public (delete-sql guid)
      (let* ([entity (guid-entity guid)]
             [table  (entity-table-name entity)]
             [attr   (car (entity-attributes entity))]
             [id     (guid-id guid)])
        (format "DELETE FROM ~a WHERE ~a = ~a;"
                (escape-sql-name table)
                (escape-sql-name (attribute-column-name attr))
                (escape-sql-value (attribute-type attr) guid))))
    
    ; type string -> any
    (define/public (parse-value type value)
      (with-handlers ([exn? (lambda (exn) (raise-exn exn:fail:contract (exn-message exn)))])
        (cond [(guid-type? type)     (entity-make-vanilla-guid #:snooze (get-snooze) (guid-type-entity type) (inexact->exact value))]
              [(boolean-type? type)  (equal? value "1")]
              [(not value)           #f]
              [(integer-type? type)  (inexact->exact (string->number value))]
              [(real-type? type)     (string->number value)]
              [(string-type? type)   value]
              [(symbol-type? type)   (string->symbol value)]
              [(time-tai-type? type) (parse-time time-tai value)]
              [(time-utc-type? type) (parse-time time-utc value)]
              [else                  (raise-type-error 'parse-value "unrecognised type" type)])))
    
    ; srfi19-time-type string -> (U time-tai time-utc)
    (define/public (parse-time time-type value)
      (if (> (string-length value) 9)
          (let* ([sec  (string->number (string-drop-right value 9))]
                 [nano (string->number (string-take-right value 9))])
            (make-time time-type nano sec))
          (let* ([nano (string->number value)])
            (make-time time-type (if nano nano 0) 0))))

    ; snooze (listof type) -> ((U (listof database-value) #f) -> (U (listof scheme-value) #f))
    (define/public (make-parser types)
      (lambda (vals)
        (and vals (map (cut parse-value <> <>) types vals))))))

; Modifications to default query SQL -------------

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
    
    (inherit escape-sql-name
             escape-sql-value
             display-distinct
             display-what
             display-expression
             display-group
             display-order)
    
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
         (display (escape-sql-name (entity-table-name entity)) out)
         (display " AS " out)
         (display (escape-sql-name id) out)
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
           (display (escape-sql-name id) out))]))
    
    ; source -> boolean
    (define (parenthesise-join? from)
      (and (join? from)
           (or (join? (join-right from))
               (parenthesise-join? (join-left from))
               (parenthesise-join? (join-right from)))))))

; Helpers --------------------------------------

; (guard any (any -> boolean) string)
(define-syntax guard
  (syntax-rules ()
    [(guard type value predicate expected)
     (unless (predicate value)
       (raise-type-error (type-name type) expected value))]))

; Provide statements ---------------------------

(provide sqlite3-sql-mixin
         sqlite3-sql-query-mixin)
