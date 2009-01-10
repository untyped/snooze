(module sql-select-internals-unit mzscheme
  
  (require (lib "unitsig.ss")
           (lib "time.ss" "srfi" "19"))
  
  (require (file "../base.ss")
           (file "../query-core.ss")
           (file "../type.ss")
           (file "sql-sig.ss"))
  
  (provide sql-select-internals@)
  
  (define sql-select-internals@
    (unit/sig sql-select-internals^
      (import sql-quote^)
      
      ; Public procedures ------------------------
      
      ;; display-select-sql 
      ;;     : select
      ;;       output-port
      ;;    -> void
      (define (display-select-sql select out)
        (let* ([what        (select-what select)]
               [from        (select-from select)]
               [where       (select-where select)]
               [group       (select-group select)]
               [order       (select-order select)]
               [limit       (select-limit select)]
               [offset      (select-offset select)]
               [from-fields (select-from-fields select)])
          (display "SELECT " out)
          (display-what-sql what from-fields out)
          (display " FROM " out)
          (display-from-sql from from-fields out)
          (when where
            (display " WHERE " out)
            (display-expr-sql where from-fields out))
          (unless (null? group)
            (display " GROUP BY " out)
            (display-group-sql group from-fields out))
          (unless (null? order)
            (display " ORDER BY " out)
            (display-order-sql order from-fields out))
          (when limit
            (display " LIMIT " out)
            (display limit out))
          (when offset
            (display " OFFSET " out)
            (display offset out))))
      
      ;; display-what-sql 
      ;;     : (cons (U field aggregate) (list-of (U field aggregate)))
      ;;       (list-of (U field aggregate))
      ;;       output-port
      ;;    -> void
      ;;
      ;; Returns an SQL fragment for a "WHAT" statement (the bit between "SELECT" and "FROM").
      ;; Doesn't include the words "SELECT" or "FROM".
      ;;
      ;; The "declared" argument is a list of fields that have been declared in subqueries.
      (define (display-what-sql what declared out)
        (let loop ([what what] [first? #t])
          (unless (null? what)
            (unless first? (display ", " out))
            (display-what-sql/field (car what) declared out)
            (loop (cdr what) #f))))
      
      ;; display-from-sql : (U table join select) (list-of (U field aggregate)) output-port -> void
      ;;
      ;; Returns an SQL fragment for a FROM statement. Doesn't include the word "FROM".
      (define (display-from-sql from declared out)
        (cond [(table? from)  (display-from-sql/table from declared out)]
              [(join? from)   (display-from-sql/join from declared out)]
              [(select? from) (display-from-sql/select from declared out)]
              [else           (raise-exn exn:fail:contract
                                (format "Expected (U table join select), received ~a" from))]))
      
      ;; display-expr-sql : (U expr field) (list-of (U field aggregate)) output-port -> void
      ;;
      ;; Returns an SQL fragment for an ON or WHERE expression. Doesn't include the words "ON" or "WHERE".
      (define (display-expr-sql expr declared out)
        (cond [(string? expr)  (display (quote-data type:text expr) out)]
              [(integer? expr) (display (quote-data type:integer expr) out)]
              [(symbol? expr)  (display (quote-data type:symbol expr) out)]
              [(boolean? expr) (display (quote-data type:boolean expr) out)]
              [(time? expr)    (display (quote-data type:time-tai expr) out)]
              [(field? expr)   (display-expr-sql/field expr declared out)]
              [(expr? expr)    (display-expr-sql/expr expr declared out)]
              [(select? expr)  (display-expr-sql/select expr declared out)]
              [else            (raise-exn exn:fail:contract
                                 (format "Expected (U expr field select), received ~a" expr))]))
      
      ;; display-group-sql : (list-of (U field aggregate)) (list-of (U field aggregate)) output-port -> void
      ;;
      ;; Returns an SQL fragment for an GROUP BY statement. Doesn't include the words "GROUP BY".
      (define (display-group-sql group declared out)
        (let loop ([group group] [first? #t])
          (unless (null? group)
            (let ([head (car group)]
                  [tail (cdr group)]) 
              (unless first?
                (display ", " out))
              (display-field-sql head declared out)
              (loop tail #f)))))
      
      ;; display-order-sql : (list-of order) (list-of (U field aggregate)) output-port -> void
      ;;
      ;; Returns an SQL fragment for an ORDER BY statement. Doesn't include the word "ORDER BY".
      (define (display-order-sql order declared out)
        (let loop ([order order] [first? #t])
          (unless (null? order)
            (let ([head (car order)]
                  [tail (cdr order)]) 
              (unless first?
                (display ", " out))
              (display-order-sql/order head declared out)
              (loop tail #f)))))
      
      ; Private procedures -----------------------
      
      ;; display-what-sql/field : (U field aggregate) (list-of (U field aggregate)) output-port -> void
      (define (display-what-sql/field field declared out)
        (if (member field declared)
            ; The field is from a subquery: print its alias:
            (display (quote-id (named-alias field)) out)
            ; The field is declared locally: print table alias, namd and field alias:
            (begin (display-field-sql field declared out)
                   (display " AS " out)
                   (display (quote-id (named-alias field)) out))))
      
      ;; display-from-sql/table : table (list-of (U field aggregate)) output-port -> void
      (define (display-from-sql/table table declared out)
        (display (quote-id (table-name table)) out)
        (display " AS " out)
        (display (quote-id (named-alias table)) out))
      
      ;; display-from-sql/join : join (list-of (U field aggregate)) output-port -> void
      (define (display-from-sql/join join declared out)
        (let ([op    (join-op join)]
              [left  (join-left join)]
              [right (join-right join)]
              [on    (join-on join)])
          (display "(" out)
          (display-from-sql left declared out)
          (cond [(eq? op 'inner) (display " INNER JOIN " out)]
                [(eq? op 'left)  (display " LEFT JOIN "  out)]
                [(eq? op 'right) (display " RIGHT JOIN " out)]
                [(eq? op 'outer) (display " OUTER JOIN " out)]
                [else              (raise-exn exn:fail:contract
                                     (format "Join operator: expected (U 'inner 'outer 'left 'right), received ~a" op))])
          (display-from-sql right declared out)
          (unless (eq? op 'outer)
            (display " ON " out)
            (display-expr-sql on declared out))
          (display ")" out)))
      
      ;; display-from-sql/select : select (list-of (U field aggregate)) output-port -> void
      (define (display-from-sql/select select declared out)
        (display "(" out)
        (display-select-sql select out)
        (display ") AS " out)
        (display (quote-id (named-alias select)) out))
      
      ;; display-expr-sql/field : (U field aggregate) (list-of (U field aggregate)) output-port -> void
      (define (display-expr-sql/field field declared out)
        (cond [(member field declared)
               (display (quote-id (named-alias field)) out)]
              [(field? field)
               (display (quote-id (named-alias (field-table field))) out)
               (display "." out)
               (display (quote-id (field-name field)) out)]
              [else (raise-exn exn:fail:snooze
                      (format "Locally declared aggregate functions cannot be referenced in expressions: ~a" field))]))
      
      ;; display-expr-sql/expr : expr (list-of (U field aggregate)) output-port -> void
      (define (display-expr-sql/expr expr declared out)
        (let ([op (expr-op expr)]
              [args (expr-args expr)])
          (cond [(eq? op 'and)      (display-expr-sql/infix  args " AND " declared out)]
                [(eq? op 'or)       (display-expr-sql/infix  args " OR " declared out)]
                [(eq? op 'not)      (display-expr-sql/outfix (car args) "NOT " #f declared out)]
                [(eq? op '=)        (display-expr-sql/infix  args " = " declared out)]
                [(eq? op '<>)       (display-expr-sql/infix  args " <> " declared out)]
                [(eq? op '<)        (display-expr-sql/infix  args " < " declared out)]
                [(eq? op '>)        (display-expr-sql/infix  args " > " declared out)]
                [(eq? op '<=)       (display-expr-sql/infix  args " <= " declared out)]
                [(eq? op '>=)       (display-expr-sql/infix  args " >= " declared out)]
                [(eq? op 'like)     (display-expr-sql/infix  args " LIKE " declared out)]
                [(eq? op 'match)    (display-expr-sql/infix  args " ~ " declared out)]
                [(eq? op 'match-ci) (display-expr-sql/infix  args " ~* " declared out)]
                [(eq? op 'null?)    (display-expr-sql/outfix (car args) #f " IS NULL" declared out)]
                [(eq? op 'in)       (display-expr-sql/in     (car args) (cadr args) declared out)]
                [else               (raise-exn exn:fail:snooze
                                      (format "Expression: unrecognised operator: ~a" op))])))
      
      ;; display-expr-sql/select : select (list-of (U field aggregate)) output-port -> void
      (define (display-expr-sql/select select declared out)
        (display "(" out)
        (display-select-sql select out)
        (display ")" out))
      
      ;; display-expr-sql/infix : (list-of expr) string (list-of (U field aggregate)) output-port -> void
      ;; Adds an infix operator to a list of arguments.
      ;; Used for most binary and n-ary expression ops.
      (define (display-expr-sql/infix args delim declared out)
        (display "(" out)
        (let loop ([args args] [first? #t])
          (unless (null? args)
            (unless first? (display delim out))
            (display-expr-sql (car args) declared out)
            (loop (cdr args) #f)))
        (display ")" out))
      
      ;; display-expr-sql/outfix : expr string string (list-of (U field aggregate)) output-port -> void
      ;; Adds prefix/suffix-position operators to a single argument.
      ;; Used for not and null? expressions
      (define (display-expr-sql/outfix arg prefix suffix declared out)
        (display "(" out)
        (when prefix (display prefix out))
        (display-expr-sql arg declared out)
        (when suffix (display suffix out))
        (display ")" out))

      ;; display-expr-sql/in : (U field aggregate) select (list-of (U field aggregate)) output-port -> void
      (define (display-expr-sql/in field source declared out)
        (display "(" out)
        (display-expr-sql field declared out)
        (display " IN (" out)
        (if (select? source)
            (display-select-sql source out)
            (let loop ([items source])
              (unless (null? items)
                (display-expr-sql (car items) declared out)
                (unless (null? (cdr items))
                  (display ", " out))
                (loop (cdr items)))))
        (display "))" out))
      
      ;; display-order-sql/order : order declared output-port -> void
      (define (display-order-sql/order order declared out)
        (let* ([arg   (order-arg order)]
               [dir   (order-dir order)]
               [alias (named-alias arg)])
          (display-field-sql arg declared out)
          (if (eq? dir 'asc)
              (display " ASC" out)
              (display " DESC" out))))
      
      ;; display-field-sql : (U field aggregate) (list-of (U field aggregate)) output-port -> void
      (define (display-field-sql field declared out)
        (if (member field declared)
            (display (quote-id (named-alias field)) out)
            (if (aggregate? field)
                (let ([op (aggregate-op field)]
                      [arg  (aggregate-arg field)])
                  (cond [(eq? op 'count)   (display "COUNT(" out)]
                        [(eq? op 'count*)  (display "COUNT(" out)]
                        [(eq? op 'max)     (display "MAX(" out)]
                        [(eq? op 'min)     (display "MIN(" out)]
                        [(eq? op 'average) (display "AVERAGE(" out)]
                        [else                (raise-exn exn:fail:contract
                                               (format "Expected (U 'count 'count* 'min 'max 'average), received ~a" op))])
                  (cond [(field? arg)
                         ; Argument is a field:
                         (display-field-sql arg declared out)
                         (display ")" out)]
                        [(named? arg)
                         ; Argument is a table or a select:
                         (display (quote-id (named-alias arg)) out)
                         (display ".*)" out)]
                        [(not arg)
                         ; Argument is #f:
                         (display "*)" out)]
                        [else
                         (raise-exn exn:fail:snooze
                           (format "Aggregate: expected (aggregate (U field named #f)), received ~a" field))]))
                (begin (display (quote-id (named-alias (field-table field))) out)
                       (display "." out)
                       (display (quote-id (field-name field)) out)))))
          
      ))
      
  )
    