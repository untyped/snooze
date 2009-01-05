(module query-core mzscheme
  
  (require (lib "contract.ss")
           (lib "struct.ss")
           (all-except (lib "list.ss" "srfi" "1") any)
           (lib "time.ss" "srfi" "19")
           (lib "cut.ss" "srfi" "26"))
  
  (require (planet "symbol.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "base.ss")
           (prefix era: (file "era.ss"))
           (file "type.ss"))

  (provide (all-defined))
  
  ; ***** NOTE *****
  ; The terms "entity" and "attribute" are used here
  ; to refer to parts of the query representation. The
  ; "entity" and "attribute" from era.ss are referred
  ; to as "era:entity" and "era:attribute".
  ; ****************
  
  ; Structures -----------------------------------
  
  ;; struct named : symbol
  (define-struct named (alias) #f)
  
  ;; struct select
  ;;     : symbol 
  ;;       (list-of (U field aggregate))
  ;;       (U entity table join select)
  ;;       (U where #f)
  ;;       (list-of order)
  ;;       (list-of field)
  ;;       (U integer #f)
  ;;       (U integer #f)
  ;;       (list-of (U entity #f))
  ;;       (list-of (U field aggregate))
  ;;       boolean
  (define-struct (select named)
    (what from where group order limit offset what-entities from-fields single-item?) #f)

  ;; struct field : symbol table symbol
  (define-struct (field named) (table name type)         #f)
  
  ;; aggregate : symbol (U 'count 'count* 'max 'min 'average) (U select table field entity attribute)
  (define-struct (aggregate named) (op arg)             #f)

  ;; struct table : symbol symbol
  (define-struct (table named) (name)                    #f)
  
  ;; struct entity : symbol era:entity
  (define-struct/properties (entity table)
    (entity)
    ([prop:custom-write
      (lambda (item port write?)
        (fprintf port "<q:entity ~a ~a>" (named-alias item) (table-name item)))])
    #f)

  ;; struct join : (U 'inner 'outer 'left 'right) source source (U expr #f) (list-of (U field aggregate)) (list-of table)
  (define-struct join (op left right on fields tables)   #f)
  
  ;; struct where : expr
  (define-struct where (expr)                            #f)
  
  ;; struct on : expr
  (define-struct on (expr)                               #f)
  
  ;; struct expr : symbol (list-of (U expr string integer symbol boolean field aggregate))
  (define-struct expr (op args)                          #f)
  
  ;; struct order : (U field aggregate) (U 'asc 'desc)
  (define-struct order (arg dir)                         #f)
  
  ; Predicates -----------------------------------
  
  ;; source? : any -> boolean
  (define (source? item)
    (or (table? item)
        (join? item)
        (select? item)))
  
  ; Accessors / mutators -------------------------
  
  ;; select-what-types : select -> (list-of type)
  (define (select-what-types select)
    (map (lambda (field)
           (if (field? field)
               (field-type field)
               (aggregate-type field)))
         (select-what select)))
  
  ;; aggregate-type : aggregate -> base-type
  (define (aggregate-type aggregate)
    (let ([op  (aggregate-op aggregate)]
          [arg (aggregate-arg aggregate)])
      (cond [(eq? op 'count)   type:integer]
            [(eq? op 'count*)  type:integer]
            [(eq? op 'max)     (type-base (field-type arg))]
            [(eq? op 'min)     (type-base (field-type arg))]
            [(eq? op 'average) type:real]
            [else              (raise-exn exn:fail:snooze
                                 (format "Unrecognised aggregate operator: ~a ~a" op aggregate))])))
          
  ; Checking (used in query-lang.ss) -------------
  
  ;; check-what-fields+tables : (U field aggregate) (list-of (U field aggregate)) (list-of table) -> void
  ;;
  ;; Checks that fields/aggregates in a WHAT clause are:
  ;;   - EITHER from tables in the FROM clause
  ;;   - OR repeats of fields/aggregates that appear in selects in the FROM clause
  (define (check-what-fields+tables what from-fields from-tables)
    (for-each (lambda (field)
                (if (field? field)
                    ; Field is a field:
                    (unless (or (member (field-table field) from-tables)
                                (member field from-fields))
                      (raise-exn exn:fail:snooze
                        (format "Field ~a does not come from a selected table" field)))
                    ; Field is an aggregate:
                    (let ([arg (aggregate-arg field)])
                      (if (field? arg)
                          ; Aggregate is collected from a field:
                          (unless (or (member (field-table arg) from-tables)
                                      (member field from-fields))
                            (raise-exn exn:fail:snooze
                              (format "WHAT: aggregate ~a does not come from a selected table" field)))
                          ; Aggregate is collected from table.*:
                          (unless (or (not arg)
                                      (member arg from-tables)
                                      (member field from-fields))
                            (raise-exn exn:fail:snooze
                              (format "WHAT: aggregate ~a does not come from a selected table" field)))))))
              what))
  
  ;; check-expr-fields+tables
  ;;     : (U expr field aggregate)
  ;;       (list-of (U field aggregate))
  ;;       (list-of table)
  ;;       (U "OR" "WHERE")
  ;;    -> void
  ;;     | exn:fail:snooze
  ;;
  ;; Check that any fields in the expression are:
  ;;   - EITHER from tables in the FROM statement
  ;;   - OR declared in subqueries
  ;;
  ;; Check that any aggregates in the expression are
  ;; references to columns defined in subqueries
  (define (check-expr-fields+tables expr fields tables expr-op)
    (cond [(field? expr)     (unless (or (member (field-table expr) tables)
                                         (member expr fields))
                               (raise-exn exn:fail:snooze
                                 (format "~a: field ~a is not declared in a table or subquery." expr-op expr)))]
          [(aggregate? expr) (unless (member expr fields)
                               (raise-exn exn:fail:snooze
                                 (format "~a: aggregate ~a is not declared in a subquery." expr-op expr)))]
          [(expr? expr)      (map (cut check-expr-fields+tables <> fields tables expr-op) (expr-args expr))]))
  
  ;; check-order-fields+tables
  ;;     : (list-of order)
  ;;       (list-of (U field aggregate))
  ;;       (list-of table)
  ;;    -> void
  ;;     | exn:fail:snooze
  ;;
  ;; Check that any fields in the ORDER statement are:
  ;;   - EITHER from tables in the FROM statement
  ;;   - OR declared in subqueries
  ;;
  ;; Check that any aggregates in the ORDER statement are:
  ;;   - EITHER references to columns defined in subqueries
  ;;   - OR defined on fields or tables that are present in
  ;;     (tables or subqueries in the) FROM clause
  (define (check-order-fields+tables order fields tables)
    (for-each (lambda (order)
                (let ([field (order-arg order)])
                  (cond [(field? field)     (unless (or (member (field-table field) tables)
                                                        (member field fields))
                                              (raise-exn exn:fail:snooze
                                                (format "ORDER: field ~a is not declared in a table or subquery." field)))]
                        [(aggregate? field) (unless (or (member field fields)
                                                        (let ([arg (aggregate-arg field)])
                                                          (if (field? arg)
                                                              (or (member (field-table arg) tables) (member arg fields))
                                                              (member arg tables))))
                                              (raise-exn exn:fail:snooze
                                                (format "ORDER: aggregate ~a is not declared in the surrounding query." field)))])))
              order))
  
  ; Contracts ------------------------------------
  
  ;; contract atom/c : contract for atomic parts of WHERE and ON expressions
  (define atom/c
    (or/c string? integer? symbol? boolean? time? field? aggregate?))
  
  ;; contract expr/c : contract for parts of WHERE and ON expressions
  (define expr/c
    (or/c expr? atom/c))

  )
  