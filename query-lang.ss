(module query-lang mzscheme
  
  (require (all-except (lib "contract.ss") any)
           (lib "etc.ss")
           (lib "kw.ss")
           (lib "list.ss" "srfi" "1")
           (lib "time.ss" "srfi" "19")
           (lib "cut.ss" "srfi" "26"))
  
  (require (planet "list.ss" ("untyped" "unlib.plt" 2))
           (planet "symbol.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "base.ss")
           (prefix era: (file "era.ss"))
           (file "query-core.ss")
           (file "type.ss"))
  
  ; ***** NOTE *****
  ; The terms "entity" and "attribute" are used here
  ; to refer to parts of the query representation. The
  ; "entity" and "attribute" from era.ss are referred
  ; to as "era:entity" and "era:attribute".
  ; ****************
  
  ; Select ---------------------------------------
  
  ;; no-what-clause : symbol
  ;;
  ;; Unique value for determining when the #:what keyword is
  ;; omitted from q:select. Local to this file.
  (define no-what-clause 
    (gensym 'no-what-clause))
  
  ;; q:select
  ;;     : [#:alias symbol]
  ;;       #:what (U field entity aggregate (list-of (U field entity aggregate select)))
  ;;       #:from (U entity table join select)
  ;;       [#:where (U expr #f)]
  ;;       [#:order (list-of order)]
  ;;       ;#:group (list-of (U field entity select))]
  ;;       [#:limit (U integer #f)]
  ;;       [#:offset (U integer #f)]
  ;;    -> select
  ;;
  ;; #:what can be:
  ;;   - (U field entity), in which case the select is set to single-item mode and returns a
  ;;     generator of field values or persistent-structs;
  ;;   - (list-of (U field entity aggregate select)), in which case the select is set to multi-item
  ;;     mode and returns a generator of lists of field values and persistent-structs;
  ;;   - omitted, in which case it defaults to the value of #:from in single item mode: #:from must
  ;;     be an entity in this case.
  ;;
  ;; #:from must be a "source", which is:
  ;;   - an entity;
  ;;   - a table;
  ;;   - a select;
  ;;   - an (inner|outer|left|right) join over other sources.
  ;;
  ;; If #:what is omitted, #:from must be a single entity.
  ;;
  ;; #:where must be (U expression #f), #:order a (list-of order), #:group a (list-of (U field aggregate))
  ;; and #:limit and #:offset (U integer #f).
  ;;
  ;; Setting #:where, #:limit or #:offset to #f is equivalent to saying "ignore this part of the
  ;; statement". Setting #:order or #:group to null is equivalent to the same.
  (define q:select
    (lambda/kw (#:key [alias (gensym 'select)] [what no-what-clause] from [where #f] [group null] [order null] [limit #f] [offset #f])
      ; Check #:alias:
      (unless (symbol? alias)
        (raise-select-exn #:alias "symbol" alias))
      ; Check #:what and #:from:
      (cond [(eq? what no-what-clause)
             ; If #:what was omitted, set it to #:from.
             ; #:from must be an entity.
             (unless (entity? from)
               (raise-exn exn:fail:contract
                 (format "#:from argument to select (#:what omitted): expected entity, received ~s" from)))
             (set! what from)]
            [(or (list? what) (field? what) (entity? what) (aggregate? what))
             (unless (source? from)
               (raise-select-exn #:from "(U entity table join select)" from))]
            [else (raise-select-exn #:what "(U field entity select aggregate (list-of (U field entity select aggregate)))" what)])
      ; Check #:where:
      (unless (or (not where) (expr? where))
        (raise-select-exn #:where "(U expr #f)" where))
      ; Check #:group:
      (unless (list? group)
        (raise-select-exn #:group "(list-of (U field entity select))" group))
      ; Check #:order:
      (unless (and (list? order) (andmap order? order))
        (raise-select-exn #:order "(list-of order)" order))
      ; Check #:limit:
      (unless (or (not limit) (integer? limit))
        (raise-select-exn #:limit "(U integer #f)" limit))
      ; Check #:offset:
      (unless (or (not offset) (integer? offset))
        (raise-select-exn #:offset "(U integer #f)" offset))
      ; Define the expanded, entity-free what, group, from-fields, from-tables and single-item:
      (begin0 (let-values ([(what-entities)           (what->entities what)]
                           [(what)                    (what->fields+aggregates what)]
                           [(group)                   (what->fields+aggregates group)]
                           [(from-fields from-tables) (source-fields+tables from)]
                           [(single-item?)            (and (not (pair? what)) (not (null? what)))])
                ; Check scopes of definitions:
                (check-what-fields+tables  what  from-fields from-tables)
                (check-expr-fields+tables  where from-fields from-tables "WHERE")
                (check-order-fields+tables order from-fields from-tables)
                ; Make the structure:
                (make-select alias
                             what
                             from
                             where
                             group
                             order
                             limit
                             offset
                             what-entities
                             from-fields
                             single-item?)))))
  
  ;; raise-select-exn : keyword string any -> void | exn:fail:contract
  (define (raise-select-exn kw expected received)
    (raise-exn exn:fail:contract
      (format "~a argument to select: expected ~a, received ~s" kw expected received)))
  
  ; Aggregates -----------------------------------
  
  ;; q:count : [symbol] (U field attribute) -> aggregate
  (define q:count
    (case-lambda
      [(arg)       (q:count (gensym 'count) arg)]
      [(alias arg) (make-aggregate alias 'count arg)]))
  
  ;; q:count* : [[symbol] (U select table #f)] -> aggregate
  (define q:count*
    (case-lambda
      [()          (q:count* (gensym 'count*) #f)]
      [(arg)       (q:count* (gensym 'count*) arg)]
      [(alias arg) (make-aggregate alias 'count* arg)]))
  
  ;; q:max : [symbol] (U field attribute) -> aggregate
  (define q:max
    (case-lambda
      [(arg)       (q:max (gensym 'max) arg)]
      [(alias arg) (make-aggregate alias 'max arg)]))
  
  ;; q:min : [symbol] (U field attribute) -> aggregate
  (define q:min
    (case-lambda
      [(arg)       (q:min (gensym 'min) arg)]
      [(alias arg) (make-aggregate alias 'min arg)]))
  
  ;; q:average : [symbol] (U field attribute) -> aggregate
  (define q:average
    (case-lambda
      [(arg)       (q:average (gensym 'average) arg)]
      [(alias arg) (make-aggregate alias 'average arg)]))
  
  ; Sources --------------------------------------
  
  ;; q:entity : [symbol] era:entity -> entity
  (define q:entity 
    (case-lambda
      [(era-entity)       (q:entity (gensym (era:entity-name era-entity)) era-entity)]
      [(alias era-entity) (make-entity alias (era:entity-name era-entity) era-entity)]))
  
  ;; q:table : [symbol] symbol -> table
  (define q:table
    (case-lambda
      [(name)       (q:table (gensym name) name)]
      [(alias name) (make-table alias name)]))
  
  ;; q:inner : source source expr -> join
  (define q:inner (cut create-join 'inner <> <> <>))
  
  ;; q:left : source source expr -> join
  (define q:left (cut create-join 'left <> <> <>))
  
  ;; q:right : source source expr -> join
  (define q:right (cut create-join 'right <> <> <>))
  
  ;; q:outer : source source -> join
  (define q:outer (cut create-join 'outer <> <> #f))
  
  ;; create-join
  ;;     : (U 'inner 'outer 'left 'right)
  ;;       (U table join select)
  ;;       (U table join select)
  ;;       (U expr #f)
  ;;    -> join
  (define (create-join op left right on)
    (let*-values ([(left-fields left-tables)
                   (source-fields+tables left)]
                  [(right-fields right-tables)
                   (source-fields+tables right)]
                  [(fields tables)
                   (values (append left-fields right-fields)
                           (append left-tables right-tables))])
      (when on
        (check-expr-fields+tables on fields tables "ON"))
      (make-join op left right on fields tables)))
  
  ; Fields and attributes ------------------------
  
  ;; q:attr : entity symbol -> attribute
  (define q:attr
    (case-lambda
      [(entity name) 
       (q:attr #f entity name)]
      [(alias entity name)
       (let ([era-attribute (era:get-attribute (entity-entity entity) name)])
         (unless era-attribute
           (raise-exn exn:fail:snooze
             (format "Attribute ~a is not defined in entity: ~a" name entity)))
         (make-field (if alias alias (symbol-append (named-alias entity) '- name))
                     entity
                     name
                     (era:attribute-type era-attribute)))]))
  
  ;; q:field : [symbol] table symbol type -> field
  (define q:field 
    (case-lambda
      [(table name type)
       (q:field (symbol-append (named-alias table) '- name) table name type)]
      [(alias table name type)
       (make-field alias table name type)]))
  
  ; Expressions ----------------------------------
  
  ;; q:and : expr ... -> expr
  (define (q:and . args)
    (make-expr 'and args))
  
  ;; q:or : expr ... -> expr
  (define (q:or . args)
    (make-expr 'or args))
  
  ;; q:not : expr -> expr
  (define (q:not arg)
    (make-expr 'not (list arg)))
  
  ;; q:= : (U expr string integer symbol boolean ref) (U expr string integer symbol boolean ref) -> expr
  (define (q:= arg1 arg2)
    (make-expr '= (list arg1 arg2)))
  
  ;; q:<> : (U expr string integer symbol boolean ref) (U expr string integer symbol boolean ref) -> expr
  (define (q:<> arg1 arg2)
    (make-expr '<> (list arg1 arg2)))
  
  ;; q:< : (U expr string integer symbol boolean ref) (U expr string integer symbol boolean ref) -> expr
  (define (q:< arg1 arg2)
    (make-expr '< (list arg1 arg2)))
  
  ;; q:> : (U expr string integer symbol boolean ref) (U expr string integer symbol boolean ref) -> expr
  (define (q:> arg1 arg2)
    (make-expr '> (list arg1 arg2)))
  
  ;; q:<= : (U expr string integer symbol boolean ref) (U expr string integer symbol boolean ref) -> expr
  (define (q:<= arg1 arg2)
    (make-expr '<= (list arg1 arg2)))
  
  ;; q:>= : (U expr string integer symbol boolean ref) (U expr string integer symbol boolean ref) -> expr
  (define (q:>= arg1 arg2)
    (make-expr '>= (list arg1 arg2)))
  
  ;; q:like : (U expr string integer symbol boolean ref) (U expr string integer symbol boolean ref) -> expr
  (define (q:like arg1 arg2)
    (make-expr 'like (list arg1 arg2)))
  
  ;; q:match : (U expr string integer symbol boolean ref) (U expr string integer symbol boolean ref) -> expr
  (define (q:match arg1 arg2)
    (make-expr 'match-ci (list arg1 arg2)))
  
  ;; q:match-ci : (U expr string integer symbol boolean ref) (U expr string integer symbol boolean ref) -> expr
  (define (q:match-ci arg1 arg2)
    (make-expr 'match-ci (list arg1 arg2)))
  
  ;; q:null? : (U expr string integer symbol boolean ref) -> expr
  (define (q:null? arg)
    (make-expr 'null? (list arg)))
  
  ;; q:in : (U expr string integer symbol boolean ref) select -> expr
  (define (q:in arg1 arg2)
    (make-expr 'in (list arg1 arg2)))
  
  ; Order ----------------------------------------
  
  ;; q:order : (U field aggregate) (U 'asc 'desc) -> order
  (define q:order make-order)
  
  ;; q:asc : (U field aggregate) -> order
  (define (q:asc arg)
    (make-order arg 'asc))
  
  ;; q:desc : (U field aggregate) -> order
  (define (q:desc arg)
    (make-order arg 'desc))
  
  ; Utility functions ----------------------------
  
  ;; what->fields+aggregates : (U field aggregate entity select (list-of (U field aggregate entity select)))
  ;;                        -> (list-of (U field aggregate))
  (define (what->fields+aggregates what)
    (if (or (pair? what) (null? what))
        (reverse (fold (lambda (item accum)
                         (cond [(field? item)     (cons item accum)]
                               [(aggregate? item) (cons item accum)]
                               [(entity? item)    (fold cons accum (entity->fields item))]
                               [(select? item)    (fold cons accum (select-what item))]
                               [else (raise-exn exn:fail:snooze
                                       (format "First argument to select: expected ~a, received ~s"
                                               "(list-of (U attribute field entity select aggregate))"
                                               item))]))
                       null
                       what))
        (what->fields+aggregates (list what))))
  
  ;; entity->fields : entity -> (list-of field)
  (define (entity->fields entity)
    (map (lambda (era:attr)
           (q:field entity
                    (era:attribute-name era:attr)
                    (era:attribute-type era:attr)))
         (era:entity-fields (entity-entity entity))))
  
  ;; what->entities
  ;;     : (U field table select (list-of (U field table select)))
  ;;    -> (list-of (U era:entity #f))
  ;;
  ;; Creates a list containing the entities to extract from the query results,
  ;; padded with a #f for each column that should not become part of an entity.
  (define (what->entities what)
    (if (or (pair? what) (null? what))
        (reverse (fold (lambda (item accum)
                         (cond [(entity? item) (cons (entity-entity item) accum)]
                               [(select? item) (fold cons accum (select-what-entities item))]
                               [else           (cons #f accum)]))
                       null
                       what))
        (what->entities (list what))))
  
  ;; source-fields+tables
  ;;     : (U table join select)
  ;;    -> (values (list-of (U field aggregate))
  ;;               (list-of table))
  ;;
  ;; Calculates the:
  ;;   - fields referenced by subqueries in a join statement
  ;;   - tables referenced in the join statement itself
  (define (source-fields+tables from)
    (cond [(table? from)  (values null (list from))]
          [(join? from)   (values (join-fields from)
                                  (join-tables from))]
          [(select? from) (values (select-what from) null)]
          [else           (raise-exn exn:fail:contract
                            (format "Expected (U table join select), received ~a" from))]))
  
  ; Public interface -----------------------------
  
  (provide [rename q:select select] ; Keyword lambda : does its own contract checking.
           ; From query-core.ss:
           named?
           select?
           field?
           aggregate?
           table?
           entity?
           join?
           expr?
           order?)
  
  (provide/contract 
   [rename q:attr      attr      (case-> (-> entity? symbol? field?)
                                         (-> symbol? entity? symbol? field?))]
   [rename q:field     field     (case-> (-> table? symbol? type? field?)
                                         (-> symbol? table? symbol? type? field?))]
   [rename q:count     count     (case-> (-> field? aggregate?)
                                         (-> symbol? field? aggregate?))]
   [rename q:count*    count*    (case-> (-> aggregate?)
                                         (-> (or/c table? select? false/c) aggregate?)
                                         (-> symbol? (or/c table? select? false/c) aggregate?))]
   [rename q:max       max       (case-> (-> field? aggregate?)
                                         (-> symbol? field? aggregate?))]
   [rename q:min       min       (case-> (-> field? aggregate?)
                                         (-> symbol? field? aggregate?))]
   [rename q:average   average   (case-> (-> field? aggregate?)
                                         (-> symbol? field? aggregate?))]
   [rename q:entity    entity    (case-> (-> era:entity? entity?)
                                         (-> symbol? era:entity? entity?))]
   [rename q:table     table     (case-> (-> symbol? table?)
                                         (-> symbol? symbol? table?))]
   [rename q:inner     inner     (-> source? source? expr/c join?)]
   [rename q:left      left      (-> source? source? expr/c join?)]
   [rename q:right     right     (-> source? source? expr/c join?)]
   [rename q:outer     outer     (-> source? source? join?)]
   [rename q:and       and       (->* () (listof expr/c) (expr?))]
   [rename q:or        or        (->* () (listof expr/c) (expr?))]
   [rename q:not       not       (-> expr/c expr?)]
   [rename q:=         =         (-> atom/c atom/c expr?)]
   [rename q:<>        <>        (-> atom/c atom/c expr?)]
   [rename q:<         <         (-> atom/c atom/c expr?)]
   [rename q:>         >         (-> atom/c atom/c expr?)]
   [rename q:<=        <=        (-> atom/c atom/c expr?)]
   [rename q:>=        >=        (-> atom/c atom/c expr?)]
   [rename q:like      like      (-> atom/c atom/c expr?)]
   [rename q:match     match     (-> atom/c atom/c expr?)]
   [rename q:match-ci  match-ci  (-> atom/c atom/c expr?)]
   [rename q:null?     null?     (-> atom/c expr?)]
   [rename q:in        in        (-> atom/c select? expr?)]
   [rename q:order     order     (-> (or/c field? aggregate?) (symbols 'asc 'desc) order?)]
   [rename q:asc       asc       (-> (or/c field? aggregate?) order?)]
   [rename q:desc      desc      (-> (or/c field? aggregate?) order?)])
  
  )
