#lang scribble/doc

@require[(file "base.ss")]

@title[#:tag "group"]{GROUP clauses}

@extmore["http://www.postgresql.org/docs/8.1/static/queries-table-expressions.html#QUERIES-GROUP"]{GROUP in PostgreSQL}
If you create a query where some (but not all) of the @schemeid[selectables]
in the @schemeid[what] clause are aggregate functions, you will need to
provide a @schemeid[group] argument to @scheme[q:select]. This argument
is analogous to the @scheme{GROUP BY} clause in SQL: it groups together
rows that share common values so that the aggregates can be accurately
calculated.

The @schemeid[group] argument is specified as a list of 
@schemeid[entity-aliases], @schemeid[attribute-aliases] and 
@schemeid[select] subqueries.

For example:

@schemeblock[
  (define-persistent-struct person
    ([name type:text]
     [age type:integer]
     [gender type:symbol]))
     
  (define (find-max-ages)
    (find-all 
     (let*-alias ([P       entity:person]
                  [max-age (q:max (q:attr P 'age))]
                  [gender  (q:attr P 'gender)])
       (q:select #:what  (list gender max-age)
                 #:from  P
                 #:group (list gender)))))]

the @schemeid[group] argument in this query specifies that the maximum
age should be selected for each gender of person, rather than for each
unique person.
