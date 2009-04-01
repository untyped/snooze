#lang scribble/doc

@(require "base.ss")

@title[#:tag "distinct"]{The @scheme[#:distinct] clause}

@extmore["http://www.postgresql.org/docs/8.3/interactive/queries-select-lists.html#QUERIES-DISTINCT"]{DISTINCT
in PostgreSQL} The @scheme[#:distinct] clause allows you to filter query
the results for unique results. It is the equivalent of the @tt{DISTINCT}
or @tt{DISTINCT ON} clauses in SQL.

Tor consistent results across all DBMSs, the query must be sorted such
that rows are returned in a deterministic order, with similar rows
adjacent to one another in the result set. 

The @scheme[#:distinct] clause may be one of the following values:

@itemize{
  @item{@scheme[#:distinct #t] dictates that rows should be completely
    unique - the equivalent of writing @scheme{SELECT DISTINCT foo} in
    SQL:
    
    @schemeblock[
      (code:comment "(listof string)")
      (code:comment "The surnames of people in the database:")
      (code:comment "one result per distinct surname.")
      (find-all
       (sql (select #:what     person.surname
                    #:from     person
                    #:distinct #t)))]}

  @item{@scheme[#:distinct #f] completely disables uniqueness checking - 
    the equivalent of writing @tt{SElECT foo} or @tt{SELECT ALL foo} in SQL:
    
    @schemeblock[
      (code:comment "(listof string)")
      (code:comment "The surnames of people in the database:")
      (code:comment "one result per person.")
      (find-all
       (sql (select #:what     person.surname
                    #:from     person
                    #:distinct #f)))]}

  @item{@scheme[#:distinct (expr ...)] enables uniqueness checking
    with a custom uniqueness rule. Two results are considered equal
    if all @scheme[expr]@schemeidfont{s} return the same values:
    
    @schemeblock[
      (code:comment "(listof (list string string))")
      (code:comment "The surnames of people in the database,")
      (code:comment "one result per distinct surname,")
      (code:comment "together with the forenames of the first ")
      (code:comment "person found with each surname:")
      (find-all
       (sql (select #:what     (person.surname person.fornames)
                    #:from     person
                    #:distinct (person.surname))))]}}
