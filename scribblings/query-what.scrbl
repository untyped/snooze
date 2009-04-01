#lang scribble/doc

@(require "base.ss")

@title[#:tag "what"]{The @scheme[#:what] clause}

The @scheme[#:what] clause specifies the result type of the query.
The data in the @scheme[#:what] clause must be derived from that 
selected in the @scheme[#:from] clause.

For example, the query:

@schemeblock[(sql (select #:from person))]

has a result type of @scheme[person], whereas the query:

@schemeblock[(sql (select #:what person.name #:from person))]

has a result type of @scheme[string]. The expression:

@schemeblock[(find-all (sql (select #:what person.name #:from person)))]

would therefore return a @scheme[(listof person)].

The @scheme[#:what] clause can be one of the following:

@itemize{
    @item{an entity or entity alias:
      @schemeblock[
        (code:comment "(listof person)")
        (find-all
         (sql (select #:what person
                      #:from (code:comment "..."))))]}

    @item{an attribute or attribute alias:
      @schemeblock[
        (code:comment "(listof string)")
        (find-all
         (sql (select #:what person.surname
                      #:from (code:comment "..."))))]}
    
    @item{@intmore["where"] an SQL expression (like those
      used in @scheme[#:where] clauses):
      @schemeblock[
        (code:comment "(listof string)")
        (find-all
         (sql (select #:what (string-append
                              person.forenames
                              " "
                              person.surname)
                      #:from (code:comment "..."))))]}
          
    @item{a list of the above:
      @schemeblock[
        (code:comment "(listof (list string string))")
        (find-all
         (sql (select #:what (person.forenames person.surname)
                      #:from (code:comment "..."))))]}}

If the @scheme[#:what] clause is omitted, Snooze infers the 
result type from the @scheme[#:from] clause:

@itemize{
  @item{if @scheme[#:from] is a single entity, the result type
    is the relevant persistent struct type:
    
    @schemeblock[
      (code:comment "(listof person)")
      (find-all
       (sql (select #:from person)))]}
      
  @item{if @scheme[#:from] is a subquery, the result type is 
    the same as that of the subquery:
    
    @schemeblock[
      (code:comment "(listof person)")
      (find-all
       (sql (select #:from (select #:from person))))]}
      
  @item{if @scheme[#:from] is a join, the result type is a 
    flattened list of the result types of the join arguments:
    
    @schemeblock[
     (code:comment "(listof (list employer employee))")
     (find-all
      (sql (select #:from (inner employer
                                 employee
                                 (code:comment "...")))))]}}
