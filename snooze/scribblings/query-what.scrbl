#lang scribble/doc

@(require "base.ss")

@title[#:tag "what"]{@scheme[#:what] clauses}

The @scheme[#:what] clause specifies the type of result to be retrieved from the query. This must be a subset of the information available in the @scheme[#:from] clause. The result type is determined as follows:

@itemize{
  @item{if @scheme[#:what] is an expression or attribute alias, each result is a Scheme value of the corresponding type;}
  @item{if @scheme[#:what] is an entity alias, each result is a persistent struct of the corresponding type (@scheme[#f] is used to represent @tt{NULL});}
  @item{if @scheme[#:what] is a query alias, each result is of the type determined by the query's @scheme[what] clause;}
  @item{if @scheme[#:what] is a list of expressions, entity aliases and query aliases, each result is a list of items of the appropriate types.}}

If the @scheme[#:what] clause is omitted, Snooze infers it from the contents of the @scheme[#:from] clause:

@itemize{
  @item{if @scheme[#:from] is a single entity, each result is a persistent struct of that type;}
  @item{if @scheme[#:from] is a query, each result is of the type determined by the query's @scheme[what] clause;}
  @item{if @scheme[#:from] is a join, each result is a flattened list of the results determined by the arguments of the join.}}

For example, the following code would return a list of results of type @scheme[(list person string integer)]:

@schemeblock[
  (define-alias P1 person)
  (define-alias P2 person)
  (find-all (sql:select #:what (list P1 P1-name (sql:max P2-age))
                        #:from (code:comment "...")))]
