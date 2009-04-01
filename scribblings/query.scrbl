#lang scribble/doc

@(require "base.ss")
          
@title[#:tag "query"]{Queries}

@extmore["http://en.wikipedia.org/wiki/SQL"]{SQL on Wikipedia} 
Snooze uses a combinator-based query language that mirrors a large subset of standard SQL, including joins, nested queries, aggregate functions, limits and offsets.

The @scheme[sql] form is used to enter the query language, and @scheme[unquote] is used to escape back into Scheme. Because @scheme[sql] statements are executed in the DBMS, the semantics of the query language are closer to SQL than Scheme. 

Here is an example of a simple query:

@schemeblock[ 
  (code:comment "(listof person)")
  (code:comment "Find all people in the database:")
  (find-all (sql (select #:from person)))]

The @scheme[sql] block describes the query to perform. The @scheme[select] form creates a @tt{SELECT} statement that selects all @scheme[person] records in the database. The @scheme[find-all] procedure sends the query to the database, retrieves all the available query results, and turns them into a list.

Snooze automatically infers the type of query result. This example is selecting @scheme[person] records, so the results of the @scheme[find-all] is a @scheme[(listof person)].

The query language lets you customise the type of result in various ways. For example:

@schemeblock[
  (code:comment "(listof string)")
  (code:comment "Find the names of all people from the database.")
  (find-all (sql (select #:what person.name
                         #:from person)))

  (code:comment "(listof (list string integer))")
  (code:comment "Find the names and ages of all people from the database.")
  (find-all (sql (select #:what (person.name person.age)
                         #:from person)))]

In the query language, dotted operators in identifiers are treated like operators meaning @italic{"attribute of"}. For example, @scheme[person.name] means @italic{"the name attribute of the person entity"}. The part before the dot has to be an entity or entity alias (see below).

@section[#:tag "query-procedures"]{Query methods/procedures}

@scheme[snooze<%>] supplies three @italic{query methods} 
that retrieve data in different ways:

@itemize{
  @item{@schemeidfont{find-all} retrieves a list of all matching
    @italic{results} from the database.}
  @item{@schemeidfont{find-one} is like @schemeidfont{find-all}
    but returns the first result found. If no results are found,
    returns @scheme[#f] instead.}
  @item{@margin-note{@other-manual['(planet untyped/unlib:3/gen)]}
    @schemeidfont{g:find} is like @schemeidfont{find-all} but returns
    a result @italic{generator}.
    
    @italic{Generators}, defined in Unlib, are a lightweight iteration
    mechanism for traversing and modifying large datasets where menory
    consumption is a concern. Unlib provides useful combinators for 
    manipulating generated data, including memory-efficient equivalents
    of @scheme[fold], @scheme[map] and @scheme[for-each].}}

@include-section{query-syntax.scrbl}
