#lang scribble/doc

@(require (file "base.ss"))
          
@title[#:tag "from"]{@scheme[#:from] clauses}

@(declare-exporting (planet untyped/snooze/snooze))

The @scheme[#:from] clause specifies the source of a query's data. A @scheme[source] can be one of the following:

@itemize{
  @item{an entity alias;}
  @item{a alias of another query;}
  @item{a join over two other sources;}}

@extmore["http://en.wikipedia.org/wiki/Join_(SQL)"]{Joins on Wikipedia}
Snooze supports four types of join:

@defproc[(sql:inner [left source]
                    [right source]
                    [on expr])
         source]{
Creates an @italic{inner join} on two sources. The @schemeid[on] argument specifies the join criteria.}

@defproc[(sql:left [left source]
                   [right source]
                   [on expr])
         source]{
Creates a @italic{left outer join} on two sources. The @schemeid[on] argument specifies the join criteria.}

@defproc[(sql:right [left source]
                    [right source]
                    [on expr])
         source]{
Creates a @italic{right outer join} on two sources. The @schemeid[on] argument specifies the join criteria.}
    
@defproc[(sql:outer [left source]
                    [right source])
         source]{
Creates a @italic{natural join} on two sources.}
