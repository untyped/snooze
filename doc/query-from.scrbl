#lang scribble/doc

@require[(file "base.ss")]

@title[#:tag "from"]{FROM clauses}

The @schemeid[from] argument to @scheme[q:select] specifies the 
@schemeid[source] tables from which the query results will be calculated. 
This is analogous to the @scheme{FROM} clause in SQL. A @schemeid[source] 
can be one of the following:

@itemize{
  @item{an @schemeid[entity-alias], as returned by @scheme[q:entity];}
  @item{a @schemeid[join] over multiple sources;}
  @item{another @schemeid[select] statement (a subquery: no recursive 
    queries, please);}}

@extmore["http://en.wikipedia.org/wiki/Join_(SQL)"]{Joins on Wikipedia}
Snooze supports four types of join:

@defproc[(q:inner [left source]
                  [right source]
                  [on expr])
         source]
         
Creates an @italic{inner join} on two sources. The @schemeid[on] argument
specifies the join criteria.

@defproc[(q:left [left source]
                 [right source]
                 [on expr])
         source]

Creates a @italic{left outer join} on two sources. The @schemeid[on] 
argument specifies the join criteria.

@defproc[(q:right [left source]
                  [right source]
                  [on expr])
         source]

Creates a @italic{right outer join} on two sources. The @schemeid[on] 
argument specifies the join criteria.
    
@defproc[(q:outer [left source]
                  [right source])
         source]

Creates a @italic{full outer join} or @italic{natural join} on two 
sources.
    
If the @schemeid[from] clause to @scheme[q:select] is a single 
entity, the @schemeid[what] clause may be omitted. In this case, the 
@schemeid[results] of the query are @schemeid[persistent-structs] of the
appropriate type.
