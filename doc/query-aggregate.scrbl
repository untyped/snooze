#lang scribble/doc

@require[(file "base.ss")]

@title[#:tag "aggregate"]{Aggregate functions}

@extmore["http://www.postgresql.org/docs/8.1/static/functions-aggregate.html"]{Aggregate functions in PostgreSQL}
Snooze has limited support for SQL aggregate functions: you can use
the @scheme{COUNT} function on entire entities, and the @scheme{COUNT},
@scheme{MAX}, @scheme{MIN} and @scheme{AVERAGE} functions on individual
attributes.

Aggregate functions can be used in the @schemeid[what], @schemeid[where]
and @schemeid[order] arguments to @scheme[q:select] and the @schemeid[on]
argument to @scheme[q:inner], @scheme[q:left] and @scheme[q:right].

Note that there are many subtleties to the use of aggregate functions in
SQL that Snooze does not concern itself with. For example, different
DBMSs support different types of argument for each aggregate function.
If your DBMS rejects a query for any reason, Snooze will raise an
@scheme[exn:fail:snooze] containing the error message.

@defproc[(q:count* [arg (U entity-alias select #f) #f])
         aggregate-function]

Creates an aggregate function that returns the number of selected rows 
from the specified entity or subquery. If @schemeid[arg] is omitted or 
@scheme[#f], the function returns the number of rows from the current
query.

@defproc[(q:count [arg attribute-alias])
         aggregate-function]

Creates an aggregate function that returns the number of non-null rows 
in the specified attribute.

@defproc[(q:max [arg attribute-alias])
         aggregate-function]

Creates an aggregate function that returns the maximum value in the 
specified attribute.

@defproc[(q:min [arg attribute-alias])
         aggregate-function]

Creates an aggregate function that returns the minimum value in the 
specified attribute.

@defproc[(q:average [arg attribute-alias])
         aggregate-function]

Creates an aggregate function that returns the average value in the 
specified attribute.
