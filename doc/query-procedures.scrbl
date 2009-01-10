#lang scribble/doc

@require[(file "base.ss")]

@title[#:tag "query-procedures"]{Query procedures}

Snooze supplies three @italic{query procedures} that retrieve data in
different ways:

@defproc[(find-all [conn connection (current-connection)]
                   [query select])
         (listof result)]

@scheme[find-all] retrieves a list of all matching @italic{results}
from the database. @schemeid[conn] is an optional connection: if omitted, 
the default connection from @scheme[call-with-database] is used.

@intmore{select}
@schemeid[query] is a @schemeid[select] statement, as returned by 
@scheme[q:select]. It determines both the data retrieved and the type of 
each @schemeid[result].

@defproc[(find-one [conn connection (current-connection)]
                   [query select])
         (U result #f)]

Similar to @scheme[find-all], but only returns the first result found. If
no  results are found, returns @scheme[#f] instead.

@defproc[(g:find [conn connection (current-connection)]
                 [query select])
         (gen-> result)]

@intmore{generators}
Similar to @scheme[find-all], but returns a @italic{generator} of results.
This is the most general query mechanism offered by Snooze: generators
allow you to manipulate results one at a time in a functional manner,
without wasting lots of memory on intermediate lists.