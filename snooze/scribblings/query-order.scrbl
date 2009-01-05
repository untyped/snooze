#lang scribble/doc

@(require "base.ss")

@title[#:tag "order"]{@scheme[#:order] clauses}

@(declare-exporting (planet untyped/snooze))

The @scheme[#:order] clause specifies the order in which results should be returned as a list of @scheme[order] terms constructed with
the procedures below.

Terms are listed in descending order of precedence. The SQL-97 standard dictates that aliased expression terms @italic{must} appear in the @scheme[#:what] clause:

@defproc[(sql:order [column column?]
                    [dir    (U 'asc 'desc)])
         order?]{
Sorts by @scheme[column] in ascending or descending order.}

@defproc[(sql:asc [column column?]) order?]{
Short-hand for @schemeid[(sql:order item 'asc)].}

@defproc[(sql:desc [column column?]) order?]{
Short-hand for @schemeid[(sql:order item 'desc)].}
