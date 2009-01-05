#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "group"]{@scheme[#:group] and @scheme[#:having] clauses}

@extmore["http://www.postgresql.org/docs/8.1/static/queries-table-expressions.html#QUERIES-GROUP"]{GROUP in PostgreSQL}
SQL provides a mechanism for grouping and filtering the results of queries that involve aggregate functions. The semantics are confusing, unintuitive, and outside the scope of this manual. Snooze provides access to these features (for hardier programmers) via the @scheme[#:group] and @scheme[#:having] clauses:

The @scheme[#:group] clause is a list of the columns by which results should be grouped. Entity and query aliases may also be used: if so, they are expanded into their constituent columns.

The @scheme[#:having] clause is like the @scheme[#:where] clause but eliminates groups from the query results.
