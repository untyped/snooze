#lang scribble/doc

@(require "base.ss")

@title[#:tag "limit"]{@scheme[#:group] and @scheme[#:having] clauses}

@extmore["http://www.postgresql.org/docs/8.3/interactive/queries-limit.html"]{LIMIT
in PostgreSQL} The @scheme[#:limit] and @scheme[#:offset] clauses allow you 
to choose how many results you wish to receive and where in the result set you wish 
to retrieve them from. Each can be a natural number or @scheme[#f], where @scheme[#f]
is equivalent to omitting the clause.
