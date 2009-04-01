#lang scribble/doc

@(require "base.ss")

@title[#:tag "group"]{@scheme[#:group] and @scheme[#:having] clauses}

@extmore["http://www.postgresql.org/docs/8.1/static/queries-table-expressions.html#QUERIES-GROUP"]{GROUP
in PostgreSQL} SQL provides a mechanism for grouping and filtering the 
results of queries that involve aggregate functions. The semantics are 
confusing, unintuitive, and outside the scope of this document. Snooze 
provides access to these  features (for hardier programmers) via the 
@scheme[#:group] and @scheme[#:having] clauses:

The @scheme[#:group] clause is a list of @scheme[#:what] items by which 
results should be grouped. For example:

@schemeblock[
  (code:comment "(listof (list employer natural-number))")
  (code:comment "The number of employees working for each employer.")
  (find-all
   (sql (select #:what  (employer (count employee.id))
                #:from  (inner employer
                               employee
                               (code:comment "..."))
                #:group (employer))))]

The @scheme[#:having] clause is like the @scheme[#:where] clause but
eliminates groups from the query results.
