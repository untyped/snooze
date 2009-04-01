#lang scribble/doc

@(require "base.ss")

@title[#:tag "order"]{The @scheme[#:order] clause}

@(declare-exporting (planet untyped/snooze))

The @scheme[#:order] clause specifies the order in which query results
should be returned.

The clause is a list of @italic{order terms}, each of which can be
one of the following:

@defform[(order expr direction)]{
Sort by @scheme[expr], where @scheme[expr] is an attribute or expression.
@scheme[direction] can be the literal @scheme['asc] (ascending order):

@schemeblock[
  (sql (select #:from  person
               #:order ((order person.age 'asc))))]

the literal @scheme['desc] (descending order):

@schemeblock[
  (sql (select #:from  person
               #:order ((order person.age 'desc))))]

or an unquoted Scheme expression evaluating to @scheme['asc] or @scheme['desc]:

@schemeblock[
  (sql (select #:from  person
               #:order ((order person.age 
                               ,(string->symbol "asc")))))]}

@defform[(asc expr)]{
Shorthand for @scheme[(order expr 'asc)].}

@defform[(desc expr)]{
Shorthand for @scheme[(order expr 'desc)].}

When multiple terms are specified, they are treated in descending order of
precedence. For example, the expression:

@schemeblock[
  (find-all
   (sql (select #:from  person
                #:order ((desc person.age)
                         (asc person.surname)))))]
                        
would return a @scheme[(listof person)] sorted first by descending age,
then by alphabetical surname.

Note that the SQL-97 standard dictates that expressions and attributes in
the @scheme[#:order] clause @italic{must} appear in the @scheme[#:what] clause.
Expressions must be aliased so the DBMS can match the two clauses up.

For example:

@schemeblock[
  (let-alias ([x (sql (+ person.age person.shoe-size))])
    (sql (select #:what  x
                 #:from  person
                 #:order ((asc x)))))]

