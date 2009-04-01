#lang scribble/doc

@(require "base.ss")

@(define snooze-eval (make-snooze-eval))

@(declare-exporting (planet untyped/snooze))
        
@title[#:tag "alias"]{Aliases}

Aliases are Snooze's mechanism for letting you to refer to
individual instances of an entity or expression within a query.

You can introduce aliases for entities, attributes or expressions
using the following forms (see below for examples):

@defform[(define-alias id expr)]{
Introduces an alias for @scheme[expr] using the same scoping 
rules as @scheme[define].}

@defform[(let-alias ([id expr] ...) body-expr ...)]{
Like @scheme[define-alias] but uses the same scoping rules as
@scheme[let].}

@section{Entity aliases}

It is sometimes necessary to refer to more than one instance of
an entity within a query. Snooze lets you do this by defining 
@italic{aliases} for entities. For example:

@schemeblock[
  (define-alias father person)
  
  (code:comment "Luke's father:")
  (find-one
   (sql (select #:what  father
                #:from  (inner person
                               father
                               (= person.father-id father.id))
                #:where (= person.name "Luke"))))]

@section{Attribute and expression aliases}

It is sometimes useful to assign an alias to an attribute or  expression. 

For example, if you are @scheme[#:order]@schemeidfont{ing}
on an expression, SQL 97 requires you to add the @italic{same}
expression to your @scheme[#:what] clause. The only way to do this is
to alias the expression:

@schemeblock[
  (code:comment "All people and BMIs, ordered by BMI:")
  (let-alias ([bmi (/ person.weight person.height)])
    (find-all
     (sql (select #:what  (person bmi)
                  #:from  person
                  #:order ((asc bmi))))))]
