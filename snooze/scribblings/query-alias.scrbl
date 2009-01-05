#lang scribble/doc

@(require "base.ss")

@(define snooze-eval (make-snooze-eval))

@(declare-exporting (planet untyped/snooze/snooze))
          
@title[#:tag "alias"]{Aliases}

To create a query, you first need to @italic{alias} one or more entities 
and attributes. This is analogous to the @scheme{AS} operator from SQL: 
by aliasing a single item more than once, you can refer to it in more 
than one context without ambiguity. While SQL allows you to omit aliases
where you are only referring to an item once, Snooze requires you to be
explicit at all times.

@defproc*[([(sql:alias [id symbol?] [entity entity?])      entity-alias?]
           [(sql:alias [id symbol?] [query  query?])       query-alias?]
           [(sql:alias [id symbol?] [expr   expression?])  expression-alias?]
           [(sql:alias [entity entity-alias?] 
                       [attr   (U attribute? symbol?)])    attribute-alias?])]{
Creates an alias for the supplied entity, attribute, SQL query, or SQL expression. Aliases defined with this procedure can only be used with the procedural version of the query language.}

@defform[(let-alias ([id datum] ...) expr ...)]{
Syntax wrapper for @scheme[sql:alias] that expands into a @scheme[let] block that binds @scheme[id]@schemeidfont{s} to aliases for each @scheme[datum]. 

Each @scheme[datum] can be an @scheme[entity], @scheme[attribute], @scheme[query], @scheme[expression] as decribed above, or it can be the name of a persistent struct. In this last case, @scheme[let-alias] binds identifiers for aliases for the corresponding entity and all of its attributes.

@examples[
  #:eval snooze-eval
  (define-persistent-struct person
    ([name type:string] [age type:integer]))
  (let-alias ([P person])
    (pretty-print (list P P-id P-age)))]}

@defform[(define-alias id datum)]{
Version of @scheme[let-alias] that expands into a @scheme[define] statement.}
