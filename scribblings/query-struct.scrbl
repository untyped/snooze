#lang scribble/doc

@(require (file "base.ss"))

@(define snooze-eval (make-snooze-eval))
          
@title[#:tag "query-struct"]{Underlying query structures}

@defmodule[(planet untyped/snooze:2/sql/sql-struct)]{

The @scheme[sql:foo] procedures described above create and return structures of a number of types, listed for completeness below.

It is rare that an application programmer has to interact directly with these structures: most of the forms below not provided via @scheme[(planet untyped/snooze/snooze)].

@defstruct[query
  ([what             (listof column?)]
   [distinct         (U expression? #t #f)]
   [from             source?]
   [where            (U expression? #f)]
   [group            (listof expression?)]
   [order            (listof order?)]
   [having           (U expression? #f)]
   [limit            (U integer? #f)]
   [offset           (U integer? #f)]
   [local-columns    (listof column?)]
   [imported-columns (listof column?)]
   [extract-info     (U entity? type? (listof (U entity? type?)))])]{
A query or subquery: the result of an @scheme[sql:select] expression or equivalent. @scheme[local-columns], @scheme[imported-columns] and @scheme[extract-info] are used internally within Snooze.}

The following are used in @scheme[#:from] clauses and as shortcuts in @scheme[#:what] and @scheme[#:group] clauses:

@defstruct[source ()]{
A query data source.}

@defstruct[(source-alias source) ([name symbol?] [value (U entity? query?)])] {
An alias of an entity or subquery.}

@defstruct[(entity-alias source-alias) ()]{
An alias for an entity.}

@defstruct[(query-alias source-alias) ()]{
An alias for a subquery.}

@defstruct[(join source)
  ([op (U 'inner 'outer 'left 'right)]
   [left source?]
   [right source?]
   [on (U expression? #f)])]{
A join over two other sources. @scheme[on] is required if @scheme[op] is @scheme['inner], @scheme['left] or @scheme['right], and forbidden if @scheme[op] is @scheme['outer].}

The following represent expressions and columns used in @scheme[#:what], @scheme[#:where] and @scheme[#:having] clauses and the conditions in inner, left and right joins:

@defstruct[expression ([type type?])]{
An expression used in a @scheme[#:where] or @scheme[#:having] statement or a join condition.}

@defstruct[(column expression) ([name symbol?])]{
A named column in a @scheme[#:what] or @scheme[#:group] clause.}

@defstruct[(attribute-alias column)
  ([entity entity-alias?] [attribute attribute?])]{
A column containing the value of an ERA attribute.}

@defstruct[(expression-alias column) ([value expression?])]{
A column containing the result of an SQL expression.}

@defstruct[(function expression)
  ([op symbol?]
   [args (listof (U expression? special-argument))])]{
An SQL operator (e.g. @tt{+}) or function call (e.g. @tt{floor} or @tt{count}).

Most functions take other expressions as arguments. Some special functions such as @scheme{sql:in} and @tt{sql:count*} have arguments of special types.}

@defstruct[(aggregate function) ()]{
An aggregate function call.}

@defstruct[(literal expression) ([value any])]{
A literal value. The interpretation of @scheme[value] depends on the expression type.}

@defstruct[order ([expression expression?]
                 [direction (U 'asc 'desc)])]{
A term in an @scheme[#:order] statement.}

} @;{end defmodule}
