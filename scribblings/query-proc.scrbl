#lang scribble/doc

@(require "base.ss")
        
@title{Procedural query layer}

@(declare-exporting (planet untyped/snooze))

The query language described above is a syntax wrapper for a
set of constructors for specially designed SQL AST nodes.

Code that uses the constructors is generally more verbose than
code that uses the syntax query language. However, in some cases
it is useful to have access to standard Scheme procedures such
as @scheme[apply] and @scheme[map] when building queries.

The elements of the procedural language are listed below
for completeness. See the documentation for the syntax wrappers
for more information.

@section{Select statements}

@defproc[(sql:select [#:from     from     (U source? query?)]
                     [#:what     what     (U expression? source-alias? (listof (U expression? source-alias?)) #f) #f]
                     [#:where    where    (U expression? #f) #f]
                     [#:order    order    (listof order?) null]
                     [#:limit    limit    (U integer? #f) #f]
                     [#:offset   offset   (U integer? #f) #f]
                     [#:group    group    (listof (U column? source-alias?)) null]
                     [#:distinct distinct (U expression? #t #f) #f]) query?]

@section{From clauses}

@defproc[(sql:inner [left source?] [right source?] [on expr?]) source?]
@defproc[(sql:left  [left source?] [right source?] [on expr?]) source?]
@defproc[(sql:right [left source?] [right source?] [on expr?]) source?]
@defproc[(sql:outer [left source?] [right source?]) source?]

@section{Expressions (where/on/having clauses)}

@defproc[(sql:or  [arg expr+quotable] ...) function?]
@defproc[(sql:and [arg expr+quotable] ...) function?]
@defproc[(sql:not [arg expr+quotable]) function?]
	
@defproc[(sql:=   [arg1 expr+quotable] [arg2 expr+quotable]) function?]
@defproc[(sql:<>  [arg1 expr+quotable] [arg2 expr+quotable]) function?]
@defproc[(sql:<   [arg1 expr+quotable] [arg2 expr+quotable]) function?]
@defproc[(sql:>   [arg1 expr+quotable] [arg2 expr+quotable]) function?]
@defproc[(sql:<=  [arg1 expr+quotable] [arg2 expr+quotable]) function?]
@defproc[(sql:>=  [arg1 expr+quotable] [arg2 expr+quotable]) function?]

@defproc[(sql:+       [arg1 expr+quotable] ...) function?]
@defproc[(sql:-       [arg1 expr+quotable] ...) function?]
@defproc[(sql:*       [arg1 expr+quotable] ...) function?]
@defproc[(sql:/       [arg1 expr+quotable] [arg2 expr+quotable]) function?]
@defproc[(sql:abs     [arg  expr+quotable]) function?]
@defproc[(sql:floor   [arg  expr+quotable]) function?]
@defproc[(sql:ceiling [arg  expr+quotable]) function?]
@defproc[(sql:round   [arg  expr+quotable]) function?]

@defproc[(sql:like               [str  expression?] [pattern expression?]) function?]
@defproc[(sql:regexp-match       [str  expression?] [pattern expression?]) function?]
@defproc[(sql:regexp-match-ci    [str  expression?] [pattern expression?]) function?]
@defproc[(sql:string-append      [arg1 expression?] ...) function?]

@defproc[(sql:string-replace     [haystack    expression?]
                                 [needle      expression?]
                                 [replacement expression?]) function?]
@defproc[(sql:regexp-replace     [haystack    expression?]
                                 [pattern     expression?]
                                 [replacement expression?]) function?]
@defproc[(sql:regexp-replace*    [haystack    expression?]
                                 [pattern     expression?]
                                 [replacement expression?]) function?]
@defproc[(sql:regexp-replace-ci  [haystack    expression?]
                                 [pattern     expression?]
                                 [replacement expression?]) function?]
@defproc[(sql:regexp-replace*-ci [haystack    expression?]
                                 [pattern     expression?]
                                 [replacement expression?]) function?]

@defproc[(sql:->string [datum expr+quotable] [format-string expr+quotable]) function?]
@defproc[(sql:->symbol [datum expr+quotable] [format-string expr+quotable]) function?]

@defproc[(sql:if [test expr+quotable?]
                 [then expr+quotable?]
                 [else (U expr+quotable? #f) #f]) function?]
@defform/subs[#:literals (else)
              (sql:cond clause ...)
              ([clause [test-expr value-expr]
               [else   value-expr]])]

@defproc[(sql:null?    [arg    expr+quotable?]) function?]
@defproc[(sql:coalesce [arg    expr+quotable?] ...) function?]
@defproc[(sql:in       [needle expr+quotable?]
                       [haystack (U (listof quotable?) query?)]) function?]
@defproc[(sql:count    [column column?]) aggregate?]
@defproc[(sql:max      [column column?]) aggregate?]
@defproc[(sql:min      [column column?]) aggregate?]
@defproc[(sql:average  [column column?]) aggregate?]
@defproc[(sql:count*   [arg (U source? #f) #f]) aggregate?]

@section{Order clauses}

@defproc[(sql:order [column column?] [dir (U 'asc 'desc)]) order?]
@defproc[(sql:asc   [column column?]) order?]
@defproc[(sql:desc  [column column?]) order?]

@section{Aliases}

@defproc*[([(sql:alias [id symbol?] [entity entity?])      entity-alias?]
           [(sql:alias [id symbol?] [query  query?])       query-alias?]
           [(sql:alias [id symbol?] [expr   expression?])  expression-alias?]
           [(sql:alias [entity entity-alias?]
                       [attr   (U attribute? symbol?)])    attribute-alias?])]

@section{Underlying data structures}

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
   [extract-info     (U entity? type? (listof (U entity? type?)))])]

@defstruct[source ()]
	
@defstruct[(source-alias source)
  ([name symbol?] [value (U entity? query?)])]

@defstruct[(entity-alias source-alias) ()]
@defstruct[(query-alias  source-alias) ()]
@defstruct[(join source)
  ([op    (U 'inner 'outer 'left 'right)]
   [left  source?]
   [right source?]
   [on    (U expression? #f)])]

@defstruct[expression ([type type?])]

@defstruct[(column expression) ([name symbol?])]

@defstruct[(attribute-alias column)
  ([entity entity-alias?] [attribute attribute?])]

@defstruct[(expression-alias column)
  ([value expression?])]

@defstruct[(function expression)
  ([op   symbol?]
   [args (listof (U expression? special-argument))])]

@defstruct[(aggregate function) ()]

@defstruct[(literal expression)
  ([value any])]

@defstruct[order
  ([expression expression?]
   [direction  (U 'asc 'desc)])]
