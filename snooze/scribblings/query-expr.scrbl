#lang scribble/doc

@(require "base.ss")

@title[#:tag "expr"]{Expressions}

@(declare-exporting (planet untyped/snooze))

Various clauses, such as @scheme[#:where] and @scheme[#:having] and the conditions of the various joins, are based on @scheme[expression]@schemeidfont{s} involving functions and attribute aliases. 

The expression language, which is a subset of the query language, is described below. These forms are essentially wrappers for SQL expressions, so SQL semantics prevail over Scheme semantics.

@section{Expression types}

Each expression has a type, which is one of the following:

@itemize{@item{@italic{boolean}}
         @item{@italic{numeric}, with @italic{integer} and @italic{real} subtypes;}
         @item{@italic{character}, with @italic{string} and @italic{symbol} subtypes;}
         @item{@italic{temporal}, with @italic{time-utc} and @italic{time-tai} subtypes.}}

The procedures below all operate on arguments that are expressions. The type of the resulting expression is determined by the types of its arguments. If literal Scheme values are supplied as arguments, they are automatically quoted to @scheme[literal] expressions of the relevant type.

@section{Boolean operators}

These functions operate on boolean arguments and produce boolean results:

@defproc[(sql:or  [arg expr+quotable] ...) function?]{
The Boolean @tt{OR} of the arguments.}

@defproc[(sql:and [arg expr+quotable] ...) function?]{
The Boolean @tt{AND} of the arguments.}

@defproc[(sql:not [arg expr+quotable]) function?]{
The negation of the argument.}

@section{Simple comparison functions}

These functions operate on numeric, boolean, string, symbolic or temporal arguments, as long as consistent types are used. For example, @scheme[(sql:= "str" 'str)] is a valid comparison whereas @scheme[(sql:= "str" 123)] is not. The result type is always boolean.

@defproc[(sql:=   [arg1 expr+quotable] [arg2 expr+quotable]) function?]{
The equivalent of Scheme's @scheme[equal?].}

@defproc[(sql:<>  [arg1 expr+quotable] [arg2 expr+quotable]) function?]{
The negation of @scheme[sql:=].}

@defproc[(sql:<   [arg1 expr+quotable] [arg2 expr+quotable]) function?]
@defproc[(sql:>   [arg1 expr+quotable] [arg2 expr+quotable]) function?]
@defproc[(sql:<=  [arg1 expr+quotable] [arg2 expr+quotable]) function?]
@defproc[(sql:>=  [arg1 expr+quotable] [arg2 expr+quotable]) function?]

@section{Mathematical functions}

The following operate on arguments of numeric or temporal types and return a result of the relevant type:

@defproc[(sql:+       [arg1 expr+quotable] ...) function?]
@defproc[(sql:-       [arg1 expr+quotable] ...) function?]

The following operate on arguments of numeric types only:

@defproc[(sql:*       [arg1 expr+quotable] ...) function?]
@defproc[(sql:/       [arg1 expr+quotable] [arg2 expr+quotable]) function?]
@defproc[(sql:abs     [arg  expr+quotable]) function?]
@defproc[(sql:floor   [arg  expr+quotable]) function?]
@defproc[(sql:ceiling [arg  expr+quotable]) function?]
@defproc[(sql:round   [arg  expr+quotable]) function?]

@section{String and pattern matching functions}

@extmore["http://www.postgresql.org/docs/8.3/interactive/functions-matching.html"]{Pattern matching in PostgreSQL}
These functions operate on character arguments and return character results:

@defproc[(sql:like [str expression?] [pattern expression?]) function?]{
@extmore["http://www.postgresql.org/docs/8.3/static/functions-matching.html#FUNCTIONS-LIKE"]{LIKE in PostgreSQL}
The SQL @tt{LIKE} function. Operates on character arguments and returns a boolean result. The second argument is the pattern to match against.}

@defproc[(sql:regexp-match [str expression?] [pattern expression?]) function?]{
@extmore["http://www.postgresql.org/docs/8.3/static/functions-matching.html#FUNCTIONS-POSIX-REGEXP"]{POSIX Regular Expressions in PostgreSQL}
POSIX regular expression pattern matching. Operators on character arguments and returns a boolean result. The second argument is the regular expression to match against.}

@defproc[(sql:regexp-match-ci [str expression?] [pattern expression?]) function?]{
Case insensitive version of @scheme[sql:regexp-match].}

@defproc[(sql:string-append [arg1 expression?] ...) function?]{
Concatenates the arguments. Operates on character arguments and returns a result of the same type.}

@defproc[(sql:string-replace [haystack    expression?]
                             [needle      expression?]
                             [replacement expression?]) function?]{
Searches for @scheme[needle] in @scheme[haystack] and replaces all occurrences with @scheme[replacement]. Operates on character arguments and returns a result of the same type.}

@defproc[(sql:regexp-replace [haystack    expression?]
                             [pattern     expression?]
                             [replacement expression?]) function?]{
Searches for the POSIX regular expression @scheme[pattern] in @scheme[haystack] and replaces the first occurrence with @scheme[replacement]. Operates on character arguments and returns a result of the same type.}

@defproc[(sql:regexp-replace* [haystack    expression?]
                              [pattern     expression?]
                              [replacement expression?]) function?]{
Like @scheme[sql:regexp-replace] but replaces all occurrences of @scheme[pattern] rather than just the first.}

@defproc[(sql:regexp-replace-ci [haystack    expression?]
                                [pattern     expression?]
                                [replacement expression?]) function?]{
Case insensitive version of @scheme[sql:regexp-replace].}

@defproc[(sql:regexp-replace*-ci [haystack    expression?]
                                 [pattern     expression?]
                                 [replacement expression?]) function?]{
Case insensitive version of @scheme[sql:regexp-replace*].}

@defproc[(sql:->string [datum expr+quotable] [format-string expr+quotable]) function?]{
@extmore["http://www.postgresql.org/docs/8.3/static/functions-formatting.html"]{String formatting functions in PostgreSQL}
Creates a @tt{TO_CHAR} function that converts any data type to a string. @scheme[format-string] must be a string-valued expression that specifies the format to use in the conversion (see the PostgreSQL documentation for examples).}

@defproc[(sql:->symbol [datum expr+quotable] [format-string expr+quotable]) function?]{
Like @scheme[sql:->string] but returns a function of symbol type.}

@section{Conditional functions}

@defproc[(sql:if [test expr+quotable?] [then expr+quotable?] [else (U expr+quotable? #f) #f]) function?]{
Creates a function that performs an if-then-else test on its arguments. @scheme[test] must be boolean valued. If @scheme[test] evaluates to @scheme[#t], the value of @scheme[then] is calculated and returned. If @scheme[test] evaluates to @scheme[#f], the value of @scheme[else] is calucated and returned instead. If @scheme[test] evaluates to @scheme[#f] and @scheme[else] is omitted, @tt{NULL} is returned.

@scheme[then] and @scheme[else] must have the same type: the return value is a function of the same type.}

@defform/subs[#:literals (else)
              (sql:cond clause ...)
              ([clause [test-expr value-expr]
                       [else      value-expr]])]{
@scheme[cond]-like syntax that expands into a chain of calls to @scheme[sql:if]. All @scheme[value-expr]@schemeidfont{s} must have the same types.}

@section{Functions related to @tt{NULL}}

@defproc[(sql:null?    [arg  expr+quotable?]) function?]{
@extmore["http://www.postgresql.org/docs/8.3/static/functions-comparison.html"]{IS NULL in PostgreSQL}
Creates a function that determines whether the argument is @tt{NULL}. Direct comparison with @tt{NULL} using @scheme[sql:=] does not work because in SQL semantics @tt{NULL} is not equal to itself.}

@defproc[(sql:coalesce [arg expr+quotable?] ...) function?]{
Creates a function that returns the value of the leftmost non-@tt{NULL} argument. All arguments must be of compatible types.}

@section{@tt{IN} functions}

@defproc[(sql:in [needle expr+quotable] [haystack (U (listof quotable) query?)]) function?]{
Creates an SQL @tt{IN} function that searches for @scheme[needle] in @scheme[haystack] and returns @scheme[#t] if it is found or @scheme[#f] otherwise.

@scheme[haystack] is a rather non-standard type of function argument. It can be one of the following:

@itemize{
  @item{a list of Scheme literals that can be quoted as literal expressions;}
  @item{a @scheme[query] that selects a single expression or attribute from a source.}}}

@section{Aggregate functions}

@extmore["http://www.postgresql.org/docs/8.3/static/functions-aggregate.html"]{Aggregate functions in PostgreSQL}
Snooze supports the four most common SQL aggregate functions applied to individual columns: @tt{COUNT}, @tt{MAX}, @tt{MIN} and @tt{AVERAGE}. There is also a variant of @tt{COUNT} that can be applied to whole entities and subqueries.

There are many subtleties to the use of aggregate functions in
SQL that Snooze does not concern itself with. For example, different DBMSs support different types of argument for each aggregate function. If your DBMS rejects a query for any reason, Snooze will raise @scheme[exn:fail] and provide you with the error message.

@defproc[(sql:count [column column?]) aggregate?]{
Creates an aggregate function that returns the number of non-null values in the specified column.}

@defproc[(sql:max [column column?]) aggregate?]{
Creates an aggregate function that returns the maximum value in the specified column.}

@defproc[(sql:min [column column?]) aggregate?]{
Creates an aggregate function that returns the minimum value in the specified column.}

@defproc[(sql:average [column column?]) aggregate?]{
Creates an aggregate function that returns the mean value in the specified column.}

@defproc[(sql:count* [arg (U source? #f) #f]) aggregate?]{
Creates an aggregate function that returns the number of non-null rows from the specified entity or subquery (equivalent to the SQL "@tt{COUNT(foo.*)}"). If @schemeid[arg] is omitted or  @scheme[#f], the function returns the number of non-null rows from the current query (equivalent to the SQL "@tt{COUNT(*)}").}
