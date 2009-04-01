#lang scribble/doc

@(require "base.ss")

@title[#:tag "where"]{The @scheme[#:where] clause}

The @scheme[#:where] clause lets you filter the data selected by the
@scheme[#:from] clause. The clause takes the form of a Boolean expression that
specifies whether any given row should be kept in the final result.

The expression language, which is a subset of the query language, is described 
below. Expressions are used in various clauses besides @scheme[#:where], including
the @scheme[#:distinct] and @scheme[#:having] clauses and join conditions.

These forms are essentially wrappers for SQL expressions, so SQL semantics 
prevail over Scheme semantics. For example, @schemeidfont{=} can be used to 
compare values of any type.

@section{Expression types}

Each expression has a type, which is one of the following:

@itemize{
  @item{@italic{Boolean};}
  @item{@italic{numeric}, with @italic{integer} and @italic{real} subtypes;}
  @item{@italic{character}, with @italic{string} and @italic{symbol} subtypes;}
  @item{@italic{temporal}, with @italic{time-utc} and @italic{time-tai} subtypes.}}

The procedures below all operate on Scheme literals and other expressions. Scheme
literals are automatically quoted as expressions of the relevant type. The type
of an expression is determined by the types of its arguments. 

The complete @scheme[#:where] clause must be of type Boolean.

@section{Boolean operators}

These functions operate on Boolean arguments and produce Boolean results:

@specform[(or arg ...)]{
The Boolean @tt{OR} of the @scheme[arg]@schemeidfont{ument}s.}

@specform[(and arg ...)]{
The Boolean @tt{AND} of the @scheme[arg]@schemeidfont{ument}s.}

@specform[(not arg)]{
The Boolean negation of the @scheme[arg]@schemeidfont{ument}.}

@section{Comparison functions}

These functions operate on numeric, Boolean, string, symbolic or temporal arguments, as long as consistent types are used. For example, @scheme[(= "str" 'str)] is a valid comparison whereas @scheme[(= "str" 123)] is not. The  result type is always Boolean.

Equality and inequality predicates perform @scheme[equal?] style (i.e. whole-content) comparison on their arguments:

@specform[(= arg1 arg2)]
@specform[(<> arg1 arg2)]

Comparison functions perform numerical, lexical or temporal comparisons as appropriate:

@specform[(< arg1 arg2)]
@specform[(> arg1 arg2)]
@specform[(<= arg1 arg2)]
@specform[(>= arg1 arg2)]

@section{Mathematical functions}

Addition and subtraction can be used on numeric or temporal arguments, producing a numeric or temporal result as appropriate:

@specform[(+ arg ...)]
@specform[(- arg ...)]

Other mathematical operators can be applied to numeric arguments only:

@specform[(* arg ...)]
@specform[(/ arg1 arg2)]
@specform[(abs arg)]
@specform[(floor arg)]
@specform[(ceiling arg)]
@specform[(round arg)]

@section{Character pattern matching functions}

@extmore["http://www.postgresql.org/docs/8.3/interactive/functions-matching.html"]{Pattern matching in PostgreSQL}
Pattern matching functions take character arguments and return Boolean results:

@specform[(like str pattern)]{
@extmore["http://www.postgresql.org/docs/8.3/static/functions-matching.html#FUNCTIONS-LIKE"]{LIKE in PostgreSQL}
The SQL @tt{LIKE} function with support for @scheme{%} and @scheme{_} wildcards in the @scheme[pattern].}

@specform[(regexp-match str pattern)]{
@extmore["http://www.postgresql.org/docs/8.3/static/functions-matching.html#FUNCTIONS-POSIX-REGEXP"]{POSIX Regular Expressions in PostgreSQL}
POSIX regular expression pattern matching (the @scheme{~} operator in PostgreSQL).}

@specform[(regexp-match-ci str pattern)]{
Case insensitive POSIX regular expression matching (the @scheme{~*} operator in PostgreSQL).}

@section{Character manipulation functions}

The following functions operator on character (string/symbol) arguments. If all arguments are symbols the result is a symbol, otherwise it is a string:

@specform[(string-append arg ...)]{
Concatenates the @scheme[arg]@schemeidfont{s}.}

@specform[(string-replace haystack needle replacement)]{
Searches for @scheme[needle] in @scheme[haystack] and replaces all occurrences with @scheme[replacement].}

@specform[(regexp-replace haystack pattern replacement)]{
Searches for the POSIX regular expression @scheme[pattern] in @scheme[haystack] and replaces the first occurrence with @scheme[replacement].}

@specform[(regexp-replace* haystack pattern replacement)]{
Like @scheme[regexp-replace] but replaces all occurrences of @scheme[pattern] rather than just the first.}

@specform[(regexp-replace-ci haystack pattern replacement)]{
Case insensitive version of @scheme[regexp-replace].}

@specform[(regexp-replace*-ci haystack pattern replacement)]{
Case insensitive version of @scheme[regexp-replace*].}

@specform[(->string datum format-string)]{
@extmore["http://www.postgresql.org/docs/8.3/static/functions-formatting.html"]{String formatting functions in PostgreSQL}
Equivalent of PostgreSQL's @tt{TO_CHAR} fucntion: converts any data type to a string.
@scheme[format-string] is a character expression that specifies the format to use in
the conversion (see the PostgreSQL documentation for examples).}

@specform[(->symbol datum format-string)]{
Like @scheme[->string] but returns a symbol result.}

@section{Conditional functions}

@specform[(if test then)]
@specform[(if test then else)]{
Performs an if-then-else test on its arguments. @scheme[test] must be Boolean valued.
If @scheme[test] evaluates to @scheme[#t], the value of @scheme[then] is calculated
and returned. If @scheme[test] evaluates to @scheme[#f], the value of @scheme[else] 
is calucated and returned instead. If @scheme[test] evaluates to @scheme[#f] and 
@scheme[else] is omitted, @tt{NULL} is returned.

@scheme[then] and @scheme[else] must have the same type: the return value is a 
function of the same type.}

@specform/subs[#:literals (else)
               (cond clause ...)
               ([clause [test-expr value-expr]
                        [else      value-expr]])]{
@scheme[cond]-like syntax that expands into a chain of @scheme[if]
expressions. All @scheme[value-expr]@schemeidfont{s} must have the
same types.}

@section{Functions related to @tt{NULL}}

@specform[(null? arg)]{
@extmore["http://www.postgresql.org/docs/8.3/static/functions-comparison.html"]{IS NULL in PostgreSQL}
Determines whether the argument is @tt{NULL}. Direct comparison with
@tt{NULL} using @scheme[=] does not work because in SQL semantics
@tt{NULL} is not equal to itself (see @scheme[null?] instead).}

@specform[(coalesce arg ...)]{
Returns the value of the leftmost non-@tt{NULL} argument. All arguments
must be of compatible types.}

@section{The @tt{IN} function}

@specform[(in needle haystack)]{
Equivalent of the SQL @tt{IN} function that searches for @scheme[needle]
in @scheme[haystack]. Returns @scheme[#t] if it is found or @scheme[#f] otherwise.

@scheme[haystack] can be one of the following:

@itemize{
  @item{a list of Scheme literals that can be quoted as literal expressions;}
  @item{a @scheme[query] with a single @scheme[#:what] item.}}}

@section{Aggregate functions}

@extmore["http://www.postgresql.org/docs/8.3/static/functions-aggregate.html"]{Aggregate functions in PostgreSQL}
Snooze supports the four most common SQL aggregate functions applied
to individual columns: @tt{COUNT}, @tt{MAX}, @tt{MIN} and @tt{AVERAGE}.
There is also a variant of @tt{COUNT} that can be applied to whole 
entities and subqueries (the equivalent of @tt{COUNT(*)}).

There are many subtleties to the use of aggregate functions in SQL that
Snooze does not concern itself with. For example, different DBMSs support
different types of argument for each aggregate function. If your DBMS
rejects a query for any reason, Snooze will raise @scheme[exn:fail]
and provide you with an appropriate error message.

@specform[(count arg)]{
Returns the number of non-@tt{NULL} values in the @scheme[arg]@schemeidfont{ument}.}

@specform[(max arg)]{
Returns the maximum value in the @scheme[arg]@schemeidfont{ument}.}

@specform[(min arg)]{
Returns the minimum value in the specified @scheme[arg]@schemeidfont{ument}.}

@specform[(average arg)]{
Returns the mean value in the specified @scheme[arg]@schemeidfont{ument}.}

@specform[(count*)]
@specform[(count* arg)]]{
Returns the number of non-null rows from the specified entity or subquery
(equivalent to the SQL "@tt{COUNT(foo.*)}"). If @scheme[arg] is omitted or 
@scheme[#f], the function returns the number of non-null rows from the current
query (equivalent to the SQL "@tt{COUNT(*)}").}

@intmore["group"] If you list aggregate and non-aggregate results in the 
@scheme[#:what] clause of a query, you may additionally have to specify a
@scheme[#:group] clause to determine how the results are aggregated.
