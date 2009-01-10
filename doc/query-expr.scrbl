#lang scribble/doc

@require[(file "base.ss")]

@define[expr-literals 
  '(string integer symbol boolean time-tai attribute-alias aggregate-function)]

@title[#:tag "expr"]{WHERE and ON clauses}

The @schemeid[where] argument to @scheme[q:select] is analogous to a
@scheme{WHERE} clause in SQL: it lets you filter your search results
based on a test expression. Similarly, the @schemeid[on] arguments to 
@scheme[q:inner], @scheme[q:left] and @scheme[q:right] let
you specify the join criteria based on a similar expression. 

Expressions are values of type @schemeid[expr], built using the
language defined below:

@schemegrammar[
#:literals [q:and q:or q:not q:= q:<> q:< q:> q:<= q:>= q:like q:match q:match-ci q:null? q:in]
expr (q:and term term ...)
     (q:or term term ...)
     (q:not term term ...)
     (q:= atom atom)
     (q:<> atom atom)
     (q:< atom atom)
     (q:> atom atom)
     (q:<= atom atom)
     (q:>= atom atom)
     (q:like atom string)
     (q:match atom string)
     (q:match-ci atom string)
     (q:null? term)
     (q:in term in-source)]

@schemegrammar[
term expr
     atom]

@schemegrammar[
#:literals [attribute-alias aggregate-function]
atom literal
     attribute-alias
     aggregate-function]

@schemegrammar[
#:literals [string integer symbol boolean time-tai]
literal string
        integer
        symbol
        boolean
        time-tai]

@schemegrammar[
#:literals [select listof]
in-source select
          (listof literal)]

@schemeid[expr] non-terminals are procedures that map to equivalent 
Boolean-valued operators in SQL. Snooze does some limited type checking
to make sure you combine @schemeid[terms] and @schemeid[exprs] in 
meaningful ways:

The following expressions accept boolean valued arguments (including
other expressions):

@defproc*[([(q:and [arg term] ...+) expr]
           [(q:or  [arg term] ...+) expr]
           [(q:not [arg term]) expr])]
 
The following expressions accept numeric arguments (including attribute
aliases and aggregate functions):

@defproc*[([(q:=  [arg1 atom] [arg2 atom]) expr]
           [(q:<> [arg1 atom] [arg2 atom]) expr]
           [(q:<  [arg1 atom] [arg2 atom]) expr]
           [(q:>  [arg1 atom] [arg2 atom]) expr]
           [(q:<= [arg1 atom] [arg2 atom]) expr]
           [(q:>= [arg1 atom] [arg2 atom]) expr])]

The following expressions accept string arguments (including attribute
aliases):

@defproc*[([(q:like     [arg1 atom] [arg2 string]) expr]
           [(q:match    [arg1 atom] [arg2 string]) expr]
           [(q:match-ci [arg1 atom] [arg2 string]) expr])]

@extmore["http://www.postgresql.org/docs/8.1/static/functions-matching.html#FUNCTIONS-LIKE"]{LIKE in PostgreSQL}
@scheme[q:like] is analogous to @scheme{LIKE} from SQL.
The second argument is a pattern string. This is effectively a literal
string with two wildcards: @scheme{%}, meaning zero or more unspecified
characters, and @scheme{_}, meaning precisely one unspecified character.

@extmore["http://www.postgresql.org/docs/8.1/static/functions-matching.html#FUNCTIONS-POSIX-REGEXP"]{POSIX Regular Expressions in PostgreSQL}
@scheme[q:match] and @scheme[q:match-ci] perform case
sensitive and case-insensitive regular expression matching respectively.
These are analogous to the @scheme{~} and @scheme{~*} operators in
PostgreSQL and are @italic{not supported} in SQLite.

The following expressions allow arguments of any type:

@defproc*[([(q:null? [arg term]) expr]
           [(q:in [arg1 term] [arg2 (U select (listof literal))]) expr])]

@extmore["http://www.postgresql.org/docs/8.1/static/functions-comparison.html"]{IS NULL in PostgreSQL}
@scheme[q:null?] is analogous to the @scheme{IS NULL}
operator in SQL. Snooze prevents you writing the equivalent of 
@scheme{blah = NULL} because SQL's @scheme{NULL} is not equal to itself.

@scheme[q:in] is analogous to the @scheme{IN} operator in SQL. You can
specify a subquery or a list of literal values to match against.

@;{
@defproc[(q:count [field field]) aggregate-function]
@defproc[(q:count* [alias entity-alias]) aggregate-function]
@defproc[(q:max [field field]) aggregate-function]
@defproc[(q:min [field field]) aggregate-function]
@defproc[(q:average [field field]) aggregate-function]
}
