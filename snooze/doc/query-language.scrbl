#lang scribble/doc

@require[(file "base.ss")]

@title[#:style 'toc]{Query language}

@extmore["http://en.wikipedia.org/wiki/SQL"]{SQL on Wikipedia} 
Snooze uses a combinator-based query language that supports the most
useful bits of SQL including joins, nested queries, aggregate functions
and limits and offsets.

Procedures in the query language are prefixed with @schemeid[q:] to 
distinguish them from standard Scheme operators.

@local-table-of-contents[]

@include-section{query-alias.scrbl}
@include-section{query-select.scrbl}
@include-section{query-what.scrbl}
@include-section{query-from.scrbl}
@include-section{query-expr.scrbl}
@include-section{query-order.scrbl}
@include-section{query-aggregate.scrbl}
@include-section{query-group.scrbl}
