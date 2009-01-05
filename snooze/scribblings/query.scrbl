#lang scribble/doc

@(require (file "base.ss"))
          
@title[#:tag "query"]{Queries}

@extmore["http://en.wikipedia.org/wiki/SQL"]{SQL on Wikipedia} 
Snooze uses a combinator-based query language that supports the most
useful bits of SQL including joins, nested queries, aggregate functions
and limits and offsets.

Procedures in the query language are prefixed with @schemeid[sql:] to 
distinguish them from standard Scheme operators. Snooze also provides a @seclink["query-syntax"]{syntax layer} that removes the need for these prefixes.

@include-section{query-proc.scrbl}
@include-section{generators.scrbl}
@include-section{query-lang.scrbl}
@include-section{query-syntax.scrbl}
@include-section{query-struct.scrbl}
