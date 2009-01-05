#lang scribble/doc

@require[(file "base.ss")]

@title[#:tag "queries" #:style 'toc]{Queries}

Queries, as you might expect, involve retrieving values from the database.
Snooze uses a combinator-based query language that expands into SQL. You
can use this language to retrieve whole persistent structs or single values.

@local-table-of-contents[]

@include-section{query-procedures.scrbl}
@include-section{query-language.scrbl}
@include-section{generators.scrbl}
