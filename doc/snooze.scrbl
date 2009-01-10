#lang scribble/doc

@require[(file "base.ss")]

@title[#:style '(toc)]{Snooze}

by Dave Gurnell, Noel Welsh and Matt Jadud

@tt{{dave, noel, matt} at @link["http://www.untyped.com"]{@tt{untyped}}}

Snooze is an Object Relational Mapping (ORM) library similar to ActiveRecord or Hibernate. It provides a mapping from structures in PLT Scheme to rows in a relational database.

Snooze currently supports SQLite 3.x and PostgreSQL 8.x database back-ends.

@local-table-of-contents[]

@include-section{intro.scrbl}
@include-section{persistent-struct.scrbl}
@include-section{era.scrbl}
@include-section{queries.scrbl}
@include-section{acknowledgements.scrbl}
