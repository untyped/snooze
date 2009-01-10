#lang scribble/doc

@(require (file "base.ss"))

@title[#:style '(toc) #:tag "snooze"]{@bold{Snooze:} Object Relational Mapping of PLT Structures}

Dave Gurnell, David Brooks, Noel Welsh, and Matt Jadud

@tt{{dave, noel, matt} at @link["http://www.untyped.com"]{@tt{untyped}}}

Snooze is an Object Relational Mapping (ORM) library similar to ActiveRecord or Hibernate. It provides a mapping from structures in PLT Scheme to rows in a relational database.

Snooze currently includes back-end code to connect to SQLite 3.6+ and PostgreSQL 8.3+ databases.

@section{Known issues}

@itemize{
  @item{The documentation is incomplete.}
  @item{Querying joins over multiple tables does not work correctly in SQLite.}}

@local-table-of-contents[]

@include-section{quick.scrbl}
@include-section{modules.scrbl}
@include-section{persistent-struct.scrbl}
@include-section{era.scrbl}
@include-section{snooze-class.scrbl}
@include-section{query.scrbl}
@include-section{acknowledgements.scrbl}
