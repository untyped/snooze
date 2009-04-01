#lang scribble/doc

@(require "base.ss")

@title[#:tag "persistent-struct"]{Persistent structures}

@(declare-exporting (planet untyped/snooze))

@intmore{era} A @italic{persistent structure} is a PLT structure that Snooze can save to a database. Snooze attaches @italic{entity metadata} to each persistent struct type via a property called @scheme[prop:entity]. The metadata serves two purposes:

@itemize{
  @item{it tells Snooze how to serialize and deserialize structs of that type;}
  @item{@intmore{query}it indirectly allows the programmer to refer to the entity in queries.}}

A persistent structure type with @italic{n} fields is mapped to a database table with @italic{n+2} columns: an integer @scheme[id] which acts as a primary key for table, an integer @scheme[revision] number which helps prevent against concurrent writes in multi-threaded applications, and a column for each attribute in the type definition. @scheme[ids] and @scheme[revisions] are automatically allocated by Snooze and should not normally be updated by application code: the lifecycle of these fields is described in @secref{save+delete}.

@include-section{define-persistent-struct.scrbl}
@include-section{id-revision.scrbl}
