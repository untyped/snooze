#lang scribble/doc

@(require "base.ss")
        
@title{Procedural query language}

@(declare-exporting (planet untyped/snooze))

The query language described above is a syntax wrapper for a
set of constructor procedures that build a kind of SQL AST tree
that can be rendered as textual SQL.

While the procedural language is more verbose than the syntax
language, its compatibility with standard Scheme procedures such
as @scheme[apply] and @scheme[map] makes it useful in a few
exceptional cases.

The elements of the procedural language are documented below
for completeness.

TODO: Complete this...