#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "query-lang"]{Procedural query language}

Snooze provides two ways of writing queries:

@itemize{
  @item{a combinator-based @italic{procedural query language}, described below;}
  @item{a thin @italic{syntax wrapper} that provides a layer of convenience on top of the procedural language.}}

This section describes the procedural language.

@include-section{query-alias.scrbl}
@include-section{query-select.scrbl}
@include-section{query-from.scrbl}
@include-section{query-what.scrbl}
@include-section{query-expr.scrbl}
@include-section{query-order.scrbl}
@include-section{query-group.scrbl}
