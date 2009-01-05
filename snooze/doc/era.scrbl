#lang scribble/doc

@require[(file "base.ss")]

@title{Entities, relationships and attributes}

@section{Persistent structures}

TODO

@defform[
  (define-persistent-struct id
    ([id type] ...))]

@defform[
  (provide-persistent-struct id
    ([id type] ...))]

@defform[
  (define/provide-persistent-struct id
    ([id type] ...))]

@section{Field types}

TODO

@defthing[type:text type]
@defthing[type:symbol type]
@defthing[type:integer type]
@defthing[type:real type]
@defthing[type:boolean type]
@defthing[type:time-tai type]

@section{Entity metadata}

TODO
