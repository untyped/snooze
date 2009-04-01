#lang scribble/doc

@(require "base.ss")
          
@title[#:style '(toc) #:tag "query-syntax"]{Query syntax}

Formally, @scheme[sql] blocks have the syntax below.
The parts of the syntax are described in subsequent sections:

@defform/subs[
  #:literals (select or and unquote inner outer left right asc desc)
  (sql top)
  ([top            query
                   order
                   expr]
   [select         (select from
                           maybe-what 
                           maybe-where
                           maybe-group
                           maybe-having
                           maybe-order
                           maybe-limit
                           maybe-offset
                           maybe-distinct)]
   [from           (code:line #:from source)]
   [maybe-what     (code:line #:what what)
                   (code:line #:what (what ...))
                   (code:line)]
   [maybe-where    (code:line #:where expr)
                   (code:line #:where #f)
                   (code:line #:where (unquote scheme-expr))
                   (code:line)]
   [maybe-group    (code:line #:group (group ...))
                   (code:line #:group (unquote scheme-expr))
                   (code:line)]
   [maybe-having   (code:line #:having expr)
                   (code:line #:having #f)
                   (code:line)]
   [maybe-order    (code:line #:order (order ...))
                   (code:line #:order (unquote scheme-expr))
                   (code:line)]
   [maybe-limit    (code:line #:limit integer)
                   (code:line #:limit #f)
                   (code:line #:limit (unquote scheme-expr))
                   (code:line)]
   [maybe-offset   (code:line #:offset integer)
                   (code:line #:offset #f)
                   (code:line #:offset (unquote scheme-expr))
                   (code:line)]
   [maybe-distinct (code:line #:distinct #t)
                   (code:line #:distinct expr)
                   (code:line #:distinct #f)
                   (code:line #:distinct (unquote scheme-expr))
                   (code:line)]
   [source         (code:line entity-alias)
                   (code:line query-alias)
                   (code:line query)
                   (code:line (outer source source))
                   (code:line (inner source source expr))
                   (code:line (left  source source expr))
                   (code:line (right source source expr))
                   (code:line (unquote scheme-expr))]
   [what           (code:line expr)
                   (code:line entity-alias)
                   (code:line query-alias)
                   (code:line (unquote scheme-expr))]
   [group          (code:line expr)
                   (code:line entity-alias)
                   (code:line query-alias)
                   (code:line (unquote scheme-expr))]
   [expr           (code:line (or expr ...))
                   (code:line (and expr ...))
                   ...
                   attribute-alias
                   expression-alias
                   literal
                   (code:line (unquote scheme-expr))]
   [order          (code:line (asc column))
                   (code:line (desc column))
                   (code:line (unquote scheme-expr))])]

@include-section{query-from.scrbl}
@include-section{query-what.scrbl}
@include-section{query-where.scrbl}
@include-section{query-order.scrbl}
@include-section{query-group.scrbl}
@include-section{query-limit.scrbl}
@include-section{query-distinct.scrbl}
@include-section{query-alias.scrbl}
@include-section{query-proc.scrbl}
