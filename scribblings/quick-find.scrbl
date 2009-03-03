#lang scribble/doc

@(require "base.ss")

@title[#:tag "quick-find"]{Quick find procedures}

@defmodule[(planet untyped/snooze/quick-find)]{

@italic{Quick find} procedures provide convenient shorthands for retrieving persistent structs based on the values of their attributes. The procedures can be defined on a per-entity basis using the constructor macros @scheme[custom-find-one], @scheme[custom-find-all], @scheme[custon-find-count] and @scheme[custom-g:find].

The constructor macros are described below and the quick find procedures themselves at the bottom of the page.

@defform/subs[
  #:literals (asc desc)
  (custom-find-one entity kw-arg ...)
  ([kw-arg (code:line #:order (order-clause ...))])]{
Defines a quick find wrapper for @scheme[find-one]. @scheme[entity] is the struct type identifier for an entity (e.g. @scheme[student]) and the @scheme[order-clause]@schemeidfont{s} are as they appear in the syntax query language (see @scheme[sql]).}

@defform/subs[
  #:literals (asc desc)
  (custom-find-all entity kw-arg ...)
  ([kw-arg (code:line #:order (order-clause ...))])]{
Like @scheme[custom-find-one] but defines a wrapper for @scheme[find-all].} 

@defform/subs[
  #:literals (asc desc)
  (custom-g:find entity kw-arg ...)
  ([kw-arg (code:line #:order (order-clause ...))])]{
Like @scheme[custom-find-one] but defines a wrapper for @scheme[g:find].} 

@defform/subs[
  #:literals (asc desc)
  (custom-find-count entity kw-arg ...)
  ([kw-arg (code:line #:order (order-clause ...))])]{
Like @scheme[custom-find-one] but defines a wrapper that finds the count of the matching structures in the database.}

The quick-find procedures produced are keyword procedures that accept keywords with the same names as the attributes of the relevant entity. For example, the @scheme[find-person] procedure below:

@schemeblock[
  (define-persistent-struct person
    ([name        type:string]
     [age         type:integer]
     [programmer? type:boolean]))

  (define find-person
    (custom-find-one person #:order ((asc person-name))))]

would accept keyword arguments for @scheme[#:name], @scheme[#:age] and @scheme[#:programmer?]. These attribute arguments can be used in the following ways:

@itemize{
  @item{Passing a boolean, numeric, string, symbol, time-tai or time-utc literal matches structs with a specific attribute value.}
  @item{Passing a list of literals matches structs with any corresponding attribute value.}
  @item{Passing @scheme[#f] for any non-boolean attribute matches structs with  @tt{null} attributes.}
  @item{Passing @scheme[void] matches any value at all (useful when wrapping quick-find calls in other procedures).}
  @item{Arbitrary expressions may be specified by passing a procedure of type @scheme[(attribute-alias -> expression)].}}

Quick find procedures also accept @scheme[#:limit] and @scheme[#:offset] arguments of type @scheme[(U natural#f)].

Examples:

@schemeblock[
  (code:comment "SELECT FROM person WHERE name = 'Dave';")
  (find-person #:name "Dave") 
  (code:comment "SELECT FROM person WHERE name = 'Dave' AND age = 30;")
  (find-person #:name "Dave" #:age 30)
  (code:comment "SELECT FROM person WHERE name IN ('Dave', 'Noel', 'Matt');")
  (find-person #:name (list "Dave" "Noel" "Matt"))
  (code:comment "SELECT FROM person WHERE name IS NULL AND \"programmer?\" = false;")
  (find-person #:name #f #:programmer? #f) 
  (code:comment "SELECT FROM person;")
  (find-person #:name (void)) 
  (code:comment "SELECT FROM person WHERE name ~ 'D.*';")
  (find-person #:name (lambda (attr) (sql:regexp-match attr "D.*")))]

} @;{end defmodule}