#lang scribble/doc

@require[(file "base.ss")]

@title[#:tag "what"]{WHAT clauses}

The @schemeid[what] argument to @scheme[q:select] specifies the type of
result you wish to retrieve from the database. It can either be a single
@schemeid[selectable] or a @schemeid[(listof selectable)]. The result type is
affected as follows:

@itemize{
  @item{if @schemeid[what] is a single @schemeid[selectable], the query
    @schemeid[results] will be single values of the appropriate type;}
  @item{if @schemeid[what] is a @schemeid[(listof selectable)], the query
    @schemeid[results] will be lists of values of the appropriate type(s).}}

A @schemeid{selectable} is one of the following:

@itemize{
  @item{an @schemeid[entity-alias], as returned by @scheme[q:entity];}
  @item{a @schemeid[field], as returned by @scheme[q:attr] or 
    @scheme[q:field];}
  @item{an @schemeid[aggregate-function], as returned by @scheme[q:count],
    @scheme[q:count*] or the like.}}

Each @schemeid[selectable] creates a particular type of query result:
@schemeid[entity-aliases] create persistent structs, while
@schemeid[fields] and @schemeid[aggregate-functions] create Scheme values
of the corresponding type.

For example, the following code:

@schemeblock[
  (define-persistent-struct person ([name type:text]))
  (find-all 
   (let-alias ([P1 entity:person]
               [P2 entity:person])
     (q:select #:what (list P1 (q:attr P1 'name) (q:count* P2))
               #:from ...)))]

would return a @schemeid[(listof (list person string integer))].
