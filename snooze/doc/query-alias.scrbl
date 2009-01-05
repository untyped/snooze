#lang scribble/doc

@require[(file "base.ss")]

@title[#:tag "alias"]{Aliases}

To create a query, you first need to @italic{alias} one or more entities 
and attributes. This is analogous to the @scheme{AS} operator from SQL: 
by aliasing a single item more than once, you can refer to it in more 
than one context without ambiguity. While SQL allows you to omit aliases
where you are only referring to an item once, Snooze requires you to be
explicit at all times.

Entity aliases are created as follows:

@defproc[(q:entity [entity entity]) entity-alias]

Creates an alias for the table associated with the supplied 
@schemeid[entity].

@intmore{entity}
Entities are created using the @scheme[define-persistent-struct] macro. 
For example, you could alias a @schemeid[person] entity as follows:

@schemeblock[
  (define-persistent-struct person ([name type:text]))
  (define alias (q:entity entity:person))]

We recommend you wrap all calls to @scheme[q:select] in a @scheme[let]
block that defines the aliases you need for that query. Snooze provides 
two convenient @scheme[let]-style macros for this purpose:

@defform[(let-alias ([id (U entity other-value)] ...) expr ...)]

This form expands to a @scheme[let] block that wraps any entity values 
in calls to @scheme[q:entity]. Non-entity values are left alone. For 
example, the following blocks are equivalent:

@schemeblock[
  (let-alias ([P1  entity:person]
              [P2  entity:person]
              [sum (+ 1 2 3)])
    (q:select #:what (list P1 P2) #:from (q:outer P1 P2)))

  (let ([P1  (q:entity entity:person)]
        [P2  (q:entity entity:person)]
        [sum (+ 1 2 3)])
    (q:select #:what (list P1 P2) #:from (q:outer P1 P2)))]

@defform[(let*-alias ([id (U entity other-value)] ...) expr ...)]

Similar to @scheme[let-alias], but expands into a @scheme[let*] block.

The @scheme[q:attr] procedure creates an attribute alias from an
entity alias: 

@defproc[(q:attr [alias entity-alias] [name symbol]) attribute-alias]

The @schemeid[alias] argument is the entity alias that contains the 
attribute you wish to reference. The @schemeid[name] argument is the
symbolic name of the attribute.

Snooze does a couple of checks at run-time, raising
@scheme[exn:fail:snooze] in the event of failure:

@itemize{
  @item{it makes sure @schemeid[name] refers to an existing attribute
    in the relevant entity description;}
  @item{it does some type checking to make sure you don't use the
    attribute in the wrong type contexts.}}

While entity aliases often need to be defined in @scheme[let] blocks,
attribute aliases can often be inlined in the query. There are some
situations where it is necessary to refer to an attribute unambiguously
more than once (nested queries are one such example), but these are
few and far between. 
