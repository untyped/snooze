#lang scribble/doc

@require[(file "base.ss")]

@title[#:tag "order"]{ORDER clauses}

The @schemeid[order] argument to @scheme[q:select] is analogous to the
@scheme{ORDER} operator on SQL: it lets you specify the order in which
you would like to receive your results.

You specify orders as a list of @schemeid[order] terms, constructed with
the procedures below. Terms should be listed in descending order of
precedence.

@defproc[(q:order [item (U attribute-alias aggregate-function)]
                  [dir (U 'asc 'desc)])
         order]

Sorts by @scheme[item] in ascending or descending order.

@defproc[(q:asc [item  (U attribute-alias aggregate-function)])
         order]

Short-hand for @schemeid[(q:order item 'asc)].

@defproc[(q:desc [item  (U attribute-alias aggregate-function)])
         order]

Short-hand for @schemeid[(q:order item 'desc)].
