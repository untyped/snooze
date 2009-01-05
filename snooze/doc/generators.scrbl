#lang scribble/doc

@require[(file "base.ss")]
@require[(planet "gen.ss" ("untyped" "unlib.plt" 2))]

@title[#:tag "generators"]{Working with generators}

@italic{Generators} are a lightweight iteration mechanism similar to
the @italic{ports} of R5RS Scheme and the @italic{streams} of SRFI 40.
They form a convenient, lightweight way of iterating through and 
modifying large datasets such as those returned by Snooze's 
@scheme[g:find] procedure.

@section{What is a generator?}

A generator is a thunk of type @scheme[(-> (U A g:end))], written 
@scheme[(gen-> A)] for short:

@defthing[gen-> contract]

@scheme[g:end] is a unique symbol:

@defthing[g:end symbol]

A generator is defined on a @italic{source}, which can be any sequence
of values: lists, vectors, input-streams and other generators are all
valid candidates. Repeated calls to the generator return successive 
results from the source until it has been exhausted. After this point,
all subsequent calls return @scheme[g:end].

We can illustrate this with the procedure @scheme[list->generator],
which creates a generator from a list:

@defproc[(list->generator [the-list (listof A)])
         (gen-> A)]

@interaction[
  (define g (list->generator (list 1 2 3)))
  (g)
  (g)
  (g)
  (g)
  (g)]

The important point is that @scheme[g:end] is completely unique and 
always marks the end of the generator's data source. You can test for
@scheme[g:end] using the @scheme[g:end?] predicate:

@defproc[(g:end? [item any]) boolean]

@interaction[
  (g:end? 123)
  (g:end? g:end)]

In Snooze, generators are typically created by calling @scheme[g:find]
on a query. Snooze provides a library of generator combinators with
equivalents to @scheme[fold], @scheme[map], @scheme[for-each] and the
like. Generator combinators are more memory-efficient than list 
combinators as they do not create lists of intermediate data.

@section{Combinators}

@defproc[(g:filter [fn     (A -> boolean)]
                   [source (gen-> A)])
         (gen-> A)]

The generator equivalent of @scheme[filter]. Given a predicate @scheme[fn]
and a @scheme[source] generator, creates a generator that returns the
values of @schemeid[source] for which @schemeid[(fn (source))] is 
non-@scheme[#f]. Note that a single call to the filter generator may 
result in multiple calls to the @schemeid[source]. Exhausted when the 
@schemeid[source] is exhausted.

@defproc[(g:map [fn     (A ...+ -> B)] 
                [source (gen-> A)] ...+)
         (gen-> B)]
         
The generator equivalent of @scheme[map]. Given a mapping function 
@schemeid[fn] and one or more @schemeid[source] generators, creates a 
generator that returns the results of @schemeid[(apply map (list (source) ...))].
Exhausted when one or more of the @schemeid[sources] is exhausted.

@defproc[(g:filter-map [fn     (A -> (U B #f))]
                       [source (gen-> A)])
         (gen-> B)]
         
The generator equivalent of @scheme[filter-map]. Given a mapping function 
@schemeid[fn] and one or more @schemeid[source] generators, creates a 
generator that returns the non-@scheme[#f] values of 
@schemeid[(apply map (list (source) ...))]. Note that one call to the map
generator may result in one or more calls to each @scheme[source]. 
Exhausted when one or more of the @schemeid[sources] is exhausted.

@defproc[(g:fold-map [fn      (A ...+ accumulator -> accumulator)]
                     [initial accumulator] 
                     [source  (gen-> A)] ...+)
         (gen-> accumulator)]

An equivalent of @scheme[fold]. Given an iterator function @schemeid[fn],
an @scheme[initial] accumulator, and one or more @schemeid[source] 
generators, creates a generator that iteratively calculates and returns 
the value of @schemeid[(apply fn (list (source) ... accumulator))]. Once
calculated the return value becomes the accumulator for the next iteration.
Exhausted when one or more of the @schemeid[sources] is exhausted.

For example:

@interaction[
  (define g (g:fold-map + 0 (list->generator (list 1 2 3))))
  (g)
  (g)
  (g)
  (g)]

@defproc[(g:remove-duplicates [source (gen-> A)]
                              [same?  (A A -> boolean) equal?])
         (gen-> A)]

Creates a generator that removes duplicates from the @scheme[source] by
comparing consecutive result values with a predicate @scheme[same?].
Skips on until the current result is different from the last. Exhausted 
when the @scheme[source] is exhausted.

For example:

@interaction[
  (define g 
    (g:remove-duplicates 
     (list->generator (list 1 1 1 2 2 2))))
  (g)
  (g)
  (g)]
  
@defproc[(g:debug [message string]
                  [source (gen-> A)])
         (gen-> A)]

Creates a generator that prints each value from @scheme[source] to the 
console using @scheme[printf] as a side-effect. The supplied 
@scheme[message] is used to prefix each printed value. The returned
values are identical to those of @scheme[source].

@defproc[(g:project [mask   (listof boolean)]
                    [source (gen-> (listof A))]
                    [same?  (A A -> boolean) eq?])
         (-> (gen-> (append (listof A) (listof (listof A)))))]

@extmore["http://en.wikipedia.org/wiki/Projection_%28relational_algebra%29"]{Projections in Wikipedia}
The most complex of the built-in combinators: performs the equivalent 
of a projection from relational algebra. Creates a generator that maintains
two accumulators:

@itemize{
  @item{@scheme[key-accum]: a list of @italic{keys}, one for each @scheme[#t] value in 
    @scheme[mask];}
  @item{@scheme[projection-accum]: a list of @italic{projections}, where 
    each projection is a list of @italic{non-key} values, one for each 
    @scheme[#f] value in @scheme[mask].}}

Each iteration, the generator pulls in one result (which has to be a 
list) from @scheme[source]. It partitions the the result into @scheme[keys]
and @scheme[non-keys], and compares each member of @scheme[key] to the
corresponding member of @scheme[key-acccum] using @scheme[same?]:

@itemize{
  @item{if all @scheme[keys] match, the generator adds @scheme[non-keys]
    @scheme[projection-accum] and iterates;}
  @item{if one or more @scheme[keys] do nt match, the generator emits
    @scheme[(append key-accum projection-accum)] and replaces 
    @scheme[key-accum] with @scheme[keys] and @scheme[projection-accum]
    with @scheme[(list non-keys)].}}
    
This effectively means the generator groups results together according
to key values.

For example:

@interaction[
  (define data
    (list (list 0 0 0)        
          (list 0 0 1)
          (list 0 1 0)        
          (list 0 1 1)
          (list 1 0 0)        
          (list 1 0 1)        
          (list 1 1 0)
          (list 1 1 1)))
  (code:comment "g1 : (gen-> (list integer (listof (list integer integer))))")
  (define g1
    (g:project (list #t #f #f)
               (list->generator data)))
  (g1)
  (g1)
  (g1)
  (code:comment "g2 : (gen-> (list integer integer (listof (list integer))))")
  (define g2
    (g:project (list #t #t #f)
               (list->generator data)))
  (g2)
  (g2)
  (g2)
  (g2)
  (g2)
  (code:comment "g3 : (gen-> (list integer integer integer (listof (list))))")
  (define g3
    (g:project (list #t #t #t)
               (list->generator data)))
  (g3)
  (g3)
  (g3)
  (code:comment "and so on...")]

@scheme[g:project] is especially useful when used in conjunction with
@scheme[match-lambda].
 
@section{Consumers}

These procedures take one or more generators as arguments and call them
repeatedly until they are exhausted, either collecting a result or for 
their side effects.

@defproc[(g:for-each [fn     (A ...+ -> void)] 
                [source (gen-> A)] ...+)
         void]

The generator equivalent of @scheme[for-each]. Repeatedly calls 
@scheme[source] until it is exhausted, then returns @scheme[void].
   
@defproc[(g:collect [source (gen-> A)])
         (listof A)]

Repeatedly calls @scheme[source] until it is exhausted, collecting and 
returning the results as a list.

For example:

@interaction[
  (g:collect (g:map + (list->generator (list 1 2 3))
                      (list->generator (list 1 2 3))))]

@defproc[(g:fold [fn      (A ...+ accumulator -> accumulator)]
                 [initial accumulator] 
                 [source  (gen-> A)] ...+)
         accumulator]

An equivalent of @scheme[fold]. Given an iterator function @schemeid[fn],
an @scheme[initial] accumulator, and one or more @schemeid[source] 
generators, repeatedly calculates the value of an @italic{accumulator}
as @schemeid[(apply fn (list (source) ... accumulator))]. Once calculated,
the value becomes the accumulator for the next iteration. The result is
the value of the accumulator when one or more @scheme[sources] is 
exhausted.

For example:

@interaction[
  (g:fold + 0 (list->generator (list 1 2 3)))]
