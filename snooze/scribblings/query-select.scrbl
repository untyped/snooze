#lang scribble/doc

@(require "base.ss")

@title[#:tag "select"]{Select statements}

@(declare-exporting (planet untyped/snooze))

@defproc[(sql:select [#:from     from     (U source? query?)]
                     [#:what     what     (U expression? source-alias? (listof (U expression? source-alias?)) #f) #f]
                     [#:where    where    (U expression? #f) #f]
                     [#:order    order    (listof order?) null]
                     [#:limit    limit    (U integer? #f) #f]
                     [#:offset   offset   (U integer? #f) #f]
                     [#:group    group    (listof (U column? source-alias?)) null]
                     [#:distinct distinct (U expression? #t #f) #f])
         query?]{
Creates a @scheme[query] that can be used on @scheme[find-all], @scheme[find-one] and @scheme[g:find]. The arguments are as follows:

The @scheme[#:from] argument, like the @tt{FROM} clause in SQL, determines the @scheme[source] of the results.

The @scheme[#:what] argument determines the data to be selected. It is analogous to the field list that goes between the keywords @tt{SELECT} and @scheme{FROM} in SQL. The @scheme[what] clause helps Snooze determine the type of @schemeid[result] to return from @scheme[find-all], @scheme[find-one] or @scheme[g:find]. The default value is derived from the @scheme[#:from] clause.

The optional @scheme[#:where] argument, like the @tt{WHERE} clause in SQL, allows you to filter the results returned. The default is a pass-all filter.

The optional @scheme[#:order] argument, like the @tt{ORDER} clause in argument specifies the order in which results should be retrieved. The default is a random order.

The optional @scheme[#:limit] and @scheme[#:offset], like the  @tt{LIMIT} and @tt{OFFSET} clauses in SQL, allow you to specify a subset of the results to retrieve. The default values return all results.

The optional @scheme[#:group] argument, like the @tt{GROUP BY} clause in SQL, lets you group results when using aggregate functions. The default value does no grouping.

The optional @scheme[#:distinct] argument, like the @tt{DISTINCT ON} clause in SQL, specifies a uniqueness constraint on the results returned:

@itemize{
  @item{@scheme[#t] may be used to indicate that rows should be completely unique (the equivalent of the @scheme[equal?] comparison in Scheme);}
  @item{an expression may be used to indicate when two rows are considered equal.}}
  
Omitting the @scheme[#:distinct] argument disables uniqueness checking.}
