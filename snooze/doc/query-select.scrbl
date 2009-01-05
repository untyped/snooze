#lang scribble/doc

@require[(file "base.ss")]

@title[#:tag "select"]{SELECT statements}

@intmore{query-procedures}
The @scheme[q:select] procedure creates a @scheme[select] statement to
be used as an argument to @scheme[find-all], @scheme[find-one] or
@scheme[g:find]:

@defproc[(q:select [#:what   what   (U selectable (listof selectable)) #f]
                   [#:from   from   source]
                   [#:where  where  expr #f]
                   [#:order  order  (listof order) null]
                   [#:limit  limit  (U integer #f) #f]
                   [#:offset offset (U integer #f) #f]
                   [#:group  group  (listof groupable) null])
         select]
         
The arguments are as follows:

@intmore{what}
The @schemeid[what] argument determines the data to be selected. It is
analogous to the field list that goes between the keywords 
@scheme{SELECT} and @scheme{FROM} in SQL, and it helps Snooze determine
the type of @schemeid[result] to return from @scheme[find-all], 
@scheme[find-one] or @scheme[g:find].

In some cases, the @schemeid[what] clause may be omitted: see 
@secref{from} for more information.

@intmore{from}
The @schemeid[from] argument determines the @schemeid{source} of the 
results: it is analogous to the @scheme{FROM} clause in SQL.

@intmore{expr}
The optional @schemeid[where] argument specifies the equivalent of an 
SQL @scheme{WHERE} clause. Setting the argument to @scheme[#f] is 
equivalent to omitting it altogether. 

@intmore{order}
The optional @schemeid[order] argument specifies the equivalent of an 
SQL @scheme{ORDER} clause. Setting the argument to @scheme[null] is 
equivalent to omitting it altogether. 

The optional @schemeid[limit] and @schemeid[offset] arguments specify
the equivalent of SQL @scheme{LIMIT} and @scheme{OFFSET} clauses. Setting
either argument to @scheme{#f} is equivalent to omitting it altogether.

@intmore{group}
The optional @schemeid[group] argument specifies the equivalent of an SQL 
@scheme{GROUP BY} clause, for use in queries containing aggregate 
functions. Setting the argument to @scheme[null] is equivalent to 
omitting it altogether. 
