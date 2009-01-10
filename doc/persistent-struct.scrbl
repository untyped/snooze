#lang scribble/doc

@require[(lib "plt-match.ss")]

@require[(file "base.ss")]
@require[(file "db.ss")]

@title{Persistent structures}

@intmore{era}
A @italic{persistent structure} is a PLT structure that Snooze can save
to a database. Snooze attaches @italic{entity metadata} to each persistent
struct type. This metadata serves two purposes:

@itemize{
  @item{it tells Snooze how to serialize and deserialize structs of that
    type;}
  @item{@intmore{queries}it lets you refer to the structure type and its
    associated database table(s) in queries.}}

A structure type with @italic{n} fields is mapped to a database table
with @italic{n+2} columns:

@itemize{
  @item{an integer @scheme[id] which acts as a primary key for table;}
  @item{an integer @scheme[revision] number which helps prevent against
    concurrent writes in multi-threaded applications;}
  @item{a column for each field in the type.}}

@scheme[ids] and @scheme[revisions] can be accessed, but are automatically
allocated by Snooze and should not be updated by application code.

@intmore{save+delete}
When a persistent structure is first created, its @scheme[id] and 
@scheme[revision] are set to @scheme[#f] and it has no corresponding 
record in the database. The structure is inserted into the database
with the programmer's first call to @scheme[save!]

@section{Defining persistent structure types}

@defform[
  (define-persistent-struct id
    ([field-id type] ...+))]{

@intmore{type}
Creates a new persistent structure, and binds variables related to the
new structure type. The @scheme[_types] should be Snooze field types.
A @scheme[define-persistent-struct] with @italic{n} fields defines the 
following identifiers:

@itemize{
  @item{@intmore{era} @schemeidfont{entity:}@scheme[id], an @italic{entity
    metadata} value that can be used in queries and other Snooze 
    procedures.}
  @item{@extmore["http://pre.plt-scheme.org/docs/html/reference/structures.html#(part%20define-struct)"]{
    Structure types in the PLT Reference} @schemeidfont{struct:}@scheme[id],
    the usual @italic{structure type descriptor} provided by 
    @scheme[define-struct].}
  @item{@schemeidfont{make-}@scheme[id], a @italic{constructor} 
    procedure that takes @italic{n} arguments and returns a new struct.}
  @item{@schemeidfont{make-}@scheme[id]@schemeidfont{/defaults}, a
    @italic{keyword constructor} that takes up to @italic{n} keyword 
    arguments and returns a new struct. The keywords supplied should
    match the @scheme[_field-ids]: where a keyword is omitted, the default
    value of the corresponding field type is used.}
  @item{@scheme[id]@schemeidfont{?}, a @italic{predicate} that returns
    @scheme[#t] for instances of the structure type and @scheme[#f]
    for any other value.}
  @item{@scheme[id]@schemeidfont{-id}, an @italic{id accessor} procedure
    that takes a structure and returns the integer primary key from the 
    correponding database record, or @scheme[#f] if the structure has not
    been saved to the database.}
  @item{@scheme[id]@schemeidfont{-revision}, a @italic{revision accessor}
    procedure that takes a structure and returns the revision number of
    the corresponding database record when the structure was last loaded
    or saved, or @scheme[#f] if there is no corresponding record.}
  @item{@scheme[id]@schemeidfont{-}@scheme[field-id] for each 
    @scheme[field]; an @italic{accessor} procedure that takes an instance
    of the structure type and returns the value for the corresponding
    field.}
  @item{@schemeidfont{set-}@scheme[id]@schemeidfont{-}@scheme[field-id]@schemeidfont{!}
    for each @scheme[field]; a @italic{mutator} procedure that takes an
    instance of the structure type and a new field value. The structure
    is destructively updated with the new value, and @schemeidfont{#<void>}
    is returned.}
  @item{@extmore["http://pre.plt-scheme.org/docs/html/reference/structures.html#(part%20structinfo)"]{
    Structure type transformer bindings in the PLT Reference} @scheme[id],
    the transformer binding provided by @scheme[define-struct], used with 
    @scheme[shared] and @scheme[match]. See the notes on @scheme[match] 
    and subtyping below.}}

} @; end of defform for first version of define-persistent-struct

Persistent structures are completely inspectable. @schemeidfont{ids} and 
@schemeidfont{revisions} are visible in their printed forms. However, 
@schemeidfont{id} and @schemeidfont{revision} fields are not exposed to 
the @scheme[struct] from @file{plt-match.ss}. For example:

@interaction[
  (code:comment "person : (persistent-struct (U string #f) (U integer #f))")
  (define-persistent-struct person
    ([name type:text] [age type:integer]))
  (code:comment "dave : person")
  (define dave
    (make-person "Dave" 30))
  (code:comment "Print dave: id and revision shown:")
  dave
  (code:comment "Match against dave: no id and revision:")
  (match dave
    [(struct person (name age))
     (list name age)])]

Persistent structure types are not meant to be subtyped, although this
feature is planned for a future version of Snooze.

@defform[
  (define-persistent-struct id
    ([field-id type] ...+)
    ([pipeline-id pipeline] ...+))]{

@intmore{save-pipeline}
As the first version of @scheme[define-persistent-struct], but allows
the programmer to specify @italic{save}, @italic{insert}, @italic{update}
and @italic{delete} pipelines for the structure type. These pipelines
can also be specified later with the @scheme[set-entity-save-pipeline!]
and related procedures.

} @; end of defform for second version of define-persistent-struct

@defform[
  (define-persistent-struct id
    ([field-id type] ...+)
    ([pipeline-id pipeline] ...+)
    ([property value] ...+))]{

@extmore["http://pre.plt-scheme.org/docs/html/reference/structures.html#(part%20structprops)"]{
  Structure type properties in the PLT Scheme Reference}
As the second version of @scheme[define-persistent-struct], but allows
the programmer to specify the values of @italic{structure type properties}
to attach to the structure type.

} @; end of defform for second version of define-persistent-struct

@section{Creating structure types}

TODO
