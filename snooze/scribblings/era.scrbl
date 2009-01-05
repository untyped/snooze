#lang scribble/doc

@(require "base.ss")
          
@(define snooze-eval (make-snooze-eval))
          
@title[#:tag "era"]{Entities, relationships and attributes}

@(declare-exporting (planet untyped/snooze/snooze))

Snooze attaches @italic{ERA metadata} to persistent struct types, providing information on how to serialize and deserialize structures and create aliases for use in queries:

@itemize{
  @item{each persistent struct type has an associated @scheme[entity] that provides structural information on structures of that type;}
  @item{each @scheme[entity] contains, among other things, a list of @scheme[attributes] that provides information on the fields of the struct type;}
  @item{support for @scheme[relationships] between entities is planned for a future version of Snooze.}}

@section{Entities}

@defstruct[entity ([name            symbol?]
                   [table-name      symbol?]
                   [struct-type     struct-type?]
                   [constructor     (any ... -> persistent-struct?)]
                   [predicate       (any -> boolean?)]
                   [attributes      (listof attribute?)]
                   [save-pipeline   (listof procedure?)]
                   [insert-pipeline (listof procedure?)]
                   [update-pipeline (listof procedure?)]
                   [delete-pipeline (listof procedure?)])]{
Metadata describing a particular @scheme[struct-type]. Pipeline procedures are of type:

@schemeblock[
  (connection? persistent-struct? -> persistent-struct?)]

While the entity itself is useful to application programmers in a number of circumstances, most of the information inside the entity does not need to be accessed form application code. @scheme[define-persistent-struct] automatically binds an identifier of the form @schemeidfont{entity:foo} to the entity for each persistent struct type:

@interaction[
  #:eval snooze-eval
  (define-persistent-struct person
    ([name type:string]
     [age  type:integer]))
  entity:person]}

@defthing[prop:entity property?]{
A structure type property used to attach @scheme[entity] metadata to persistent structure types.}

@defproc[(struct-has-entity? [struct any]) boolean?]{
Returns @scheme[#t] if @scheme[struct] is a structure or structure type that has a value for @scheme[prop:entity].

@examples[
  #:eval snooze-eval
  (struct-has-entity? struct:person)
  (struct-has-entity? (make-person "Dave" 30))]}

@defproc[(struct-entity [struct (U struct? struct-type?)]) entity?]{
Returns the value of @scheme[prop:entity] that is associated with @scheme[struct].

@examples[
  #:eval snooze-eval
  (struct-entity struct:person)
  (struct-entity (make-person "Dave" 30))]}

@section{Relationships}

Snooze does not currently have explicit support for relationships. These are planned for a future release. For now, the recommended way of creating a relationship between two structures is by using a foreign key field of @scheme[type:integer]:

@schemeblock[
  (define-persistent-struct person
    ([name type:string]))
  (define-persistent-struct pet
    ([name     type:string]
     [owner-id type:integer]))]

A caveat to this approach is that you have to make sure the target structure is saved before you reference its ID for the foreign key:

@schemeblock[
  (code:comment "This is incorrect.")
  (code:comment "The person will not have an ID because it has not been saved:")
  (make-pet "Garfield" (person-id (make-person "Jon")))
  (code:comment "This is correct.")
  (code:comment "The call to save! allocates an ID for the person:")
  (make-pet "Garfield" (person-id (save! (make-person "Jon"))))]

@section{Attributes}

@defstruct[attribute ([name        symbol?]
                      [column-name symbol?]
                      [entity      entity?]
                      [index       natural?]
                      [accessor    (persistent-struct? -> any)]
                      [mutator     (persistent-struct? any -> void?)]
                      [type        type?])]{
Metadata describing a particular attribute (or field or column) of an entity.

@scheme[define-persistent-struct] automatically binds an identifier of the form @schemeidfont{attr:foo-bar} for each attribute of each persistent struct type:

@interaction[
  #:eval snooze-eval
  attr:person-id
  attr:person-revision
  attr:person-name
  attr:person-age]}

@defproc[(entity-has-attribute? [entity entity?] 
                                [attribute (U attribute? symbol?)]) boolean?]{
Returns @scheme[#t] if @scheme[entity] has the supplied @scheme[attribute], @scheme[#f] otherwise. @scheme[attribute] can be an attribute structure or an attribute name.

@examples[
  #:eval snooze-eval
  (entity-has-attribute? entity:person attr:person-name)
  (entity-has-attribute? entity:person 'name)]}

@defproc[(entity-attribute [entity entity?] [name (U attribute? symbol?)]) attribute?]{
Determines if @scheme[entity] has the supplied @scheme[attribute]. Returns the attribute exists; raises @scheme[exn:fail] otherwise.

@examples[
  #:eval snooze-eval
  (entity-attribute entity:person attr:person-name)
  (entity-attribute entity:person 'name)
  (entity-attribute entity:person 'nom)]}

@section{Attribute types}

Each attribute has an associated @italic{type} that determines the type of column used in the database. Types come in several flavours, described below. Note that the reflection of a type may be different in different DBMS types. For example, SQLite does not support the SQL @tt{TIMESTAMP} data type, so Snooze uses integers to serialize time values.

@defstruct[type
  ([allows-null? boolean?]
   [default      any])]

@defstruct[(boolean-type type) ()]{
Stores @scheme[#t] and @scheme[#f] values. There is no direct Scheme representation of @tt{NULL}; @tt{NULL} values in the database are mapped to @scheme[#f] in Scheme.}

@defstruct[(integer-type type) ()]{
Stores integer values. @tt{NULL} values in the database are mapped to @scheme[#f] in Scheme.}

@defstruct[(real-type type) ()]{
Stores real number values. @tt{NULL} values in the database are mapped to @scheme[#f] in Scheme.}

@defstruct[(string-type type)
  ([max-length integer?])]{
Stores string values as @tt{VARCHARs} or arbitrary length @tt{TEXTs}. The value of @scheme[max-length] determines the SQL type. @tt{NULL} values in the database are mapped to @scheme[#f] in Scheme.}

@defstruct[(symbol-type type)
  ([max-length integer?])]{
Like @scheme[string-type] but for symbol values.}

@defstruct[(time-utc-type type)
  ([max-length integer?])]{
Stores SRFI 19 UTC times as GMT @tt{TIMESTAMP WITHOUT TIME ZONEs} (or @tt{INTEGERs} in SQLite). @tt{NULL} values in the database are mapped to @scheme[#f] in Scheme.}

@defstruct[(time-tai-type type)
  ([max-length integer?])]{
Like @scheme[time-utc-type] but for SRFI 19 TAI times.}

@section{Shorthand types}

Snooze provides a number of short-hand types. The types below all allow and default to @tt{NULL}, and @scheme[type:string] and @scheme[type:symbol] allow data of arbitrary length:

@defthing[type:boolean type?]
@defthing[type:integer type?]
@defthing[type:real type?]
@defthing[type:string type?]
@defthing[type:symbol type?]
@defthing[type:time-utc type?]
@defthing[type:time-tai type?]

@include-section{persistent-struct-util.scrbl}
