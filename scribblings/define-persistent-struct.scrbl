#lang scribble/doc

@(require "base.ss")

@(define snooze-eval (make-snooze-eval))
          
@title[#:tag "define-persistent-struct"]{Defining persistent structure types}

@defform/subs[
  (define-persistent-struct id (attr-defn ...+) entity-option ...)
    ([attr-defn     [attr-id type attribute-option ...]]
     [attr-option   (code:line #:column-name symbol?)]
     [entity-option (code:line #:table-name symbol?)
                    (code:line #:on-save (listof (connection? persistent-struct? -> persistent-struct?)))
                    (code:line #:on-insert (listof (connection? persistent-struct? -> persistent-struct?)))
                    (code:line #:on-update (listof (connection? persistent-struct? -> persistent-struct?)))
                    (code:line #:on-delete (listof (connection? persistent-struct? -> persistent-struct?)))
                    (code:line #:property struct-type-property? any)])]{

Creates a new persistent structure, and binds variables related to the new structure type. A @scheme[define-persistent-struct] with @italic{n} attributes defines the following identifiers:

@itemize{
  @item{@intmore{era} @scheme[id], an @italic{entity metadata} value that can be used in queries and other Snooze procedures, doubles as a transformer binding that can be used with PLT forms such as @scheme[shared] and @scheme[match] ;}
  @item{@extmore["http://pre.plt-scheme.org/docs/html/reference/structures.html#(part%20define-struct)"]{
    Structure types in the PLT Reference} @schemeidfont{struct:}@scheme[id], the usual @italic{structure type descriptor} defined by @scheme[define-struct];}
  @item{@schemeidfont{make-}@scheme[id], a @italic{constructor} procedure that takes @italic{n} arguments and returns a new struct;}
  @item{@schemeidfont{make-}@scheme[id]@schemeidfont{/defaults}, a @italic{keyword constructor} that takes up to @italic{n+2} keyword arguments and returns a new struct. The keywords accepted include @scheme[#:id], @scheme[#:revision] and the various @scheme[attr-id]@schemeidfont{s}: where a keyword is omitted, the default value of the attribute is used;}
  @item{@schemeidfont{copy-}@scheme[id], a @italic{copy constructor} that takes a persistent structure and up to @italic{n+2} keyword arguments and returns a new struct. The keywords accepted include @scheme[#:id], @scheme[#:revision] and the various @scheme[attr-id]@schemeidfont{s}: where a keyword is omitted, the value of the attribute in the existing structure is used;}
  @item{@scheme[id]@schemeidfont{?}, a @italic{predicate} that returns @scheme[#t] for instances of the structure type and @scheme[#f] for any other value;}
  @item{@scheme[id]@schemeidfont{-}@scheme[attr-id], a set of @italic{accessor} procedures, including @scheme[id]@schemeidfont{-id} and @scheme[id]@schemeidfont{-revision}, that take a persistent structure as an argument and return the value of the corresponding attribute;}
  @item{@schemeidfont{set-}@scheme[id]@schemeidfont{-}@scheme[attr-id]@schemeidfont{!}, a set of @italic{mutator} procedures, including @schemeidfont{set-}@scheme[id]@schemeidfont{-id!} and @schemeidfont{set-}@scheme[id]@schemeidfont{-revision!}, that take a persistent structure and an arbitrary Scheme value as arguments, and mutate the structure to set the corresponding attribute to the supplied value;}
  @item{@extmore["http://pre.plt-scheme.org/docs/html/reference/structures.html#(part%20structinfo)"]{
    Structure type transformer bindings in the PLT Reference} @scheme[id], the transformer binding provided by @scheme[define-struct], used with PLT forms such as @scheme[shared] and @scheme[match];}
  @item{@schemeidfont{entity:}@scheme[id], an alias for @scheme[id], is provided for backwards compatibility only (replaced by @scheme[id]);}
  @item{@schemeidfont{attr:}@scheme[id]@schemeidfont{-}@scheme[attr-id], a set of @italic{attribute metadata} values, are provided for backwards compatibility only (replaced by the @scheme[attr] macro);}}

By default, persistent structures are stored in rows in a database table of the same name. The @scheme[#:table-name] entity option allows you to override the default table name and provide something more user-friendly: hyphen characters in table names must be escaped in most DBMSs. Similarly, the @scheme[#:column-name] attribute option allows you to override the default column name for each attribute. The column names for @schemeidfont{id} and @schemeidfont{revision} may not be changed.

@intmore{pipelines} The @scheme[#:on-save], @scheme[#:on-insert], @scheme[#:on-update] and @scheme[#:on-delete] keywords allow you to specify @italic{database pipelines} to be run during calls to @scheme[save!] and @scheme[delete!].

} @; end of defform for first version of define-persistent-struct

Persistent structures are completely inspectable. @schemeidfont{ids} and @schemeidfont{revisions} are visible in their printed forms and are exposed by the @scheme[struct] form from @scheme[scheme/match]. For example:

@interaction[
  #:eval snooze-eval
  (code:comment "person : (persistent-struct (U string #f) (U integer #f))")
  (define-persistent-struct person
    ([name type:string]
     [age  type:integer]))
  (code:comment "dave : person")
  (define dave
    (make-person "Dave" 30))
  (code:comment "Print dave: id and revision shown:")
  dave
  (code:comment "Match against dave: no id and revision:")
  (match dave
    [(struct person (id revision name age))
     (list id revision name age)])]

Persistent structure types are not meant to be subtyped, although this feature is planned for a future version of Snooze.
