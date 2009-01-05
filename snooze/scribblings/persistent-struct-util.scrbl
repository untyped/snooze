#lang scribble/doc

@(require (file "base.ss"))

@(define snooze-eval (make-snooze-eval))
          
@title[#:tag "persistent-struct-util"]{Persistent structure utilities}

@defproc[(struct-attributes
          [struct persistent-struct?]) list?]{
Returns a list of the values of the attributes of @scheme[struct] (including its ID and revision).

@examples[
  #:eval snooze-eval
  (define-persistent-struct person
    ([name type:string] [age type:integer]))
  (define dave (make-person "Dave" 30))
  (struct-attributes dave)]}

@defproc[(struct-has-attribute?
          [struct persistent-struct?]
          [name (U attribute? symbol?)]) boolean?]{
Returns @scheme[#t] if @scheme[struct] has an attribute with the supplied @scheme[name], @scheme[#f] otherwise.

@examples[
  #:eval snooze-eval
  (struct-has-attribute? dave attr:person-name)
  (struct-has-attribute? dave 'age)
  (struct-has-attribute? dave 'nom)]}

@defproc[(struct-attribute
          [struct persistent-struct?]
          [name (U attribute? symbol?)]) any]{
Returns the value from @scheme[struct] of the attribute with the supplied @scheme[name]. Raises @scheme[exn:fail:contract] if @scheme[struct] does not have a corresponding attribute.

@examples[
  #:eval snooze-eval
  (struct-attribute dave attr:person-name)
  (struct-attribute dave 'age)
  (struct-attribute dave 'nom)]}

@defproc[(set-struct-attribute!
          [struct persistent-struct?]
          [name (U attribute? symbol?)]
          [value any]) void?]{
Mutates @scheme[struct], setting the @scheme[value] of the attribute with the supplied @scheme[name].

@examples[
  #:eval snooze-eval
  (set-struct-attribute! dave attr:person-name "Noel")
  (set-struct-attribute! dave 'age #f)
  (struct-attributes dave)]}

@defproc[(set-struct-attributes!
          [struct persistent-struct?]
          [vals list?]) void?]{
Mutates @scheme[struct], setting the values of all its attributes. @scheme[vals] must contain an ID, a revision, and one value for each attribute of @scheme[struct] in the order specified inthe @scheme[define-persistent-struct] form.

@examples[
  #:eval snooze-eval
  (set-struct-attributes! dave (list #f #f "Dave" 30))
  (struct-attributes dave)]}

@defproc[(make-persistent-struct/defaults
          [entity entity?]
          [attr (U attribute? symbol?)]
          [value any]
          [attr2 (U attribute? symbol?)]
          [value2 any]
          ...) persistent-struct?]{
Returns a new persistent of the supplied type, with default values for all attributes except those specified in the arguments. Used in the implementation of the keyword constructor created by @scheme[define-persistent-struct].

Each @scheme[attr] must be followed by a corresponding @scheme[value].

@examples[
  #:eval snooze-eval
  (make-persistent-struct/defaults
   entity:person attr:person-name "Noel")]}

@defproc[(copy-persistent-struct
          [struct persistent-struct?]
          [attr (U attribute? symbol?)]
          [value any]
          [attr2 (U attribute? symbol?)]
          [value2 any]
          ...) persistent-struct?]{
Returns a new persistent structure with the same type and attributes of @scheme[struct], except where new values are supplied in the arguments. Used in the implentation of the copy procedure created by @scheme[define-persistent-struct].

Each @scheme[attr] must be followed by a corresponding @scheme[value].}

@defproc[(update-persistent-struct-from-copy!
          [struct persistent-struct?]
          [copy persistent-struct?]) persistent-struct?]{
Mutates @scheme[struct] to be the same as @scheme[copy]. Used when rolling back cancelled transactions.

@examples[
  #:eval snooze-eval
  (copy-persistent-struct dave attr:person-name "Noel")]}
