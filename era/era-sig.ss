#lang scheme/signature

; persistent-struct ----------------------------

; entity
; struct-type-descriptor
; any ... -> persistent-struct
; any -> boolean
; persistent-struct integer -> any
; persistent-struct integer any -> void
; attribute
; attribute
;
; The entity and structure type for persistent-struct.
;
; persistent-struct is the supertype of all types defined with
; make-persistent-struct-type or define-persistent-struct.
;
; persistent-struct defines two attributes: "id" and "revision",
; both of type (U integer #f):
;
;   persistent-struct : (struct (U integer #f) (U integer #f)
entity:persistent-struct
struct:persistent-struct
make-persistent-struct
persistent-struct?
persistent-struct-ref
persistent-struct-set!
attr:struct-id
attr:struct-revision

; (struct integer -> any) integer symbol -> (persistent-struct -> any)
;
; Serves the same purpose as MzScheme's make-struct-field-accessor.
;
; Arguments are the same as make-struct-field-accessor.
;
; NOTE: This procedure is currently not strictly necessary. It is provided
; firstly for symmetry with make-persistent-struct-field-mutator, and secondly
; because it may become necessary in future updates to Snooze.
make-persistent-struct-field-accessor

; (struct integer any -> void) integer symbol -> (persistent-struct any -> void)
;
; Serves the same purpose as MzScheme's make-struct-field-mutator,
; but backs the struct up in the current transaction frame before mutating it.
;
; Arguments are the same as make-struct-field-mutator.
;
; The resulting accessor procedure unfortunately cannot be identified using
; MzScheme's struct-accessor-procedure? predicate.
make-persistent-struct-field-mutator

; persistent-struct -> (U integer #f)
; persistent-struct (U integer #f) -> void
; persistent-struct -> (U integer #f)
; persistent-struct (U integer #f) -> void
;
; The basic accessors and mutators for IDs and revisions.
struct-id
set-struct-id!
struct-revision
set-struct-revision!

; persistent-struct -> boolean
struct-saved?

; Struct utilities -----------------------------

; persistent-struct -> guid
struct-guid

; persistent-struct symbol -> any
;
; Determines whether the struct has an attribute of the supplied name.
struct-has-attribute?

; persistent-struct symbol -> any
;
; Retrieves the attribute with the supplied name from the struct.
; Raises exn:fail:snooze if the attribute is not found.
struct-attribute

; persistent-struct -> (listof any)
;
; Retrieves a list of the values of all attributes (including ID and revision)
; of the supplied struct.
;
; Values are returned in the same order as the attributes returned by
; (entity-attributes (struct-entity struct)).
struct-attributes

; persistent-struct symbol any -> void
;
; Mutates the struct, setting the relevant attribute to the supplied value.
set-struct-attribute!

; persistent-struct (vectorof any) -> void
;
; Mutates the struct, setting all its attributes (including ID and revision)
; to the supplied values. Values must be supplied in the same order as
; (entity-attributes (struct-entity struct)).
set-struct-attributes!

; entity [#:field-name any] ... -> persistent-struct
;
; Creates a persistent-struct with default values for all attributes except
; those supplied as keyword arguments.
make-persistent-struct/defaults

; persistent-struct [#:field-name any] ... -> persistent-struct
;
; Copies the persistent struct, creating a new struct with the same values 
; except those supplied as keyword arguments.
;
; This procedure is used to clone structs when they are first mutated inside
; a transaction, so they can be restored to their original state if the 
; transaction is rolled back.
copy-persistent-struct

; persistent-struct persistent-struct -> void
;
; Mutates the first struct, setting the values of all attributes (including
; ID and revision) to the values in the second struct.
;
; This procedure is used in transaction rollback.
update-persistent-struct-from-copy!
