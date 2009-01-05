(module type mzscheme
  
  (require (lib "contract.ss")
           (lib "time.ss" "srfi" "19"))
  
  (require (file "base.ss"))
  
  ; A field type is a structure:
  ; 
  ;     (struct name (U base #f) null-proc initial-proc)
  ;
  ; where:
  ;   - name is a symbol used to make types more identifiable in printed output;
  ;   - base is one of a set of core types from which the type is derived (or #f if
  ;     the type is itself a base type);
  ;   - null-proc is a procedure that takes a type and returns a Scheme value for NULL (usually #f);
  ;   - initial-proc is a procedure that takes a type and returns an initial Scheme value, OR
  ;     throws exn:fail to indicate that no value has been specified.
  ;
  ; It is the base type that determines how a type is serialized, parsed, and referred to
  ; in SQL. If the set of base types is altered, the rest of Snooze must be updated to
  ; handle the changes.
  ;
  ; Note: base types have their base field set to #f... this avoids problems using
  ; equal? on two types. The type->base procedure is used to find the relevant base type
  ; for a given type. To add to the confusion, this procedure is provided as type-base,
  ; making it look like base types have themselves in their base fields.
  
  ;; struct type : symbol (U type #f) (type -> any) (type -> any)
  ;;
  ;; The null? and default fields contain proc
  (define-struct type (name base null-proc initial-proc) #f)
  
  ; Constructors ---------------------------------
  
  ;; type->base : type -> type
  (define (type->base type)
    (if (memq type base-types)
        type
        (type-base type)))
  
  ;; set-type-initial : type any -> type
  (define (set-type-initial type initial)
    (make-type (string->symbol (format "~a/~a" (type-name (type->base type)) initial))
               (type->base type)
               (type-null-proc type)
               (lambda (type) initial)))
  
  ;; set-type-initial-proc : type (-> any) -> type
  (define (set-type-initial-proc type thunk)
    (make-type (type->base type)
               (type-null-proc type)
               thunk))
  
  ; Accessors ------------------------------------
  
  ;; type-null : type -> any
  (define (type-null type)
    ((type-null-proc type) type))

  ;; type-initial : type -> any
  (define (type-initial type)
    ((type-initial-proc type) type))
  
  ; Nulls and defaults ---------------------------
  
  ;; default-null-proc : type -> any
  (define (default-null-proc type)
    #f)
  
  ;; default-initial-proc : type -> any
  (define (default-initial-proc type)
    (type-null type))
  
  ; Base types -----------------------------------
  
  ; These objects represent the types that can be specified for
  ; attributes of persistent structs:
  
  ;; base-types : (list-of type)
  (define base-types null)
  
  ;; base-type? : any -> boolean
  (define (base-type? type)
    (if (memq type base-types) 
        #t
        #f))
  
  ;; syntax define-base-type
  ;;
  ;; Convenience for defining a type and adding it to base-types.
  (define-syntax define-base-type
    (syntax-rules ()
      [(define-base-type name)
       (begin (define name (make-type 'name #f default-null-proc default-initial-proc))
              (set! base-types (append base-types (list name))))]))
  
  (define-base-type type:id)
  (define-base-type type:revision)
  (define-base-type type:text)
  (define-base-type type:integer)
  (define-base-type type:real)
  (define-base-type type:symbol)
  (define-base-type type:boolean)
  (define-base-type type:time-tai) ; as defined in SRFI 19

  ; Provide statements --------------------------- 
  
  (provide type? base-type?)
  
  (provide/contract
   [rename type->base type-base (-> type? base-type?)]
   [type-null                   (-> type? any)]
   [type-initial                (-> type? any)]
   [set-type-initial            (-> type? any/c type?)]
   [set-type-initial-proc       (-> type? procedure? type?)]
   [type:id                     type?]
   [type:revision               type?]
   [type:text                   type?]
   [type:integer                type?]
   [type:real                   type?]
   [type:symbol                 type?]
   [type:boolean                type?]
   [type:time-tai               type?])
  
  )
