#lang scheme/base

(require "../base.ss")

(require (for-syntax scheme/base)
         "core-snooze-interface.ss")

; Snooze objects ---------------------------------

(define current-snooze (make-parameter #f))

; Guids ------------------------------------------

; (struct snooze-cache<%> integer)
(define-serializable-struct guid
  (snooze [id #:mutable])
  #:transparent
  #:property
  prop:custom-write
  (lambda (guid out write?)
    (define show (if write? write display))
    (display "#(guid " out)
    (show (guid-id guid) out)
    (display ")" out)))

(define (create-guid #:snooze [snooze (current-snooze)] id)
  (make-guid snooze id))

; property
; guid -> boolean
; guid -> entity
(define-values (prop:guid-entity-box guid-entity? guid-entity-box)
  (make-struct-type-property 'guid-entity-box))

; (_ id entity)
(define-syntax (define-guid-type stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([print-prefix (format "#(~a " (syntax->datum #'id))])
       #'(define-serializable-struct (id guid)
           ()
           #:transparent
           #:property
           prop:guid-entity-box
           (box #f)
           #:property
           prop:custom-write
           (lambda (guid out write?)
             (define show (if write? write display))
             (display print-prefix out)
             (show (guid-id guid) out)
             (let ([struct (guid-cache-ref guid)])
               (when struct
                 (for ([val (in-vector (struct->vector struct) 2)])
                   (display " " out)
                   (show val out))))
             (display ")" out))
           #:property
           prop:equal+hash
           (list (lambda (guid1 guid2 same?)
                   (and (same? (guid-id guid1)
                               (guid-id guid2))
                        (same? (send (guid-snooze guid1) cache-ref guid1)
                               (send (guid-snooze guid2) cache-ref guid2))))
                 (lambda (guid recur)
                   (recur guid))
                 (lambda (guid recur)
                   (recur guid)))))]))

; guid -> entity
(define (guid-entity guid)
  (unbox (guid-entity-box guid)))

; guid -> snooze-struct
(define (guid-cache-ref guid)
  (send (guid-snooze guid) cache-ref guid))

; [(U natural #f)] -> guid
(define (guid-copy guid [id (guid-id guid)] #:snooze [snooze (guid-snooze guid)])
  (entity-make-guid #:snooze snooze (guid-entity guid) id))

; Attribute types --------------------------------

; (struct boolean any)
(define-serializable-struct type (allows-null? default-maker) #:transparent)

; (struct boolean any)
(define-serializable-struct (boolean-type type) ()  #:transparent)

; (struct boolean any)
(define-serializable-struct (integer-type type) () #:transparent)

; (struct boolean any)
(define-serializable-struct (real-type type) () #:transparent)

; (struct boolean any (U integer #f))
(define-serializable-struct (string-type type) (max-length) #:transparent)

; (struct boolean any (U integer #f))
(define-serializable-struct (symbol-type type) (max-length) #:transparent)

; (struct boolean any)
(define-serializable-struct (time-utc-type type) () #:transparent)

; (struct boolean any)
(define-serializable-struct (time-tai-type type) () #:transparent)

; (struct boolean any entity)
(define-serializable-struct (guid-type type) (entity) #:transparent)

; type -> any
(define (type-default type)
  ((type-default-maker type)))

; type -> any
(define (type-null type)
  (if (boolean-type? type)
      (void)
      #f))

; type any -> boolean
(define (type-valid? type value)
  (cond [(equal? value (type-null type)) (type-allows-null? type)]
        [(boolean-type? type)            (boolean? value)]
        [(integer-type? type)            (integer? value)]
        [(real-type? type)               (real? value)]
        [(string-type? type)             (let ([max-length (string-type-max-length type)])
                                           (and (string? value)
                                                (or (not max-length)
                                                    (<= (string-length value) max-length))))]
        [(symbol-type? type)             (let ([max-length (string-type-max-length type)])
                                           (and (symbol? value)
                                                (or (not max-length)
                                                    (<= (string-length (symbol->string value)) max-length))))]
        [(time-tai-type? type)           (time-tai? value)]
        [(time-utc-type? type)           (time-utc? value)]))

; type -> symbol
(define (type-name type)
  (cond [(boolean-type? type)  'boolean]
        [(integer-type? type)  'integer]
        [(real-type? type)     'real]
        [(string-type? type)   'string]
        [(symbol-type? type)   'symbol]
        [(time-utc-type? type) 'time-utc]
        [(time-tai-type? type) 'time-tai]))

; type type -> boolean
;
; Returns #t if the arguments are of the same class of type (boolean types, integer types, etc).
(define (type-compatible? type1 type2)
  (or (and (boolean-type? type1)  (boolean-type? type2))
      (and (integer-type? type1)  (integer-type? type2))
      (and (real-type? type1)     (real-type? type2))
      (and (string-type? type1)   (string-type? type2))
      (and (symbol-type? type1)   (symbol-type? type2))
      (and (time-utc-type? type1) (time-utc-type? type2))
      (and (time-tai-type? type1) (time-tai-type? type2))
      (and (guid-type? type1)
           (guid-type? type2)
           (eq? (guid-type-entity type1)
                (guid-type-entity type2)))))

; type
(define type:boolean  (make-boolean-type  #t (lambda () #f)))
(define type:integer  (make-integer-type  #t (lambda () #f)))
(define type:real     (make-real-type     #t (lambda () #f)))
(define type:string   (make-string-type   #t (lambda () #f) #f))
(define type:symbol   (make-symbol-type   #t (lambda () #f) #f))
(define type:time-tai (make-time-tai-type #t (lambda () #f)))
(define type:time-utc (make-time-utc-type #t (lambda () #f)))

; Entities ---------------------------------------

; (struct symbol 
;         symbol
;         struct-type
;         (any ... -> snooze-struct)
;         ((U snooze-struct any) -> boolean)
;         (snooze-struct natural -> any)
;         (snooze-struct natural any -> void)
;         (any ... -> guid)
;         ((U guid any) -> boolean)
;         ((U natural #f) -> guid)
;         ((U guid any) -> boolean)
;         (listof attribute)
;         ((struct -> struct) struct -> struct)
;         ((struct -> struct) struct -> struct))
(define-struct entity 
  (name
   table-name
   struct-type
   private-constructor
   private-predicate
   private-accessor
   private-mutator
   cached-constructor
   cached-predicate
   guid-constructor
   guid-predicate
   attributes
   on-save
   on-delete)
  #:transparent
  #:mutable
  #:property 
  prop:custom-write
  (lambda (entity port write?)
    (fprintf port "#<entity:~a>" (entity-name entity))))

;  symbol
;  symbol
;  ((U natural #f) -> guid)
;  ((U guid any) -> boolean)
;  ((struct -> struct) struct -> struct)
;  ((struct -> struct) struct -> struct)
; ->
;  void
;
; Entities have mutual dependencies with the struct types they are attached to.
; We can't mutate struct types, so we have to create them first and then patch
; the entity. This procedure gets around the field contracts on entity during
; the creation process.
(define (make-vanilla-entity name table-name guid-constructor guid-predicate on-save on-delete)
  (make-entity name table-name #f ; struct type
               #f #f #f #f #f #f  ; constructors and predicates
               guid-constructor   ; guid constructor
               guid-predicate     ; guid predicate
               null               ; attributes
               on-save            ; hooks
               on-delete))        ;

; entity [#:snooze snooze] [(U natural #f)] -> guid
(define (entity-make-guid entity #:snooze [snooze (current-snooze)] [id #f])
  ((entity-guid-constructor entity) snooze id))

; entity any -> boolean
(define (entity-guid? entity guid)
  ((entity-guid-predicate entity) guid))

; entity (U symbol attribute) -> boolean
(define (entity-has-attribute? entity name+attr)
  (if (attribute? name+attr)
      (eq? (attribute-entity name+attr) entity)
      (ormap (lambda (attr)
               (eq? (attribute-name attr) name+attr))
             (entity-attributes entity))))

; entity (U symbol attribute) -> boolean
(define (entity-guid-attribute? entity name+attr)
  (or (eq? name+attr (car (entity-attributes entity)))
      (eq? name+attr 'guid)))

; entity (U symbol attribute) -> attribute
(define (entity-attribute entity name+attr)
  (or (if (attribute? name+attr)
          (and (eq? (attribute-entity name+attr) entity)
               name+attr)
          (ormap (lambda (attr)
                   (and (eq? (attribute-name attr) name+attr)
                        attr))
                 (entity-attributes entity)))
      (raise-exn exn:fail:contract
        (format "Attribute not found: ~s ~s" entity name+attr))))

; Relationships ----------------------------------

; Watch this space...

; Attributes -------------------------------------

; (struct symbol
;         symbol
;         type
;         entity
;         integer
;         (snooze-struct -> any)
;         (snooze-struct any -> void)
;         (guid -> any)
;         (guid any -> void))
(define-struct attribute 
  (name
   column-name
   type
   entity
   index
   private-accessor
   private-mutator
   cached-accessor
   cached-mutator)
  #:transparent
  #:property
  prop:custom-write
  (lambda (attribute port write?)
    (fprintf port "#<attr:~a-~a>" 
             (entity-name (attribute-entity attribute))
             (attribute-name attribute))))

; Snooze structs ---------------------------------

; property
; any -> boolean
; any -> (U entity #f)
(define-values (prop:entity prop:entity-set? prop:entity-ref)
  (make-struct-type-property 'entity))

; any -> boolean
; Used in this module only... the public procedure of the same name
; recognises guids rather than structs.
(define (snooze-struct? struct)
  (and (struct? struct)
       (prop:entity-set? struct)))

; Provide statements -----------------------------

(provide (all-from-out "core-snooze-interface.ss")
         define-guid-type
         guid
         struct:guid)

(provide/contract
 [current-snooze                  (parameter/c (is-a?/c snooze-cache<%>))]
 [rename create-guid make-guid    (->* ((or/c natural-number/c #f))
                                       (#:snooze (is-a?/c snooze-cache<%>))
                                       guid?)]
 [guid?                           procedure?]
 [guid-snooze                     (-> guid? (is-a?/c snooze-cache<%>))]
 [guid-id                         (-> guid? (or/c natural-number/c #f))]
 [set-guid-id!                    (-> guid? (or/c natural-number/c #f) void?)]
 [guid-entity-box                 (-> struct-type? box?)]
 [guid-entity                     (-> guid? entity?)]
 [guid-cache-ref                  (-> guid? snooze-struct?)]
 [guid-copy                       (->* (guid?)
                                       ((or/c natural-number/c #f)
                                        #:snooze (is-a?/c snooze-cache<%>))
                                       guid?)]
 [struct type                     ([allows-null? boolean?] [default-maker (-> any)])]
 [struct (guid-type type)         ([allows-null? boolean?] [default-maker (-> (or/c guid?     #f))] [entity entity?])]
 [struct (boolean-type type)      ([allows-null? boolean?] [default-maker (-> (or/c boolean?  #f))])]
 [struct (integer-type type)      ([allows-null? boolean?] [default-maker (-> (or/c integer?  #f))])]
 [struct (real-type type)         ([allows-null? boolean?] [default-maker (-> (or/c real?     #f))])]
 [struct (string-type type)       ([allows-null? boolean?] [default-maker (-> (or/c string?   #f))] [max-length (or/c natural-number/c #f)])]
 [struct (symbol-type type)       ([allows-null? boolean?] [default-maker (-> (or/c symbol?   #f))] [max-length (or/c natural-number/c #f)])]
 [struct (time-utc-type type)     ([allows-null? boolean?] [default-maker (-> (or/c time-utc? #f))])]
 [struct (time-tai-type type)     ([allows-null? boolean?] [default-maker (-> (or/c time-tai? #f))])]
 [type-null                       (-> type? any)]
 [type-default                    (-> type? any)]
 [type-valid?                     (-> type? any/c boolean?)]
 [type-name                       (-> type? symbol?)]
 [type-compatible?                (-> type? type? boolean?)]
 [type:boolean                    boolean-type?]
 [type:integer                    integer-type?]
 [type:real                       real-type?]
 [type:string                     string-type?]
 [type:symbol                     symbol-type?]
 [type:time-tai                   time-tai-type?]
 [type:time-utc                   time-utc-type?]
 [struct entity                   ([name                symbol?]
                                   [table-name          symbol?]
                                   [struct-type         struct-type?]
                                   [private-constructor procedure?]
                                   [private-predicate   procedure?]
                                   [private-accessor    procedure?]
                                   [private-mutator     procedure?]
                                   [cached-constructor  procedure?]
                                   [cached-predicate    procedure?]
                                   [guid-constructor    (-> (is-a?/c snooze-cache<%>)
                                                            (or/c natural-number/c #f)
                                                            guid?)]
                                   [guid-predicate      (-> any/c boolean?)]
                                   [attributes          (listof attribute?)]
                                   [on-save             (-> (-> snooze-struct? any) snooze-struct? snooze-struct?)]
                                   [on-delete           (-> (-> snooze-struct? any) snooze-struct? snooze-struct?)])]
 [make-vanilla-entity             (-> symbol?
                                      symbol?
                                      procedure?
                                      procedure?
                                      procedure?
                                      procedure?
                                      entity?)]
 [entity-make-guid                (->* (entity?)
                                       (#:snooze (is-a?/c snooze-cache<%>)
                                                 (or/c natural-number/c #f)) guid?)]
 [entity-guid?                    (-> entity? any/c boolean?)]
 [entity-has-attribute?           (-> entity? (or/c symbol? attribute?) boolean?)]
 [entity-guid-attribute?          (-> entity? (or/c symbol? attribute?) boolean?)]
 [entity-attribute                (-> entity? (or/c symbol? attribute?) attribute?)]
 [struct attribute                ([name                symbol?]
                                   [column-name         symbol?]
                                   [type                type?]
                                   [entity              entity?]
                                   [index               integer?]
                                   [private-accessor    procedure?]
                                   [private-mutator     procedure?]
                                   [cached-accessor     procedure?]
                                   [cached-mutator      procedure?])]
 [prop:entity                     struct-type-property?]
 [prop:entity-set?                (-> any/c boolean?)]
 [prop:entity-ref                 (-> any/c entity?)])
