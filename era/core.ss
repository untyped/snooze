#lang scheme/base

(require "../base.ss")

(require (for-syntax scheme/base)
         scheme/dict
         "../generic/connection.ss"
         "core-snooze-interface.ss")

; Snooze objects ---------------------------------

(define current-snooze (make-parameter #f))

; Guids ------------------------------------------

; (struct snooze-cache<%> integer)
(define-serializable-struct guid
  (id+serial snooze)
  #:transparent
  #:property
  prop:custom-write
  (lambda (guid out write?)
    (define show (if write? write display))
    (display "#(" out)
    (show (guid-id+serial guid) out)
    (display ")" out)))

; property
; guid -> boolean
; guid -> entity
(define-values (prop:guid-entity-box guid-entity? guid-entity-box)
  (make-struct-type-property 'guid-entity-box))

; guid -> guid
(define (copy-guid guid)
  ((entity-guid-constructor (guid-entity guid))
   (guid-id+serial guid)
   (guid-snooze guid)))

; guid -> (U integer #f)
(define (guid-id guid)
  (and (number? (guid-id+serial guid))
       (guid-id+serial guid)))

; guid -> (U symbol #f)
(define (guid-serial guid)
  (and (symbol? (guid-id+serial guid))
       (guid-id+serial guid)))

; guid guid -> boolean
(define (guid=? guid1 guid2)
  (and (eq? (guid-id+serial guid1)
            (guid-id+serial guid2))
       (eq? (entity-name (guid-entity guid1))
            (entity-name (guid-entity guid2)))
       (eq? (guid-snooze guid1)
            (guid-snooze guid2))))

; guid guid -> boolean
(define (guid=?-hash-code guid)
  (equal-hash-code
   (list (guid-id+serial guid)
         (entity-name (guid-entity guid))
         (guid-snooze guid))))

; guid -> boolean
(define (guid-local? guid)
  (and (guid-serial guid) #t))

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
             (let ([show   (if write? write display)]
                   [struct (and (not (in-cache-code?))
                                (with-handlers ([exn? (lambda (exn) (printf "exn fetching struct: ~s~n" (exn-message exn)))])
                                  (guid-ref guid)))])
               (parameterize ([in-cache-code? #t])
                 (display print-prefix out)
                 (show (guid-id+serial guid) out)
                 (when struct
                   (for ([val (in-vector (struct->vector struct) 2)])
                     (display " " out)
                     (show val out)))
                 (display ")" out))))
           #:property
           prop:equal+hash
           (list (lambda (guid1 guid2 same?)
                   (same? (guid-ref guid1)
                          (guid-ref guid2)))
                 (lambda (guid recur) (recur guid))
                 (lambda (guid recur) (recur guid)))))]))

; guid -> entity
(define (guid-entity guid)
  (unbox (guid-entity-box guid)))

; guid -> snooze-struct
(define (guid-ref guid)
  (parameterize ([in-cache-code? #t])
    (or (send (guid-snooze guid) cache-ref guid)
        (raise-exn exn:fail:snooze:cache
          (format "guid not cached: ~s" guid)))))

; Interning guids --------------------------------

; (weak-custom-hashof guid (weak-box guid))
(define interned-guids
  (make-weak-custom-hash guid=? guid=?-hash-code))

; guid -> boolean
(define (guid-interned? guid)
  (eq? (dict-ref interned-guids guid #f) guid))

; guid -> guid
(define (intern-guid guid)
  (weak-box-value
   (dict-ref interned-guids
             guid
             (lambda ()
               (let* ([guid (copy-guid guid)]
                      [box  (make-weak-box guid)])
                 (dict-set! interned-guids guid box)
                 box)))))

; Attribute types --------------------------------

; (struct boolean)
(define-serializable-struct type (allows-null?) #:transparent)

; (struct boolean)
(define-serializable-struct (boolean-type type) () #:transparent)

; (struct boolean)
(define-serializable-struct (numeric-type type) () #:transparent)
(define-serializable-struct (integer-type numeric-type) () #:transparent)
(define-serializable-struct (real-type    numeric-type) () #:transparent)

; (struct boolean (U natural #f))
(define-serializable-struct (character-type type) (max-length) #:transparent)
(define-serializable-struct (string-type character-type) () #:transparent)
(define-serializable-struct (symbol-type character-type) () #:transparent)

; (struct boolean)
(define-serializable-struct (temporal-type type)          () #:transparent)
(define-serializable-struct (time-utc-type temporal-type) () #:transparent)
(define-serializable-struct (time-tai-type temporal-type) () #:transparent)

; (struct boolean entity)
(define-serializable-struct (guid-type type) (entity) #:transparent)
(define-serializable-struct (primary-key-type guid-type) () #:transparent)
(define-serializable-struct (foreign-key-type guid-type) () #:transparent)

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
        [(string-type? type)             (let ([max-length (character-type-max-length type)])
                                           (and (string? value)
                                                (or (not max-length)
                                                    (<= (string-length value) max-length))))]
        [(symbol-type? type)             (let ([max-length (character-type-max-length type)])
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
        [(time-tai-type? type) 'time-tai]
        [(guid-type? type)     (entity-name (guid-type-entity type))]))

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
(define type:boolean  (make-boolean-type  #t))
(define type:integer  (make-integer-type  #t))
(define type:real     (make-real-type     #t))
(define type:string   (make-string-type   #t #f))
(define type:symbol   (make-symbol-type   #t #f))
(define type:time-tai (make-time-tai-type #t))
(define type:time-utc (make-time-utc-type #t))

; Entities ---------------------------------------

; (struct symbol 
;         symbol
;         string
;         string
;         (guid any ... -> string)
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
   pretty-name
   pretty-name-plural
   pretty-formatter
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
;  string
;  string
;  (guid any ... -> string)
;  ((U natural #f) -> guid)
;  ((U guid any) -> boolean)
;  ((struct -> struct) struct -> struct)
;  ((struct -> struct) struct -> struct)
; ->
;  entity
;
; Entities have mutual dependencies with the struct types they are attached to.
; We can't mutate struct types, so we have to create them first and then patch
; the entity. This procedure gets around the field contracts on entity during
; the creation process.
(define (make-vanilla-entity name table-name pretty-name pretty-name-plural pretty-formatter guid-constructor guid-predicate on-save on-delete)
  (make-entity name table-name pretty-name pretty-name-plural pretty-formatter
               #f                       ; struct type
               #f #f #f #f #f #f        ; constructors and predicates
               guid-constructor         ; guid constructor
               guid-predicate           ; guid predicate
               null                     ; attributes
               on-save                  ; hooks
               on-delete))              ;

; entity [#:snooze snooze] natural -> guid
(define (entity-make-vanilla-guid #:snooze [snooze (current-snooze)] entity id)
  ((entity-guid-constructor entity) id snooze))

; entity [#:snooze snooze] -> guid
(define (entity-make-local-guid #:snooze [snooze (current-snooze)] entity)
  ((entity-guid-constructor entity) (gensym (entity-name entity)) snooze))

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
;         string
;         string
;         type
;         entity
;         integer
;         (snooze<%> -> any)
;         (snooze-struct -> any)
;         (snooze-struct any -> void)
;         (guid -> any)
;         (guid any -> void))
(define-struct attribute 
  (name
   column-name
   pretty-name
   pretty-name-plural
   type
   entity
   index
   default-maker
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

; type -> any
(define (attribute-default attr #:snooze [snooze (current-snooze)])
  ((attribute-default-maker attr) snooze))

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

; Helpers ----------------------------------------

; guid -> void
(define (raise-cache-ref-error guid)
  (raise-exn exn:fail:snooze:cache
    (format "cache-ref: struct not found in cache: ~s" guid)))

; Provide statements -----------------------------

(provide (all-from-out "core-snooze-interface.ss")
         define-guid-type
         guid
         struct:guid)

(provide/contract
 [current-snooze                       (parameter/c (or/c (is-a?/c snooze-cache<%>) #f))]
 [guid?                                procedure?]
 [guid=?                               (-> guid? guid? boolean?)]
 [guid=?-hash-code                     (-> guid? number?)]
 [guid-local?                          (-> guid? boolean?)]
 [guid-snooze                          (-> guid? (is-a?/c snooze-cache<%>))]
 [copy-guid                            (-> guid? guid?)]
 [guid-id                              (-> guid? (or/c natural-number/c #f))]
 [guid-serial                          (-> guid? (or/c symbol? #f))]
 [guid-entity-box                      (-> struct-type? box?)]
 [guid-entity                          (-> guid? entity?)]
 [guid-ref                             (-> guid? snooze-struct?)]
 [guid-interned?                       (-> guid? boolean?)]
 [intern-guid                          (-> (and/c guid? (not/c guid-local?)) guid?)]
 [struct type                          ([allows-null? boolean?])]
 [struct (guid-type type)              ([allows-null? boolean?] [entity entity?])]
 [struct (boolean-type type)           ([allows-null? boolean?])]
 [struct (numeric-type type)           ([allows-null? boolean?])]
 [struct (integer-type numeric-type)   ([allows-null? boolean?])]
 [struct (real-type numeric-type)      ([allows-null? boolean?])]
 [struct (character-type type)         ([allows-null? boolean?] [max-length (or/c natural-number/c #f)])]
 [struct (string-type character-type)  ([allows-null? boolean?] [max-length (or/c natural-number/c #f)])]
 [struct (symbol-type character-type)  ([allows-null? boolean?] [max-length (or/c natural-number/c #f)])]
 [struct (temporal-type type)          ([allows-null? boolean?])]
 [struct (time-utc-type temporal-type) ([allows-null? boolean?])]
 [struct (time-tai-type temporal-type) ([allows-null? boolean?])]
 [type-null                            (-> type? any)]
 [type-valid?                          (-> type? any/c boolean?)]
 [type-name                            (-> type? symbol?)]
 [type-compatible?                     (-> type? type? boolean?)]
 [type:boolean                         boolean-type?]
 [type:integer                         integer-type?]
 [type:real                            real-type?]
 [type:string                          string-type?]
 [type:symbol                          symbol-type?]
 [type:time-tai                        time-tai-type?]
 [type:time-utc                        time-utc-type?]
 [struct entity                        ([name                symbol?]
                                        [table-name          symbol?]
                                        [pretty-name         string?]
                                        [pretty-name-plural  string?]
                                        [pretty-formatter    procedure?]
                                        [struct-type         struct-type?]
                                        [private-constructor procedure?]
                                        [private-predicate   procedure?]
                                        [private-accessor    procedure?]
                                        [private-mutator     procedure?]
                                        [cached-constructor  procedure?]
                                        [cached-predicate    procedure?]
                                        [guid-constructor    (-> (or/c natural-number/c #f)
                                                                 (or/c symbol? #f)
                                                                 (is-a?/c snooze-cache<%>)
                                                                 guid?)]
                                        [guid-predicate      (-> any/c boolean?)]
                                        [attributes          (listof attribute?)]
                                        [on-save             (-> (-> connection? guid? guid?) connection? guid? guid?)]
                                        [on-delete           (-> (-> connection? guid? guid?) connection? guid? guid?)])]
 [make-vanilla-entity                  (-> symbol?
                                           symbol?
                                           string?
                                           string?
                                           procedure?
                                           procedure?
                                           procedure?
                                           procedure?
                                           procedure?
                                           entity?)]
 [entity-make-vanilla-guid             (->* (entity? natural-number/c) (#:snooze (is-a?/c snooze-cache<%>)) guid?)]
 [entity-make-local-guid               (->* (entity?) (#:snooze (is-a?/c snooze-cache<%>)) guid?)]
 [entity-guid?                         (-> entity? any/c boolean?)]
 [entity-has-attribute?                (-> entity? (or/c symbol? attribute?) boolean?)]
 [entity-guid-attribute?               (-> entity? (or/c symbol? attribute?) boolean?)]
 [entity-attribute                     (-> entity? (or/c symbol? attribute?) attribute?)]
 [struct attribute                     ([name                symbol?]
                                        [column-name         symbol?]
                                        [pretty-name         string?]
                                        [pretty-name-plural  string?]
                                        [type                type?]
                                        [entity              entity?]
                                        [index               integer?]
                                        [default-maker       procedure?]
                                        [private-accessor    procedure?]
                                        [private-mutator     procedure?]
                                        [cached-accessor     procedure?]
                                        [cached-mutator      procedure?])]
 [attribute-default                    (->* (attribute?) (#:snooze (is-a?/c snooze-cache<%>)) any/c)]
 [prop:entity                          struct-type-property?]
 [prop:entity-set?                     (-> any/c boolean?)]
 [prop:entity-ref                      (-> any/c entity?)])
