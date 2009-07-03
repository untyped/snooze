#lang scheme/base

(require "../base.ss")

(require (for-syntax scheme/base
                     (unlib-in syntax))
         scheme/dict
         (unlib-in enumeration)
         "../common/connection.ss"
         "core-snooze-interface.ss")

; Snooze objects ---------------------------------

(define current-snooze (make-parameter #f))

; Guids ------------------------------------------

; (struct snooze<%> integer)
(define-serializable-struct guid
  (id+serial snooze)
  #:transparent
  #:property
  prop:custom-write
  (lambda (guid out write?)
    (define show (if write? write display))
    (unless (guid? guid)
      (raise-type-error 'guid.custom-write "guid" guid))
    (show (vector (string->symbol (format "guid:~a" (guid-id+serial guid)))) out)))

; property
; guid -> boolean
; guid -> entity
(define-values (prop:guid-entity-box guid-entity? guid-entity-box)
  (make-struct-type-property 'guid-entity-box))

; guid -> guid
(define (copy-guid guid)
  (unless (guid? guid)
    (raise-type-error 'copy-guid "guid" guid))
  ((entity-guid-constructor (guid-entity guid))
   (guid-id+serial guid)
   (guid-snooze guid)))

; guid -> (U integer #f)
(define (guid-id guid)
  (unless (guid? guid)
    (raise-type-error 'guid-id "guid" guid))
  (let ([id+serial (guid-id+serial guid)])
    (and (number? id+serial)
         id+serial)))

; guid -> (U symbol #f)
(define (guid-serial guid)
  (unless (guid? guid)
    (raise-type-error 'guid-serial "guid" guid))
  (let ([id+serial (guid-id+serial guid)])
    (and (symbol? id+serial)
         id+serial)))

; guid guid -> boolean
(define (guid=? guid1 guid2)
  (unless (guid? guid1) (raise-type-error 'guid=?-first-arg "guid" guid1))
  (unless (guid? guid2) (raise-type-error 'guid=?-second-arg "guid" guid2))
  (and (eq? (guid-id+serial guid1)
            (guid-id+serial guid2))
       (eq? (guid-entity guid1)
            (guid-entity guid2))
       (eq? (guid-snooze guid1)
            (guid-snooze guid2))))

; guid guid -> boolean
(define (guid=?-hash-code guid)
  (unless (guid? guid) (raise-type-error 'guid=?-hash-code "guid" guid))
  (equal-hash-code
   (list (guid-id+serial guid)
         (entity-name (guid-entity guid))
         (guid-snooze guid))))

; guid -> boolean
(define (guid-local? guid)
  (and (guid-serial guid) #t))

; guid -> boolean
(define (guid-vanilla? guid)
  (not (guid-serial guid)))

; any -> boolean
(define (local-guid? guid)
  (and (guid? guid) (guid-local? guid)))

; any -> boolean
(define (vanilla-guid? guid)
  (and (guid? guid) (guid-vanilla? guid)))

; (_ id entity)
(define-syntax (define-guid-type stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([custom-write-id (make-id #f #'id '.custom-write)])
       #'(define-serializable-struct (id guid)
           ()
           #:transparent
           #:property
           prop:guid-entity-box
           (box #f)
           #:property
           prop:custom-write
           (lambda (guid out write?)
             (unless (guid? guid)
               (raise-type-error 'custom-write-id "guid" guid))
             (let ([show   (if write? write display)]
                   [struct (and (not (in-cache-code?))
                                (with-handlers ([exn? (lambda (exn) 'uncached)])
                                  (guid-ref guid)))])
               (parameterize ([in-cache-code? #t])
                 (if (eq? struct 'uncached)
                     (show (vector 'id 'uncached) out)
                     (let* ([ans   (struct->vector struct)]
                            [guid* (vector-ref ans 1)])
                       (vector-set! ans 0 'id)
                       (vector-set! ans 1 (list 'ext (guid-id+serial guid)
                                                'int (and guid* (guid-id+serial guid*))))
                       (for ([i (in-range 2 (vector-length ans))])
                         (let ([val (vector-ref ans i)])
                           (when (guid? val)
                             (vector-set! ans i (vector (string->symbol (format "guid:~a" (guid-id+serial guid))))))))
                       (show ans out))))))
           #:property
           prop:equal+hash
           (list (lambda (guid1 guid2 same?)
                   (let ([struct1 (guid-ref guid1)]
                         [struct2 (guid-ref guid2)])
                     (or (and (not struct1)
                              (not struct2))
                         (and struct1
                              struct2
                              (for/and ([a (in-vector (struct->vector (guid-ref guid1)))]
                                        [b (in-vector (struct->vector (guid-ref guid2)))])
                                (if (guid? a)
                                    (if (guid? b)
                                        (guid=? a b)
                                        #f)
                                    (if (guid? b)
                                        #f
                                        (same? a b))))))))
                 (lambda (guid recur) (recur guid))
                 (lambda (guid recur) (recur guid)))))]))

; guid -> entity
(define (guid-entity guid)
  (unbox (guid-entity-box guid)))

; guid -> snooze-struct
(define (guid-ref guid)
  (parameterize ([in-cache-code? #t])
    (or (send (send (guid-snooze guid) get-current-cache) cache-ref/local guid)
        (raise-exn exn:fail:snooze:cache
          (format "guid not cached: ~s" guid)))))

; Interning guids --------------------------------

; (weak-custom-hashof guid (weak-box guid))
(define interned-guids
  (make-weak-custom-hash guid=? guid=?-hash-code))

; guid -> boolean
(define (guid-interned? guid)
  (let ([box (dict-ref interned-guids guid #f)])
    (and box (eq? (weak-box-value box) guid))))

; any -> boolean
(define (interned-guid? guid)
  (and (guid? guid)
       (guid-interned? guid)))

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

; (struct boolean entity)
(define-serializable-struct (guid-type type) (entity) #:transparent)

; (struct boolean)
(define-serializable-struct (boolean-type type) () #:transparent)

; (struct boolean number number)
(define-serializable-struct (numeric-type type) (min-value max-value) #:transparent)
(define-serializable-struct (integer-type numeric-type) () #:transparent)
(define-serializable-struct (real-type    numeric-type) () #:transparent)

; (struct boolean (U natural #f))
(define-serializable-struct (character-type type) (max-length) #:transparent)
(define-serializable-struct (string-type character-type) () #:transparent)
(define-serializable-struct (symbol-type character-type) () #:transparent)

; (struct boolean (U natural #f) (U enum #f) (listof symbol))
(define-serializable-struct (enum-type symbol-type) (enum values) #:transparent)

; (struct boolean)
(define-serializable-struct (temporal-type type)          () #:transparent)
(define-serializable-struct (time-utc-type temporal-type) () #:transparent)
(define-serializable-struct (time-tai-type temporal-type) () #:transparent)

; (struct boolean)
(define-serializable-struct (binary-type type) () #:transparent)

; boolean (U enum (listof symbol)) -> enum-type
(define (create-enum-type allow-null? enum+vals)
  (let* ([enum       (if (enum? enum+vals) enum+vals #f)]
         [vals       (if (enum? enum+vals) (enum-values enum+vals) enum+vals)]
         [max-length (foldl max 0 (map string-length (map symbol->string vals)))])
    (make-enum-type allow-null? max-length enum vals)))

; type -> any
(define (type-null type)
  (if (boolean-type? type)
      (void)
      #f))

; type any -> boolean
(define (type-valid? type val)
  (if (equal? val (type-null type))
      (type-allows-null? type)
      (match type
        [(struct guid-type (_ entity))     ((entity-cached-predicate entity) val)]
        [(? boolean-type?)                 (boolean? val)]
        [(struct integer-type (_ min max)) (and (integer? val)
                                                (or (not min) (>= val min))
                                                (or (not max) (<= val max)))]
        [(struct real-type (_ min max))    (and (number? val)
                                                (or (not min) (>= val min))
                                                (or (not max) (<= val max)))]
        [(struct enum-type (_ _ _ vals))   (and (member val vals) #t)]
        [(struct string-type (_ max))      (and (string? val)
                                                (or (not max)
                                                    (<= (string-length val) max)))]
        [(struct symbol-type (_ max))      (and (symbol? val)
                                                (or (not max)
                                                    (<= (string-length (symbol->string val)) max)))]
        [(? time-tai-type?)                (time-tai? val)]
        [(? time-utc-type?)                (time-utc? val)]
        [(? binary-type?)                  (serializable? val)])))

; type -> symbol
(define (type-name type)
  (cond [(guid-type? type)     (entity-name (guid-type-entity type))]
        [(boolean-type? type)  'boolean]
        [(integer-type? type)  'integer]
        [(real-type? type)     'real]
        [(string-type? type)   'string]
        [(symbol-type? type)   'symbol]
        [(enum-type? type)     'enum]
        [(time-utc-type? type) 'time-utc]
        [(time-tai-type? type) 'time-tai]
        [(binary-type? type)   'binary]))

; type type -> boolean
;
; Returns #t if the arguments are of the same class of type (boolean types, integer types, etc).
(define (type-compatible? type1 type2)
  (or (and (guid-type?     type1) (guid-type?     type2)
           (eq? (guid-type-entity type1)
                (guid-type-entity type2)))
      (and (boolean-type?  type1) (boolean-type?  type2))
      (and (integer-type?  type1) (integer-type?  type2))
      (and (real-type?     type1) (real-type?     type2))
      (and (string-type?   type1) (string-type?   type2))
      (and (symbol-type?   type1) (symbol-type?   type2))
      (and (time-utc-type? type1) (time-utc-type? type2))
      (and (time-tai-type? type1) (time-tai-type? type2))
      (and (binary-type?   type1) (binary-type?   type2))))

; type -> contract
(define (type-contract type)
  
  ; boolean -> list
  (define (null-name null?)
    (if null?
        '(#:allow-null? #t)
        '(#:allow-null? #f)))
  
  (flat-named-contract
   (match type
     [(struct guid-type (null? entity))     `(foreign-key/c ,(entity-name entity) ,@(null-name null?))]
     [(? boolean-type?)                     '(boolean-attr/c)]
     [(struct numeric-type (null? min max)) `(,(if (integer-type? type) 'integer-attr/c 'real-attr/c)
                                              ,@(if min `(#:min-value ,min) null)
                                              ,@(if max `(#:max-value ,max) null)
                                              ,@(null-name null?))]
     [(struct enum-type (null? _ _ values)) `(enum-attr/c #:values ,values ,@(null-name null?))]
     [(struct character-type (null? max))   `(,(if (symbol-type? type) 'symbol-attr/c 'string-attr/c)
                                              ,@(if max `(#:max-length ,max) null)
                                              ,@(null-name null?))]
     [(struct time-utc-type (null?))        `(time-utc-attr/c ,@(null-name null?))]
     [(struct time-tai-type (null?))        `(time-tai-attr/c ,@(null-name null?))]
     [(struct binary-type (null?))          `(binary-attr/c ,@(null-name null?))])
   (cut type-valid? type <>)))

; type
(define type:boolean  (make-boolean-type  #t))
(define type:integer  (make-integer-type  #t #f #f))
(define type:real     (make-real-type     #t #f #f))
(define type:string   (make-string-type   #t #f))
(define type:symbol   (make-symbol-type   #t #f))
(define type:time-tai (make-time-tai-type #t))
(define type:time-utc (make-time-utc-type #t))
(define type:binary   (make-binary-type   #t))

; Entities ---------------------------------------

; (struct symbol 
;         symbol
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
;         (struct -> (listof check-result))
;         (struct -> (listof check-result))
(define-struct entity 
  (name
   plural-name
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
   defaults-constructor
   copy-constructor
   guid-constructor
   guid-predicate
   attributes
   default-alias
   on-save
   on-delete
   save-check
   delete-check)
  #:transparent
  #:mutable
  #:property 
  prop:custom-write
  (lambda (entity port write?)
    (fprintf port "#<entity:~a>" (entity-name entity))))

;  symbol
;  symbol
;  symbol
;  string
;  string
;  (guid any ... -> string)
;  ((U natural #f) -> guid)
;  ((U guid any) -> boolean)
; ->
;  entity
;
; Entities have mutual dependencies with the struct types they are attached to.
; We can't mutate struct types, so we have to create them first and then patch
; the entity. This procedure gets around the field contracts on entity during
; the creation process.
(define (make-vanilla-entity
         name
         plural-name
         table-name
         pretty-name
         pretty-name-plural
         pretty-formatter
         guid-constructor
         guid-predicate)
  (let ([empty-hook  (lambda (continue conn guid) (continue conn guid))]
        [empty-check (lambda (guid) null)])
    (make-entity name plural-name table-name pretty-name pretty-name-plural pretty-formatter
                 #f                       ; struct type
                 #f #f #f #f #f #f #f #f  ; constructors and predicates
                 guid-constructor         ; guid constructor
                 guid-predicate           ; guid predicate
                 null                     ; attributes
                 #f                       ; default-alias
                 empty-hook               ; hooks
                 empty-hook               ;
                 empty-check              ; validation
                 empty-check)))           ;

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

; entity -> (listof attribute)
(define (entity-data-attributes entity)
  (cddr (entity-attributes entity)))

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
  (parameterize ([current-snooze snooze])
    ((attribute-default-maker attr))))

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

; Check results ----------------------------------

; (struct string (hasheqof symbol any))
(define-serializable-struct check-result (message annotations) #:transparent)
(define-serializable-struct (check-success check-result) () #:transparent)    ; okay
(define-serializable-struct (check-problem check-result) () #:transparent)    ; problem
(define-serializable-struct (check-warning check-problem) () #:transparent)   ; problem, can save/delete anyway
(define-serializable-struct (check-error   check-problem) () #:transparent)   ; problem, cannot save/delete
(define-serializable-struct (check-failure check-error) () #:transparent)     ; problem, cannot save/delete, caused by user

; (struct string (hasheqof symbol any) exn)
(define-serializable-struct (check-fatal check-error) (exn) #:transparent)    ; problem, cannot save/delete, caused by exn

; contract
(define annotations/c
  (and/c hash? hash-eq?))

; Provide statements -----------------------------

(provide (all-from-out "core-snooze-interface.ss")
         define-guid-type
         guid
         struct:guid)

(provide/contract
 [current-snooze                       (parameter/c (or/c (is-a?/c snooze<%>) #f))]
 [guid?                                procedure?]
 [guid=?                               (-> guid? guid? boolean?)]
 [guid=?-hash-code                     (-> guid? number?)]
 [guid-local?                          (-> guid? boolean?)]
 [guid-vanilla?                        (-> guid? boolean?)]
 [local-guid?                          (-> any/c boolean?)]
 [vanilla-guid?                        (-> any/c boolean?)]
 [guid-snooze                          (-> guid? (is-a?/c snooze<%>))]
 [copy-guid                            (-> guid? guid?)]
 [guid-id                              (-> guid? (or/c natural-number/c #f))]
 [guid-serial                          (-> guid? (or/c symbol? #f))]
 [guid-entity-box                      (-> struct-type? box?)]
 [guid-entity                          (-> guid? entity?)]
 [guid-ref                             (-> guid? snooze-struct?)]
 [guid-interned?                       (-> guid? boolean?)]
 [interned-guid?                       (-> any/c boolean?)]
 [intern-guid                          (-> (and/c guid? (not/c guid-local?)) guid?)]
 [struct type                          ([allows-null? boolean?])]
 [struct (guid-type type)              ([allows-null? boolean?] [entity entity?])]
 [struct (boolean-type type)           ([allows-null? boolean?])]
 [struct (numeric-type type)           ([allows-null? boolean?]
                                        [min-value    (or/c number? #f)]
                                        [max-value    (or/c number? #f)])]
 [struct (integer-type numeric-type)   ([allows-null? boolean?]
                                        [min-value    (or/c integer? #f)]
                                        [max-value    (or/c integer? #f)])]
 [struct (real-type numeric-type)      ([allows-null? boolean?]
                                        [min-value    (or/c number? #f)]
                                        [max-value    (or/c number? #f)])]
 [struct (character-type type)         ([allows-null? boolean?]
                                        [max-length   (or/c natural-number/c #f)])]
 [struct (string-type character-type)  ([allows-null? boolean?]
                                        [max-length   (or/c natural-number/c #f)])]
 [struct (symbol-type character-type)  ([allows-null? boolean?]
                                        [max-length   (or/c natural-number/c #f)])]
 [struct (enum-type symbol-type)       ([allows-null? boolean?]
                                        [max-length   (or/c natural-number/c #f)]
                                        [enum         (or/c enum? #f)]
                                        [values       (listof symbol?)])]
 [struct (temporal-type type)          ([allows-null? boolean?])]
 [struct (time-utc-type temporal-type) ([allows-null? boolean?])]
 [struct (time-tai-type temporal-type) ([allows-null? boolean?])]
 [struct (binary-type type)            ([allows-null? boolean?])]
 [create-enum-type                     (-> boolean? (or/c enum? (listof symbol?)) enum-type?)]
 [type-null                            (-> type? any)]
 [type-valid?                          (-> type? any/c boolean?)]
 [type-name                            (-> type? symbol?)]
 [type-compatible?                     (-> type? type? boolean?)]
 [type-contract                        (-> type? contract?)]
 [type:boolean                         boolean-type?]
 [type:integer                         integer-type?]
 [type:real                            real-type?]
 [type:string                          string-type?]
 [type:symbol                          symbol-type?]
 [type:time-tai                        time-tai-type?]
 [type:time-utc                        time-utc-type?]
 [type:binary                          binary-type?]
 [struct entity                        ([name                 symbol?]
                                        [plural-name          symbol?]
                                        [table-name           symbol?]
                                        [pretty-name          string?]
                                        [pretty-name-plural   string?]
                                        [pretty-formatter     procedure?]
                                        [struct-type          struct-type?]
                                        [private-constructor  procedure?]
                                        [private-predicate    procedure?]
                                        [private-accessor     procedure?]
                                        [private-mutator      procedure?]
                                        [cached-constructor   procedure?]
                                        [cached-predicate     procedure?]
                                        [defaults-constructor procedure?]
                                        [copy-constructor     procedure?]
                                        [guid-constructor     (-> (or/c natural-number/c #f)
                                                                  (or/c symbol? #f)
                                                                  (is-a?/c snooze<%>)
                                                                  guid?)]
                                        [guid-predicate       (-> any/c boolean?)]
                                        [attributes           (listof attribute?)]
                                        [default-alias        any/c]
                                        [on-save              (-> (-> connection? guid? guid?) connection? guid? guid?)]
                                        [on-delete            (-> (-> connection? guid? guid?) connection? guid? guid?)]
                                        [save-check           procedure?]
                                        [delete-check         procedure?])]
 [make-vanilla-entity                  (-> symbol?
                                           symbol?
                                           symbol?
                                           string?
                                           string?
                                           procedure?
                                           procedure?
                                           procedure?
                                           entity?)]
 [entity-make-vanilla-guid             (->* (entity? natural-number/c) (#:snooze (is-a?/c snooze<%>)) guid?)]
 [entity-make-local-guid               (->* (entity?) (#:snooze (is-a?/c snooze<%>)) guid?)]
 [entity-guid?                         (-> entity? any/c boolean?)]
 [entity-has-attribute?                (-> entity? (or/c symbol? attribute?) boolean?)]
 [entity-guid-attribute?               (-> entity? (or/c symbol? attribute?) boolean?)]
 [entity-attribute                     (-> entity? (or/c symbol? attribute?) attribute?)]
 [entity-data-attributes               (-> entity? (listof attribute?))]
 [struct attribute                     ([name                 symbol?]
                                        [column-name          symbol?]
                                        [pretty-name          string?]
                                        [pretty-name-plural   string?]
                                        [type                 type?]
                                        [entity               entity?]
                                        [index                integer?]
                                        [default-maker        procedure?]
                                        [private-accessor     procedure?]
                                        [private-mutator      procedure?]
                                        [cached-accessor      procedure?]
                                        [cached-mutator       procedure?])]
 [attribute-default                    (->* (attribute?) (#:snooze (is-a?/c snooze<%>)) any/c)]
 [prop:entity                          struct-type-property?]
 [prop:entity-set?                     (-> any/c boolean?)]
 [prop:entity-ref                      (-> any/c entity?)]
 [struct check-result                  ([message string?] [annotations annotations/c])]
 [struct (check-success check-result)  ([message string?] [annotations annotations/c])]
 [struct (check-problem check-result)  ([message string?] [annotations annotations/c])]
 [struct (check-warning check-problem) ([message string?] [annotations annotations/c])]
 [struct (check-error   check-problem) ([message string?] [annotations annotations/c])]
 [struct (check-failure check-error)   ([message string?] [annotations annotations/c])]
 [struct (check-fatal   check-error)   ([message string?] [annotations annotations/c] [exn exn?])])
