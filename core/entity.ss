#lang scheme/base

(require "../base.ss")

(require srfi/26
         (unlib-in symbol)
         "../common/connection.ss"
         "snooze-struct.ss"
         "check-default.ss"
         "struct.ss"
         "pretty.ss")

;   symbol
;   symbol
;   (listof symbol)
;   (entity -> (listof type))
;   (listof (snooze -> any))
;   (snooze<%> (U natural #f) -> guid)
;   (any -> boolean)
;   [#:table-name               symbol]
;   [#:pretty-name              string]
;   [#:pretty-name-plural       string]
;   [#:attr-column-names        (listof symbol)]
;   [#:attr-pretty-names        (listof string)]
;   [#:attr-pretty-names-plural (listof string)]
;   [#:on-save                  (U ((struct -> struct) struct -> struct) #f)]
;   [#:on-delete                (U ((struct -> struct) struct -> struct) #f)]
;   [#:save-check               (U (struct -> (listof check-result)) #f)]
;   [#:delete-check             (U (struct -> (listof check-result)) #f)]
;   [#:properties               (alistof property any)]
;   [#:pretty-formatter         (guid any ... -> string)]
; -> 
;   entity
;   struct-type
;   (any ... -> snooze-struct)
;   (any -> boolean)
(define (create-entity name
                       plural-name
                       attr-names
                       make-attr-types
                       attr-defaults
                       guid-constructor
                       guid-predicate
                       #:table-name               [table-name               (name->database-name name)]
                       #:pretty-name              [pretty-name              (name->pretty-name   name)]
                       #:pretty-name-plural       [pretty-name-plural       (name->pretty-name   plural-name)]
                       #:pretty-formatter         [pretty-formatter         (cut format "~a" <>)]
                       #:attr-column-names        [attr-column-names        attr-names]
                       #:attr-pretty-names        [attr-pretty-names        (map name->pretty-name attr-names)]
                       #:attr-pretty-names-plural [attr-pretty-names-plural (map name->pretty-name
                                                                                 (map name->plural-name attr-pretty-names))]
                       #:on-save                  [on-save                  #f]
                       #:on-delete                [on-delete                #f]
                       #:save-check               [save-check               #f]
                       #:delete-check             [delete-check             #f]
                       #:properties               [properties               null])
  
  ; entity
  (define entity
    (make-vanilla-entity
     name
     plural-name
     table-name
     pretty-name
     pretty-name-plural
     pretty-formatter
     guid-constructor
     guid-predicate))
  
  ; (listof type)
  (define attr-types
    (make-attr-types entity))
  
  ; integer
  ;
  ; Make sure attr-names, attr-types, and other attr-foo lists are all the same length,
  ; and bind that length to a variable.
  (define num-attrs
    (let ([num-args (length attr-names)])
      (cond [(not (= num-args (length attr-types)))
             (raise-type-error 'make-entity (format "~a attribute types" num-attrs) attr-types)]
            [(not (= num-args (length attr-column-names)))
             (raise-type-error 'make-entity (format "~a column names" num-attrs) attr-column-names)]
            [(not (= num-args (length attr-pretty-names)))
             (raise-type-error 'make-entity (format "~a pretty names" num-attrs) attr-pretty-names)]
            [(not (= num-args (length attr-pretty-names-plural)))
             (raise-type-error 'make-entity (format "~a plural pretty names" num-attrs) attr-pretty-names-plural)]
            [else (+ num-args 2)])))
  
  ; struct-type-descriptor
  ; any ... -> struct
  ; struct -> boolean
  ; struct integer -> any
  ; struct integer any -> void
  (define-values (struct-type struct-constructor struct-predicate struct-accessor struct-mutator)
    (make-struct-type 
     name                       ; name symbol
     #f                         ; supertype
     num-attrs                  ; number of fields passed in constructor (excludes fields from supertype)
     0                          ; number of auto-value fields
     (void)                     ; values for auto-value fields
     (list* (cons prop:entity entity)
            properties)         ; properties
     #f))                       ; inspector-or-#f
  
  (define guid-attribute
    (let* ([name             'guid]
           [col              'guid]
           [type             (make-guid-type #f entity)]
           [default-maker    (lambda () #f)]
           [index            0])
      (create-attribute name col "unique ID" "unique IDs" type entity index default-maker struct-accessor struct-mutator)))
  
  (define revision-attribute
    (let* ([name             'revision]
           [col              'revision]
           [type             (make-integer-type #f 0 #f)]
           [default-maker    (lambda () #f)]
           [index            1])
      (create-attribute name col "revision" "revisions" type entity index default-maker struct-accessor struct-mutator)))
  
  ; (listof attribute)
  (define attributes
    (list* guid-attribute
           revision-attribute
           (for/list ([index         (in-range 2 (+ num-attrs 2))]
                      [name          (in-list attr-names)]
                      [col           (in-list attr-column-names)]
                      [pretty        (in-list attr-pretty-names)]
                      [pretty-plural (in-list attr-pretty-names-plural)]
                      [type          (in-list attr-types)]
                      [default-maker (in-list attr-defaults)])
             (create-attribute name col pretty pretty-plural type entity index default-maker struct-accessor struct-mutator))))
  
  ; guid revision any ... -> guid
  (define constructor struct-constructor)
  
  ; any ... -> guid
  (define (public-constructor . args)
    (apply constructor (entity-make-temporary-guid entity) #f args))
  
  ; any -> boolean
  (define predicate struct-predicate)
  
  ; Patch the entity:
  (set-entity-struct-type! entity struct-type)
  (set-entity-private-constructor! entity struct-constructor)
  (set-entity-private-predicate!   entity struct-predicate)
  (set-entity-private-accessor!    entity struct-accessor)
  (set-entity-private-mutator!     entity struct-mutator)
  (set-entity-constructor!         entity constructor)
  (set-entity-predicate!           entity predicate)
  (set-entity-attributes!          entity attributes)
  (set-entity-save-check!          entity (or save-check   default-check-snooze-struct))
  (set-entity-delete-check!        entity (or delete-check default-check-old-snooze-struct))
  (set-entity-on-save!             entity (or on-save      (make-default-save-hook    entity)))
  (set-entity-on-delete!           entity (or on-delete    (make-default-delete-hook  entity)))
  
  (values entity struct-type public-constructor predicate))

; Helpers ----------------------------------------

; symbol -> symbol
(define (name->database-name name)
  (string->symbol (regexp-replace* #px"[^a-z0-9]" (string-downcase (symbol->string name)) "")))

;  symbol symbol string string
;  type entity integer
;  (snooze -> any) (struct natural -> any)
; ->
;  attribute 
(define (create-attribute name col pretty pretty-plural type entity index default-maker struct-accessor struct-mutator)
  (let* ([private-accessor (make-struct-field-accessor struct-accessor index name)]
         [private-mutator  (make-struct-field-mutator  struct-mutator index name)]
         [accessor         (make-accessor private-accessor)]
         [mutator          private-mutator])
    (make-attribute name col pretty pretty-plural
                    type entity index
                    default-maker
                    private-accessor
                    private-mutator
                    accessor
                    mutator)))

; (struct -> any) -> (guid -> any)
(define (make-accessor struct-accessor)
  (lambda (struct)
    (let ([ans (struct-accessor struct)])
      (if (guid? ans)
          (send (current-snooze) find-by-guid ans)
          ans))))

; Provide statements -----------------------------

(define snooze-struct-property-list/c
  (listof (cons/c (and/c struct-type-property?
                         (not/c (cut eq? <> prop:entity)))
                  any/c)))

(provide/contract
 [rename create-entity make-entity
         (->* (symbol?
               symbol?
               (listof symbol?)
               (-> entity? (listof type?))
               (listof procedure?)
               procedure?
               (-> any/c boolean?))
              (#:table-name symbol?
                            #:pretty-name              string?
                            #:pretty-name-plural       string?
                            #:attr-column-names        (listof symbol?)
                            #:attr-pretty-names        (listof string?)
                            #:attr-pretty-names-plural (listof string?)
                            #:on-save                  procedure?
                            #:on-delete                procedure?
                            #:properties               snooze-struct-property-list/c
                            #:pretty-formatter         procedure?)
              (values entity?
                      struct-type?
                      procedure?
                      procedure?))]
 [name->database-name (-> symbol? symbol?)])
