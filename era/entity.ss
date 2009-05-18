#lang scheme/base

(require "../base.ss")

(require srfi/26
         (unlib-in symbol)
         "../generic/connection.ss"
         (prefix-in cached: "cached-struct.ss")
         (prefix-in real: "snooze-struct.ss")
         "core.ss"
         "pretty.ss")

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
;   [#:on-save                  ((struct -> struct) struct -> struct)]
;   [#:on-delete                ((struct -> struct) struct -> struct)]
;   [#:properties               (alistof property any)]
;   [#:pretty-formatter         (guid any ... -> string)]
; -> 
;   entity
;   struct-type
;   (any ... -> snooze-struct)
;   (any -> boolean)
(define (create-entity name
                       attr-names
                       make-attr-types
                       attr-defaults
                       guid-constructor
                       guid-predicate
                       #:table-name               [table-name               name]
                       #:pretty-name              [pretty-name              (name->pretty-name name)]
                       #:pretty-name-plural       [pretty-name-plural       (pluralize-pretty-name pretty-name)]
                       #:pretty-formatter         [pretty-formatter         display]
                       #:attr-column-names        [attr-column-names        attr-names]
                       #:attr-pretty-names        [attr-pretty-names        (map name->pretty-name attr-names)]
                       #:attr-pretty-names-plural [attr-pretty-names-plural (map pluralize-pretty-name attr-pretty-names)]
                       #:on-save                  [on-save                  (lambda (continue conn struct) (continue conn struct))]
                       #:on-delete                [on-delete                (lambda (continue conn struct) (continue conn struct))]
                       #:properties               [properties               null])
  
  ; entity
  (define entity
    (make-vanilla-entity name table-name pretty-name pretty-name-plural pretty-formatter guid-constructor guid-predicate on-save on-delete))
  
  ; (listof type)
  (define attr-types (make-attr-types entity))
  
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
            (cons prop:custom-write snooze-struct-custom-write)
            (cons prop:equal+hash snooze-struct-equal+hash)
            properties)         ; properties
     #f))                       ; inspector-or-#f
  
  (define guid-attribute
    (let* ([name             'guid]
           [col              'id]
           [type             (make-guid-type #f entity)]
           [default-maker    (lambda (snooze) #f)]
           [index            0])
      (create-attribute name col "unique ID" "unique IDs" type entity index default-maker struct-accessor struct-mutator)))
  
  (define revision-attribute
    (let* ([name             'revision]
           [col              'revision]
           [type             (make-integer-type #f)]
           [default-maker    (lambda (snooze) #f)]
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
  
  ; any ... -> guid
  (define cached-constructor
    (cached:make-cached-constructor
     entity
     (symbol-append 'make- name)
     struct-constructor
     (- (length attributes) 2)))
  
  ; any -> boolean
  (define cached-predicate
    (cached:make-cached-predicate struct-predicate))
  
  ; Patch the entity:
  (set-entity-struct-type! entity struct-type)
  (set-entity-private-constructor! entity struct-constructor)
  (set-entity-private-predicate!   entity struct-predicate)
  (set-entity-private-accessor!    entity struct-accessor)
  (set-entity-private-mutator!     entity struct-mutator)
  (set-entity-cached-constructor!  entity cached-constructor)
  (set-entity-cached-predicate!    entity cached-predicate)
  (set-entity-attributes!          entity attributes)
  
  (values entity struct-type cached-constructor cached-predicate))

; Helpers ----------------------------------------

;  symbol symbol string string
;  type entity integer
;  (snooze -> any) (struct natural -> any)
; ->
;  attribute 
(define (create-attribute name col pretty pretty-plural type entity index default-maker struct-accessor struct-mutator)
  (let* ([private-accessor (make-struct-field-accessor struct-accessor index name)]
         [private-mutator  (make-struct-field-mutator  struct-mutator index name)]
         [cached-accessor  (cached:make-cached-accessor private-accessor)]
         [cached-mutator   (cached:make-cached-mutator  private-mutator)])
    (make-attribute name col pretty pretty-plural
                    type entity index
                    default-maker
                    private-accessor
                    private-mutator
                    cached-accessor
                    cached-mutator)))

; snooze-struct output-port boolean -> void
(define (snooze-struct-custom-write struct out write?)
  (parameterize ([in-cache-code? #t])
    (let* ([show   (if write? write display)]
           [entity (real:struct-entity struct)]
           [vals   (real:snooze-struct-ref* struct)]
           [guid   (car vals)]
           [rev    (cadr vals)])
      (display "#(struct:" out)
      (display (entity-name entity) out)
      (display " " out)
      (show (and guid (guid-id guid)) out)
      (display " " out)
      (show rev out)
      (for ([val (in-list (cddr vals))])
        (display " " out)
        (show val out))
      (display ")" out))))

; (listof procedure)
(define snooze-struct-equal+hash
  (list (lambda (struct1 struct2 same?)
          (let* ([vec1  (struct->vector struct1)]
                 [vec2  (struct->vector struct2)]
                 [type1 (vector-ref vec1 0)]
                 [type2 (vector-ref vec2 0)]
                 [guid1 (vector-ref vec1 1)]
                 [guid2 (vector-ref vec2 1)])
            (and (same? type1 type2)
                 (same? (and guid1 (guid-id guid1))
                        (and guid2 (guid-id guid2)))
                 (for/and ([item1 (in-vector vec1 2)]
                           [item2 (in-vector vec2 2)])
                   (same? item1 item2)))))
        (lambda (struct recur)
          (recur struct))
        (lambda (struct recur)
          (recur struct))))

; Provide statements -----------------------------

(provide/contract
 [rename create-entity
         make-entity
         (->* (symbol?
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
                            #:properties               (listof (cons/c
                                                                (and/c struct-type-property?
                                                                       (not/c (cut eq? <> prop:entity))
                                                                       (not/c (cut eq? <> prop:equal+hash)))
                                                                any/c))
                            #:pretty-formatter         procedure?)
              (values entity?
                      struct-type?
                      procedure?
                      procedure?))])
