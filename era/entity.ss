#lang scheme/base

(require "../base.ss")

(require srfi/26
         (unlib-in symbol)
         "../generic/connection.ss"
         "snooze-struct.ss"
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
                       attr-names
                       make-attr-types
                       attr-defaults
                       guid-constructor
                       guid-predicate
                       #:table-name               [table-name               name]
                       #:pretty-name              [pretty-name              (name->pretty-name name)]
                       #:pretty-name-plural       [pretty-name-plural       (pluralize-pretty-name pretty-name)]
                       #:pretty-formatter         [pretty-formatter         (cut format "~a" <>)]
                       #:attr-column-names        [attr-column-names        attr-names]
                       #:attr-pretty-names        [attr-pretty-names        (map name->pretty-name attr-names)]
                       #:attr-pretty-names-plural [attr-pretty-names-plural (map pluralize-pretty-name attr-pretty-names)]
                       #:on-save                  [on-save                  #f]
                       #:on-delete                [on-delete                #f]
                       #:save-check               [save-check               #f]
                       #:delete-check             [delete-check             #f]
                       #:properties               [properties               null])
  
  ; entity
  (define entity
    (make-vanilla-entity name table-name pretty-name pretty-name-plural pretty-formatter guid-constructor guid-predicate))
  
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
           [col              'guid]
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
  
  ; vanilla-guid revision any ... -> guid
  (define cached-constructor
    (make-cached-constructor
     entity
     (symbol-append 'make- name)
     struct-constructor
     (length attributes)))
  
  ; any ... -> guid
  (define (public-constructor #:snooze [snooze (current-snooze)]. args)
    (apply cached-constructor #:snooze snooze #f #f args))
  
  ; any -> boolean
  (define cached-predicate
    (make-cached-predicate struct-predicate))
  
  ; Patch the entity:
  (set-entity-struct-type! entity struct-type)
  (set-entity-private-constructor! entity struct-constructor)
  (set-entity-private-predicate!   entity struct-predicate)
  (set-entity-private-accessor!    entity struct-accessor)
  (set-entity-private-mutator!     entity struct-mutator)
  (set-entity-cached-constructor!  entity cached-constructor)
  (set-entity-cached-predicate!    entity cached-predicate)
  (set-entity-attributes!          entity attributes)
  (set-entity-save-check!          entity (or save-check (make-default-save-check   entity)))
  (set-entity-delete-check!        entity (or save-check (make-default-delete-check entity)))
  (set-entity-on-save!             entity (or on-save    (make-default-save-hook    entity)))
  (set-entity-on-delete!           entity (or on-delete  (make-default-delete-hook  entity)))
  
  (values entity struct-type public-constructor cached-predicate))

; Helpers ----------------------------------------

;  symbol symbol string string
;  type entity integer
;  (snooze -> any) (struct natural -> any)
; ->
;  attribute 
(define (create-attribute name col pretty pretty-plural type entity index default-maker struct-accessor struct-mutator)
  (let* ([private-accessor (make-struct-field-accessor struct-accessor index name)]
         [private-mutator  (make-struct-field-mutator  struct-mutator index name)]
         [cached-accessor  (make-cached-accessor private-accessor)]
         [cached-mutator   (make-cached-mutator  private-mutator)])
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
           [entity (snooze-struct-entity struct)]
           [vals   (snooze-struct-ref* struct)]
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

; entity symbol (any ... -> snooze-struct) natural -> ([#:snooze snooze<%>] any ... -> guid)
;
; The returned constructor has the same number of arguments as (entity-private-constructor entity):
; it takes a guid and revision as well as regular data attributes.
(define (make-cached-constructor
         entity
         procedure-name
         struct-constructor
         expected-arity)
  (lambda (#:snooze [snooze (current-snooze)] . args)
    (let ([cache (send snooze get-current-cache)])
      (unless (= (length args) expected-arity)
        (raise-exn exn:fail:contract
          (format "~a: expected ~a non-keyword argument(s), received ~s"
                  procedure-name
                  expected-arity
                  args)))
      (send cache add-copied-struct! (apply struct-constructor args)))))

; (any -> boolean) -> (any -> boolean)
(define (make-cached-predicate struct-predicate)
  (lambda (guid)
    (struct-predicate (guid-ref guid))))

; (struct -> any) -> (guid -> any)
(define (make-cached-accessor struct-accessor)
  (lambda (guid)
    (let ([ans (struct-accessor (guid-ref guid))])
      (if (guid? ans)
          (send (guid-snooze ans) find-by-guid ans)
          ans))))

; (struct any -> void) -> (guid any -> void)
(define (make-cached-mutator struct-mutator)
  (lambda (guid val)
    (struct-mutator (guid-ref guid) val)))

; entity -> (guid -> (listof check-result))
(define (make-default-save-check entity)
  (lambda (guid) null))

; entity -> (guid -> (listof check-result))
(define (make-default-delete-check entity)
  (lambda (guid) null))

; entity -> ((connection guid -> guid) connection guid -> guid)
(define (make-default-save-hook entity)
  (lambda (continue conn guid) (continue conn guid)))

; entity -> ((connection guid -> guid) connection guid -> guid)
(define (make-default-delete-hook entity)
  (lambda (continue conn guid) (continue conn guid)))

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
