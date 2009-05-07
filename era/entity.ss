#lang scheme/base

(require "../base.ss")

(require srfi/26
         (unlib-in symbol)
         "../generic/connection.ss"
         "cached-struct.ss"
         "core.ss")

;   symbol
;   (entity -> (listof symbol))
;   (entity -> (listof type))
;   (entity -> (listof (snooze -> any)))
;   (snooze-cache<%> (U natural #f) -> guid)
;   (any -> boolean)
;   [#:table-name   symbol]
;   [#:column-names (entity -> (listof symbol))]
;   [#:on-save      ((struct -> struct) struct -> struct)]
;   [#:on-delete    ((struct -> struct) struct -> struct)]
;   [#:properties   (alistof property any)]
; -> 
;   entity
;   struct-type
;   (any ... -> snooze-struct)
;   (any -> boolean)
(define (create-entity name
                       make-attr-names
                       make-attr-types
                       make-attr-defaults
                       guid-constructor
                       guid-predicate
                       #:table-name   [table-name        name]
                       #:column-names [make-column-names make-attr-names]
                       #:on-save      [on-save           (lambda (continue conn struct) (continue conn struct))]
                       #:on-delete    [on-delete         (lambda (continue conn struct) (continue conn struct))]
                       #:properties   [properties        null])
    
  ; entity
  (define entity
    (make-vanilla-entity name table-name guid-constructor guid-predicate on-save on-delete))
  
  ; (listof symbol)
  ; (listof type)
  ; (listof (snooze -> any))
  ; (listof symbol)
  (define-values (attr-names attr-types attr-defaults column-names)
    (values (make-attr-names    entity)
            (make-attr-types    entity)
            (make-attr-defaults entity)
            (make-column-names  entity)))
  
  ; integer
  ;
  ; Make sure attr-names, attr-types and column-names are all the same length,
  ; and bind that length to a variable.
  (define num-attrs
    (let ([num-args (length attr-names)])
      (cond [(not (= num-args (length attr-types)))
             (raise-type-error 'make-entity (format "~a attribute types" num-attrs) attr-types)]
            [(not (= num-args (length column-names)))
             (raise-type-error 'make-entity (format "~a column names" num-attrs) column-names)]
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
            (cons prop:equal+hash snooze-struct-equal+hash)
            properties)         ; properties
     #f))                       ; inspector-or-#f
  
  (define guid-attribute
    (let* ([name             'guid]
           [col              'id]
           [type             (make-guid-type #f entity)]
           [default-maker    (lambda (snooze) (make-guid #f #:snooze snooze))]
           [index            0])
      (create-attribute name col type entity index
                        default-maker struct-accessor struct-mutator)))
  
  (define revision-attribute
    (let* ([name             'revision]
           [col              'revision]
           [type             (make-integer-type #f)]
           [default-maker    (lambda (snooze) #f)]
           [index            1])
      (create-attribute name col type entity index
                        default-maker struct-accessor struct-mutator)))
  
  ; (listof attribute)
  (define attributes
    (list* guid-attribute
           revision-attribute
           (for/list ([index         (in-range 2 (+ num-attrs 2))]
                      [name          (in-list attr-names)]
                      [col           (in-list column-names)]
                      [type          (in-list attr-types)]
                      [default-maker (in-list attr-defaults)])
             (create-attribute name col type entity index
                               default-maker struct-accessor struct-mutator))))
  
  ; any ... -> guid
  (define cached-constructor
    (make-cached-constructor
     entity
     (symbol-append 'make- name)
     struct-constructor
     (- (length attributes) 2)))
  
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
  
  (values entity struct-type cached-constructor cached-predicate))

; Helpers ----------------------------------------

; symbol symbol type entity integer (snooze -> any) (struct natural -> any) (struct natural any -> void) -> attribute 
(define (create-attribute name col type entity index default-maker struct-accessor struct-mutator)
  (let* ([private-accessor (make-struct-field-accessor struct-accessor index name)]
         [private-mutator  (make-struct-field-mutator  struct-mutator  index name)]
         [cached-accessor  (make-cached-accessor private-accessor)]
         [cached-mutator   (make-cached-mutator  private-mutator)])
    (make-attribute name col type entity index
                    default-maker
                    private-accessor
                    private-mutator
                    cached-accessor
                    cached-mutator)))

; (listof procedure)
(define snooze-struct-equal+hash
  (list (lambda (struct1 struct2 same?)
          (let ([vec1 (struct->vector struct1)]
                [vec2 (struct->vector struct2)])
            (and (same? (vector-ref vec1 0)
                        (vector-ref vec2 0))
                 (same? (guid-id (vector-ref vec1 1))
                        (guid-id (vector-ref vec2 1)))
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
               (-> entity? (listof symbol?))
               (-> entity? (listof type?))
               (-> entity? (listof procedure?))
               procedure?
               (-> any/c boolean?))
              (#:table-name symbol?
                            #:column-names (-> entity? (listof symbol?))
                            #:on-save      procedure?
                            #:on-delete    procedure?
                            #:properties   (listof (cons/c
                                                    (and/c struct-type-property?
                                                           (not/c (cut eq? <> prop:entity))
                                                           (not/c (cut eq? <> prop:equal+hash)))
                                                    any/c)))
              (values entity?
                      struct-type?
                      procedure?
                      procedure?))])
