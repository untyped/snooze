#lang scheme/base

(require "../base.ss")

(require srfi/26
         (unlib-in symbol)
         "../generic/connection.ss"
         "cache.ss"
         "core.ss")

;   symbol
;   (listof symbol)
;   (listof type)
;   ((U natural #f) -> guid)
;   (any -> boolean)
;   [#:table-name   symbol]
;   [#:column-names (listof symbol)]
;   [#:on-save      ((struct -> struct) struct -> struct)]
;   [#:on-delete    ((struct -> struct) struct -> struct)]
;   [#:properties   (alistof property any)]
; -> 
;   entity
;   struct-type
;   (any ... -> snooze-struct)
;   (any -> boolean)
(define (create-entity name
                       attr-names
                       attr-types
                       guid-constructor
                       guid-predicate
                       #:table-name   [table-name   name]
                       #:column-names [column-names attr-names]
                       #:on-save      [on-save      (lambda (continue struct) (continue struct))]
                       #:on-delete    [on-delete    (lambda (continue struct) (continue struct))]
                       #:properties   [properties   null])
  
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
  
  ; entity
  (define entity
    (make-vanilla-entity name table-name guid-constructor guid-predicate on-save on-delete))
  
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
     (cons (cons prop:entity entity) properties) ; properties
     #f))                       ; inspector-or-#f
  
  (define guid-attribute
    (let* ([name             'guid]
           [col              'id]
           [type             (make-guid-type #f (cut guid-constructor #f) entity)]
           [index            0])
      (create-attribute name col type entity index struct-accessor struct-mutator)))
  
  (define revision-attribute
    (let* ([name             'revision]
           [col              'revision]
           [type             (make-integer-type #f (lambda () #f))]
           [index            1])
      (create-attribute name col type entity index struct-accessor struct-mutator)))
  
  ; (listof attribute)
  (define attributes
    (list* guid-attribute
           revision-attribute
           (for/list ([index (in-range 2 (+ num-attrs 2))]
                      [name  (in-list attr-names)]
                      [col   (in-list column-names)]
                      [type  (in-list attr-types)])
             (create-attribute name col type entity index struct-accessor struct-mutator))))
  
  ; any ... -> guid
  (define cached-constructor
    (make-cached-constructor
     (symbol-append 'make- name)
     struct-constructor
     (- (length attributes) 2)
     guid-attribute
     revision-attribute))
  
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
  
  #;(add-schema-entity! entity)
  
  (values entity struct-type cached-constructor cached-predicate))

; Helpers ----------------------------------------

; symbol symbol type entity integer (struct natural -> any) (struct natural any -> void) -> attribute 
(define (create-attribute name col type entity index struct-accessor struct-mutator)
  (let* ([private-accessor (make-struct-field-accessor struct-accessor index name)]
         [private-mutator  (make-struct-field-mutator  struct-mutator  index name)]
         [cached-accessor  (make-cached-accessor private-accessor)]
         [cached-mutator   (make-cached-mutator  private-mutator)])
    (make-attribute name col type entity index
                    private-accessor
                    private-mutator
                    cached-accessor
                    cached-mutator)))

; Provide statements -----------------------------

(provide/contract
 [rename create-entity
         make-entity
         (->* (symbol?
               (listof symbol?)
               (listof type?)
               (-> (or/c natural-number/c #f) guid?)
               (-> any/c boolean?))
              (#:table-name symbol?
                            #:column-names (listof symbol?)
                            #:on-save      procedure?
                            #:on-delete    procedure?
                            #:properties   (listof (cons/c (and/c struct-type-property?
                                                                  (not/c (cut eq? <> prop:entity)))
                                                           any/c)))
              (values entity?
                      struct-type?
                      procedure?
                      procedure?))])
