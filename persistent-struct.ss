#lang scheme/base

(require mzlib/etc
         scheme/contract
         scheme/match
         (only-in srfi/1/list iota)
         srfi/26/cut
         (planet untyped/unlib:3/pipeline)
         "base.ss"
         "schema.ss"
         "era/era.ss"
         "era/era-dummy.ss")

;   symbol
;   (listof symbol)
;   (listof type)
;   [#:table-name symbol]
;   [#:column-names (listof symbol)]
;   [#:on-save pipeline]
;   [#:on-insert pipeline]
;   [#:on-update pipeline]
;   [#:on-delete pipeline]
;   [#:properties (alistof property any)]
; -> 
;   entity
;   struct-type
;   (any ... -> persistent-struct)
;   (any -> boolean)
(define (make-persistent-struct-type 
         name 
         attr-names
         attr-types
         #:table-name [table-name name]
         #:column-names [column-names attr-names]
         #:on-save [save-pipeline null]
         #:on-insert [insert-pipeline null]
         #:on-update [update-pipeline null]
         #:on-delete [delete-pipeline null]
         #:properties [properties null])
  
  ; integer
  ;
  ; Make sure attr-names, attr-types and column-names are all the same length,
  ; and bind that length to a variable.
  (define num-attrs
    (let ([num-attrs (length attr-names)])
      (cond [(not (= num-attrs (length attr-types)))
             (raise-exn exn:fail:contract (format "Expected ~a attribute types, received: ~s" num-attrs attr-types))]
            [(not (= num-attrs (length column-names)))
             (raise-exn exn:fail:contract (format "Expected ~a attribute DB names, received: ~s" num-attrs column-names))]
            [else num-attrs])))
  
  ; entity
  ;
  ; The entity and persistent struct type are mutually dependent: we have to define
  ; one of them before the other, and we won't have meaningful values before we have
  ; defined both. We define the entity first, fill it with rubbish, and patch it with
  ; sensible values in the code below.
  (define entity
    (make-entity name 
                 table-name
                 #f                            ; patched below
                 (make-dummy-constructor name) ; patched below
                 (make-dummy-predicate name)   ; patched below
                 (make-dummy-accessor name)    ; patched below
                 (make-dummy-mutator name)     ; patched below
                 null                          ; patched below
                 save-pipeline
                 insert-pipeline
                 update-pipeline
                 delete-pipeline))
  
  ; struct-type-descriptor
  ; any ... -> struct
  ; struct -> boolean
  ; struct integer -> any
  ; struct integer any -> void
  (define-values (struct-type private-constructor predicate private-accessor private-mutator)
    (make-struct-type 
     name                       ; name symbol
     struct:persistent-struct   ; supertype
     num-attrs                  ; number of fields passed in constructor (excludes fields from supertype)
     0                          ; number of auto-value fields
     (void)                     ; values for auto-value fields
     (cons (cons prop:entity entity) properties) ; properties
     #f))                       ; inspector-or-#f
  
  ; any ... -> struct
  (define (constructor . args)
    (if (= (length args) num-attrs)
        (apply private-constructor
               (type-default type:id) 
               (type-default type:revision)
               args)
        (raise-exn exn:fail:contract:arity
          (format "Expected ~a arguments, received ~a" num-attrs args))))
  
  ; (listof attribute)
  (define attributes
    (append (map (lambda (attr)
                   (make-attribute (attribute-name attr)
                                   (attribute-column-name attr)
                                   entity
                                   (attribute-index attr)
                                   (attribute-accessor attr)
                                   (attribute-mutator attr)
                                   (attribute-type attr)))
                 (entity-attributes entity:persistent-struct))
            (map (lambda (index attr-name column-name type)
                   (make-attribute attr-name
                                   column-name
                                   entity
                                   (+ index (length (entity-attributes entity:persistent-struct)))
                                   (make-persistent-struct-field-accessor private-accessor index attr-name)
                                   (make-persistent-struct-field-mutator private-mutator index attr-name)
                                   type))
                 (iota num-attrs)
                 attr-names
                 column-names
                 attr-types)))
  
  ; Patch the entity:
  (set-entity-struct-type! entity struct-type)
  (set-entity-constructor! entity private-constructor)
  (set-entity-predicate!   entity predicate)
  (set-entity-accessor!    entity private-accessor)
  (set-entity-mutator!     entity private-mutator)
  (set-entity-attributes!  entity attributes)
  
  (add-schema-entity! entity)
  
  (values entity struct-type constructor predicate))

; Contracts --------------------------------------

; contract
(define pipeline-name/c
  (symbols 'save 'insert 'update 'delete))

; contract
(define pipeline/c
  (listof stage?))

; contract
(define prop:entity/c
  (flat-named-contract 'prop:entity/c (cut eq? <> prop:entity)))

; contract
(define not-prop:entity/c
  (and/c struct-type-property? (not/c prop:entity/c)))

; Provide statements -----------------------------

(provide/contract
 [make-persistent-struct-type
  (->* (symbol? (listof symbol?) (listof type?))
       (#:table-name symbol?
        #:column-names (listof symbol?)
        #:on-save (listof stage?)
        #:on-insert (listof stage?)
        #:on-update (listof stage?)
        #:on-delete (listof stage?)
        #:properties (listof (cons/c not-prop:entity/c any/c)))
       (values entity?
               struct-type?
               procedure?
               procedure?))])
