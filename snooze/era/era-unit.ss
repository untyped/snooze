#lang scheme/unit

(require (only-in srfi/1 find)
         srfi/26
         (file "../base.ss")
         (file "era-dummy.ss")
         (file "era-internal.ss")
         (file "era-sig.ss")
         (file "era-struct.ss")
         (file "transaction-sig.ss"))

(import transaction^)

(export era^)

; See era-sig.ss for documentation-style comments.

; persistent-struct ----------------------------

; entity
(define entity:persistent-struct
  (make-entity 'persistent-struct
               'persistent-struct
               #f
               (make-dummy-constructor 'persistent-struct) ; patched below
               (make-dummy-predicate   'persistent-struct) ; patched below
               (make-dummy-accessor    'persistent-struct) ; patched below
               (make-dummy-mutator     'persistent-struct) ; patched below
               null                                        ; patched below
               null                                        ; save pipeline
               null                                        ; insert pipeline
               null                                        ; update pipeline
               null))                                      ; delete pipeline

; struct-type-descriptor
; any ... -> struct
; struct -> boolean
; struct integer -> any
; struct integer any -> void
(define-values (struct:persistent-struct
                make-persistent-struct
                persistent-struct?
                persistent-struct-ref
                persistent-struct-set!)
  (make-struct-type 
   'persistent-struct ; name symbol
   #f                 ; supertype
   2                  ; constructor arity (id and revision included)
   0                  ; number of auto-value fields
   (void)             ; values for auto-value fields
   (list (cons prop:entity entity:persistent-struct)) ; properties
   #f))               ; inspector-or-#f

; Accessors and mutators -------------------------
    
; (persistent-struct integer -> any) integer symbol -> (persistent-struct -> any)
;
; NOTE: This procedure is currently not strictly necessary. It is provided
; firstly for symmetry with make-persistent-struct-field-mutator, and secondly
; because it may become necessary in future updates to Snooze.
(define (make-persistent-struct-field-accessor struct-ref index attribute-name)
  (make-struct-field-accessor struct-ref index attribute-name))

; (persistent-struct integer any -> void) integer symbol -> (persistent-struct any -> void)
(define (make-persistent-struct-field-mutator struct-set! index attribute-name)
  (define set! (make-struct-field-mutator struct-set! index attribute-name))
  (lambda (struct value)
    (store-transaction-backup! struct)
    (set! struct value)))

; persistent-struct -> (U integer #f)
; persistent-struct (U integer #f) -> void
; persistent-struct -> (U integer #f)
; persistent-struct (U integer #f) -> void
(define-values (struct-id set-struct-id! struct-revision set-struct-revision!)
  (values (make-persistent-struct-field-accessor persistent-struct-ref  0 'id)
          (make-persistent-struct-field-mutator  persistent-struct-set! 0 'id)
          (make-persistent-struct-field-accessor persistent-struct-ref  1 'revision)
          (make-persistent-struct-field-mutator  persistent-struct-set! 1 'revision)))

; attribute
(define attr:struct-id
  (make-attribute 'id 
                  'id
                  entity:persistent-struct
                  0
                  struct-id
                  set-struct-id!
                  type:id))

; attribute
(define attr:struct-revision
  (make-attribute 'revision
                  'revision
                  entity:persistent-struct
                  1
                  struct-revision
                  set-struct-revision!
                  type:revision))

; Patch entity:persistent-struct:
(set-entity-struct-type! entity:persistent-struct struct:persistent-struct)
(set-entity-constructor! entity:persistent-struct make-persistent-struct)
(set-entity-predicate!   entity:persistent-struct persistent-struct?)
(set-entity-accessor!    entity:persistent-struct persistent-struct-ref)
(set-entity-mutator!     entity:persistent-struct persistent-struct-set!)
(set-entity-attributes!  entity:persistent-struct (list attr:struct-id attr:struct-revision))

; Struct utilities -----------------------------

; persistent-struct -> guid
(define (struct-guid struct)
  (make-guid (struct-entity struct)
             (struct-id struct)))

; persistent-struct (U symbol attribute) -> any
(define (struct-has-attribute? struct name+attr)
  (entity-has-attribute? (struct-entity struct) name+attr))

; persistent-struct (U symbol attribute) -> any
(define (struct-attribute struct name+attr)
  (define entity
    (struct-entity struct))
  (define attr 
    (if (attribute? name+attr)
        name+attr
        (entity-attribute entity name+attr)))
  (define ref
    (attribute-accessor attr))
  (ref struct))

; persistent-struct -> (listof any)
(define (struct-attributes struct)
  (if (persistent-struct? struct)
      (cdr (vector->list (struct->vector struct)))
      (raise-exn exn:fail:contract
        (format "Expected persistent-struct, received ~s" struct))))

; persistent-struct (U symbol attribute) any -> void
(define (set-struct-attribute! struct name+attr value)
  (define entity (struct-entity struct))
  (define attr 
    (if (attribute? name+attr)
        name+attr
        (entity-attribute entity name+attr)))
  (define set! (attribute-mutator attr))
  (set! struct value))

; persistent-struct (vectorof any) -> void
(define (set-struct-attributes! struct values)
  (define entity (struct-entity struct))
  (define attrs (entity-attributes entity))
  (if (= (length values) (length attrs))
      (for-each (lambda (attr value)
                  ((attribute-mutator attr) struct value))
                attrs
                values)
      (raise-exn exn:fail:snooze
        (format "Expected list of ~a field values, received ~s" (length attrs) values))))

; entity [attr any] ... -> persistent-struct
(define (make-persistent-struct/defaults entity . args)
  ; (listof attribute)
  (define attributes (entity-attributes entity))
  
  ; Check attributes are part of the correct entity:
  (define-values (arg-attrs arg-vals)
    (check-attribute-keywords entity args))
  
  ; persistent-struct
  (apply (entity-constructor entity)
         (map (lambda (attr)
                (attribute-keyword-get attr arg-attrs arg-vals (type-default (attribute-type attr))))
              attributes)))

; persistent-struct [attr any] ... -> persistent-struct
(define (copy-persistent-struct old-struct . args)
  ; entity
  (define entity     (struct-entity old-struct))
  ; (listof attribute)
  (define attributes (entity-attributes entity))
  ; (listof any)
  (define existing   (struct-attributes old-struct))
  
  ; Check attributes are part of the correct entity:
  (define-values (arg-attrs arg-vals)
    (check-attribute-keywords entity args))
  
  ; persistent-struct
  (apply (entity-constructor entity)
         (map (cut attribute-keyword-get <> arg-attrs arg-vals <>)
              attributes
              existing)))

; persistent-struct persistent-struct -> void
(define (update-persistent-struct-from-copy! struct copy)
  ; entity
  (define entity (struct-entity struct))
  
  ; Check that struct and copy are the same type with the same ID:
  (unless (equal? entity (struct-entity copy))
    (raise-exn exn:fail:snooze
      (format "Expected two arguments of the same type, received ~s ~s" struct copy)))
  
  (for-each (lambda (attr)
              (define ref (attribute-accessor attr))
              (define set! (attribute-mutator attr))
              (set! struct (ref copy)))
            (entity-attributes entity)))

; Helpers --------------------------------------

; attribute attribute -> boolean
(define (attribute-name-equal? attr1 attr2)
  (equal? (attribute-name attr1)
          (attribute-name attr2)))

; entity (alternating-listof (U symbol attribute) any) -> (listof attribute) (listof any)
(define (check-attribute-keywords entity args)
  ; boolean (listof (U attribute any)) (listof attribute) (listof any) -> (listof attribute) (listof any)
  (let loop ([even? #f] [args args] [attrs-accum null] [vals-accum null])
    (if even?
        ; Attribute value:
        (if (null? args)
            (raise-exn exn:fail:contract
              (format "No value for ~s" (car attrs-accum)))
            (let ([attr (car attrs-accum)]
                  [val  (car args)])
              (cond [(keyword? val)   (raise-exn exn:fail:contract (format "Keyword arguments are deprecated: ~s" val))]
                    [(attribute? val) (raise-exn exn:fail:contract (format "No value for ~s" attr))]
                    [else             (loop (not even?) (cdr args) attrs-accum (cons val vals-accum))])))
        ; Attribute:
        (if (null? args)
            (values (reverse attrs-accum)
                    (reverse vals-accum))
            (let* ([attr+name (car args)]
                   [attr      (cond [(attribute? attr+name) (entity-attribute entity attr+name)]
                                    [(symbol? attr+name)    (entity-attribute entity attr+name)]
                                    [else                   (raise-exn exn:fail:contract 
                                                              (if (null? attrs-accum)
                                                                  (format "No attribute for value: ~s" attr+name)
                                                                  (format "Multiple values for ~s" (car attrs-accum))))])])
              (if (find (cut attribute-name-equal? attr <>) attrs-accum)
                  (raise-exn exn:fail:contract 
                    (format "Attribute specified more than once: ~s" attr))
                  (loop (not even?) (cdr args) (cons attr attrs-accum) vals-accum)))))))


; attribute (listof attribute) (listof any) any -> any
;
; We search for attributes by name so we don't have to worry that, for example,
; attr:struct-id and attr:person-id are not equal. Note that we are reliant on
; check-attribute-keywords to chuck out attributes from other entities, otherwise
; we might get confused between, for example, attr:person-name and attr:pet-name.
(define (attribute-keyword-get needle attrs vals default)
  (let/ec return
    (for ([attr attrs] [val vals])
      (when (eq? needle attr)
        (return val)))
    default))
