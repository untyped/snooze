(module persistent-struct mzscheme
  
  (require-for-syntax
   (lib "list.ss" "srfi" "1")
   (lib "cut.ss" "srfi" "26"))
  
  (require-for-syntax
   (planet "debug.ss" ("untyped" "unlib.plt" 2))
   (planet "syntax.ss" ("untyped" "unlib.plt" 2)))
  
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "struct.ss")
           (all-except (lib "list.ss" "srfi" "1") any))
  
  (require (file "base.ss")
           (file "era.ss")
           (file "transaction.ss")
           (file "type.ss"))
  
  (provide define-persistent-struct
           provide-persistent-struct
           make-persistent-struct
           copy-persistent-struct)
  
  ;; make-persistent-struct
  ;;   : symbol
  ;;     (list-of (cons symbol type))
  ;;     [ (list-of (cons (U 'on-save 'on-insert 'on-update 'on-delete)
  ;;                      (connection persistent-struct -> void))) ]
  ;;     [ (list-of (cons property any)) ]
  ;;  -> (values struct-type
  ;;             (any ... -> persistent-struct)
  ;;             entity
  ;;             (any -> boolean)
  ;;             (persistent-struct integer -> any)
  ;;             (persistent-struct integer any -> void))
  (define make-persistent-struct
    (case-lambda 
      ;; No pipelines or properties.
      [(name field/type-pairs)
       (make-persistent-struct name field/type-pairs null null)]
      ;; Pipelines but no properties.
      [(name field/type-pairs pipelines)
       (make-persistent-struct name field/type-pairs pipelines null)]
      ;; The most general form: pipelines and properties
      [(name field/type-pairs pipelines properties)
       
       ; Check to make sure prop:entity isn't already on the property list
       (when (member prop:entity (map car properties))
         (raise-exn exn:fail:snooze
           (format "You cannot specify prop:entity in the properties argument: ~a" properties)))
       
       (let*-values
           (; The entity and persistent struct type are mutually dependent: we have to define
            ; one of them before the other, and we won't have meaningful values before we have
            ; defined both. We define the entity first, fill it with rubbish, and patch it with
            ; sensible values in the code below.
            [(the-entity)
             (make-entity name 
                          (make-dummy-constructor name) ; this dummy value satisfies the contract on make-entity
                          (make-dummy-getter name)      ; this dummy value satisfies the contract on make-entity
                          (make-dummy-setter name)      ; this dummy value satisfies the contract on make-entity
                          null   ; fields
                          null   ; save pipeline
                          null   ; insert pipeline
                          null   ; update pipeline
                          null)] ; delete pipeline
           
            ; [MCJ] Personally, I think 'id' should get burried in a parent structure;
            ; instead, it is secretly inserted into the data strcutre that 
            ; the user is creating. I suppose it is six of one 
            ; and a half-dozen of another.
            [(type constructor predicate accessor mutator)
             (make-struct-type
              name                                ; name symbol
              #f                                  ; supertype
              (+ (length field/type-pairs) 2)     ; number of fields passed in constructor (includes id and revision)
              0                                   ; number of auto-value fields
              (void)                              ; values for auto-value fields
              (cons (cons prop:entity the-entity) ; properties
                    properties)                    
              #f)]                                ; inspector-or-#f
            
            ; The structure type (we need this for structure-esque macros like:
            ;     (match a-record [(struct record (id revision ...))] ...)
            [(the-struct-type) type]
            
            ; We'll want to provide 'the-constructor' as part of the
            ; definitions we're binding.
            [(the-constructor)
             (lambda field-values
               (if (= (length field-values)
                      (length field/type-pairs))
                   (apply constructor (cons* #f #f field-values)) ; add id and revision to arguments
                   (raise-exn exn:fail:contract:arity
                     (format "Expected ~a arguments, received ~a" (length field/type-pairs) field-values))))]
         
            ; Provide the predicate created by make-struct-type directly.
            ; Included here only for consistency.
            [(the-predicate) 
             predicate]
         
            ; This list of pairs is used in the accessor and mutator functions.
            ; There is probably a better way, but this isn't a big deal.
            [(indexed-fields)
             (map (lambda (field index)
                    (list field index))
                  (map car (cons* '(id id) '(revision revision) field/type-pairs))
                  (iota (+ (length field/type-pairs) 2) 0))]
         
            ; We provide one accessor instead of many. This accessor
            ; consumes a structure and a symbol for a field name. 
            ; It loooks up the field index, and returns the correct 
            ; value.
            [(the-accessor)
             (lambda (s field)
               (let ([position (cadr (assoc field indexed-fields))])
                 (apply (make-struct-field-accessor accessor position field)
                        (list s))))]
         
            ; We provide one mutator; like the accessor, except it 
            ; takes two arguments, a structure, field name, and a value.
            [(the-mutator)
             (lambda (s field value)
               (let ([position (cadr (assoc field indexed-fields))])
                 (if (and (in-transaction?)
                          (roll-back-persistent-structs?))
                     (let ([old-value (the-accessor s field)])
                       (begin0 (apply (make-struct-field-mutator mutator position field)
                                      (list s value))
                               (record-delta! s field old-value)))
                     (apply (make-struct-field-mutator mutator position field)
                            (list s value)))))]
            
            ; Sneakily add id and revision to the list of attributes, and convert
            ; the (list-of (list id type)) value passed to this procedure to a
            ; list of attribute structures.
            [(attribute-fields)
             (cons* (make-attribute 'id type:id)
                    (make-attribute 'revision type:revision)
                    (map make-attribute
                         (map car  field/type-pairs)
                         (map cadr field/type-pairs)))])
         
         ; Here we're patching the entity. We create the attribute 
         ; fields for the entity, and patch its attributes and pipelines
         ; If there are no pipelines, the for-each is effectively skipped.
         
         (set-entity-constructor! the-entity constructor)
         (set-entity-getter!      the-entity accessor)
         (set-entity-setter!      the-entity mutator)
         (set-entity-fields!      the-entity attribute-fields)
         
         (for-each (match-lambda
                     [(list 'save value)
                      (set-entity-save-pipeline!   the-entity value)]
                     [(list 'insert value)
                      (set-entity-insert-pipeline! the-entity value)]
                     [(list 'update value)
                      (set-entity-update-pipeline! the-entity value)]
                     [(list 'delete value)
                      (set-entity-delete-pipeline! the-entity value)]
                     [(list key value)
                      (raise-exn exn:fail:snooze
                        (format "Unrecognised pipeline ~a: \"~a\": should be one of: save, insert, update or delete."
                                (entity-name the-entity)
                                key))])
                   pipelines)
         
         ;; Return multiple values; the user will need to set up
         ;; an appropriate 'define-values' statement to use this
         ;; procedure, anyway. However, the syntactic 
         ;; 'define-persistent-struct' is provided for consistency.
         (values the-struct-type
                 the-constructor
                 the-entity
                 the-predicate
                 the-accessor
                 the-mutator))]))
  
  ; Syntactic forms ------------------------------
  
  ;; This begin-for-syntax block would/will eventually move into
  ;; a module that is require-for-syntaxed in.
  (begin-for-syntax
    ;; I didn't touch this function at all.
    (define (field-names->names stx before after name field-names)
      (map (lambda (field-name)
             (make-syntax-symbol stx before name '- field-name after))
           field-names))
    
    ;; This generates the definitions for the accessors.
    (define (def-accessor-names the-accessor accessor-names field-names)
      (map (lambda (accessor-name position fname)
             #`(define #,accessor-name
                 (lambda (s)
                   (#,the-accessor s (quote #,fname)))))
           accessor-names
           ;; Skip the ID and revision at positions 0 and 1 in the struct:
           (iota (length accessor-names) 2)
           field-names))
    
    (define (def-mutator-names the-mutator mutator-names field-names)
      (map (lambda (mutator-name position fname)
             #`(define #,mutator-name
                 (lambda (s v)
                   (#,the-mutator s 
                      (quote #,fname) 
                      v))))
           mutator-names
           ;; Skip the ID and revision at positions 0 and 1 in the struct:
           (iota (length mutator-names) 2)
           field-names))
    
    ) ;; end begin-for-syntax
  
  (define-syntax (define-persistent-struct stx)
    (syntax-case stx ()
      ; Fields only:
      [(_ name
          ([field-name field-type] ...))
       #'(_ name
            ([field-name field-type] ...) 
            ()
            ())]
      ; Fields and pipelines:
      [(_ name 
          ([field-name field-type] ...)
          ([pipeline-name pipeline] ...))
       #'(_ name 
            ([field-name field-type] ...)
            ([pipeline-name pipeline] ...)
            ())]
      ; Fields, pipelines and properties:
      [(_ name 
          ([field-name field-type] ...)
          ([pipeline-name pipeline] ...)
          ([property property-value] ...))
       (let* ([field-names          (syntax->list #'(field-name ...))]
              [field-types          (syntax->list #'(field-type ...))]
              [field-accessors (field-names->names #'name '|| '|| #'name field-names)]
              [field-mutators  (field-names->names #'name 'set- '! #'name field-names)])
         (with-syntax ([struct-type         (make-syntax-symbol #'name 'struct: #'name)]
                       [constructor         (make-syntax-symbol #'name 'make- #'name)]
                       [predicate           (make-syntax-symbol #'name #'name '?)]
                       [entity              (make-syntax-symbol #'name 'entity: #'name)]
                       [accessor            (make-syntax-symbol #'name #'name '-ref)]
                       [mutator             (make-syntax-symbol #'name #'name '-set!)]
                       [id-accessor         (make-syntax-symbol #'name #'name '-id)]
                       [revision-accessor   (make-syntax-symbol #'name #'name '-revision)])
           ;; Swap this 'begin' with a 'quote' to see what is
           ;; going on in the macro.
           #`(begin (begin (define-values (struct-type constructor entity predicate accessor mutator)
                             (make-persistent-struct 'name
                                                     (list (list 'field-name field-type) ...)
                                                     (list (list 'pipeline-name pipeline) ...)
                                                     (list (cons property property-value) ...)))
                           (define id-accessor get-id)
                           (define revision-accessor get-revision)
                           #,@(def-accessor-names #'accessor field-accessors field-names)
                           #,@(def-mutator-names #'mutator field-mutators field-names)
                           ; Included to make things like the (struct ...) clause in plt-match work.
                           ; Copied by-example from an expanded define-struct.
                           ; The syntax-quotes-within-syntax-quotes are intensional.
                           (define-syntaxes (name)
                             (let ([certify (syntax-local-certifier #t)])
                               (list-immutable #'struct-type
                                               #'constructor
                                               #'predicate
                                               (list-immutable #,@(map (lambda (accessor)
                                                                         (with-syntax ([accessor accessor])
                                                                           #`(certify #'accessor)))
                                                                       (reverse field-accessors)))
                                               (list-immutable #,@(map (lambda (mutator)
                                                                         (with-syntax ([mutator mutator])
                                                                           #`(certify #'mutator)))
                                                                       (reverse field-mutators)))
                                               #t)))))))]))
  
  ;; syntax provide-persistent-struct : identifier (identifier type) ...
  (define-syntax (provide-persistent-struct stx)
    (define (field-names->names before after name field-names)
      (map (lambda (field-name)
             (make-syntax-symbol stx before name '- field-name after))
           (syntax-object->datum field-names)))
    (syntax-case stx ()
      [(_ name ((field-name field-type) ...))
       (with-syntax
           ([struct-type-name          (make-syntax-symbol stx 'struct: #'name)]
            [constructor-name          (make-syntax-symbol stx 'make- #'name)]
            [predicate-name            (make-syntax-symbol stx #'name '?)]
            [entity-name               (make-syntax-symbol stx 'entity: #'name)]
            [id-accessor-name          (make-syntax-symbol stx #'name '-id)]
            [revision-accessor-name    (make-syntax-symbol stx #'name '-revision)]
            [(field-accessor-name ...) (field-names->names '|| '|| #'name #'(field-name ...))]
            [(field-mutator-name ...)  (field-names->names 'set- '! #'name #'(field-name ...))])
         #'(provide name
                    struct-type-name
                    constructor-name
                    entity-name
                    predicate-name
                    id-accessor-name
                    revision-accessor-name
                    field-accessor-name ...
                    field-mutator-name ...))]))
  
  ;; syntax (copy-persistent-struct persistent-struct-type
  ;;                                persistent-struct
  ;;                                (identifier any)
  ;;                                ...) 
  ;;     -> persistent-struct
  (define-syntax (copy-persistent-struct stx)
    (syntax-case stx ()
      [(_ struct-type struct [name value] ...)
       #'(let ([old struct]
               [new (copy-struct struct-type struct [name value] ...)])
           (set-id! new (get-id old))
           (set-revision! new (get-revision old))
           new)]))
  
  ; Helpers --------------------------------------
  
  ;; make-dummy-constructor : symbol -> procedure
  (define (make-dummy-constructor struct-name)
    (lambda args
      (raise-exn exn:fail:snooze
        "make-persistent-struct didn't overwrite the dummy constructor in ~a" struct-name)))
  
  ;; make-dummy-getter : symbol -> procedure
  (define (make-dummy-getter struct-name)
    (lambda args
      (raise-exn exn:fail:snooze
        "make-persistent-struct didn't overwrite the dummy getter in ~a" struct-name)))
  
  ;; make-dummy-setter : symbol -> procedure
  (define (make-dummy-setter struct-name)
    (lambda args
      (raise-exn exn:fail:snooze
        "make-persistent-struct didn't overwrite the dummy setter in ~a" struct-name)))
  
  )
 