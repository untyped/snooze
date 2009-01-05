(module era-private mzscheme
  
  (require (lib "contract.ss")
           (all-except (lib "list.ss" "srfi" "1") any))
  
  (require (planet "hash-table.ss" ("untyped" "unlib.plt" 2))
           (planet "list.ss" ("untyped" "unlib.plt" 2))
           (planet "pipeline.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "base.ss")
           (file "type.ss"))
  
  ; Structures and properties --------------------
  
  ;; prop:entity
  ;;
  ;; Property attached to persistent structs, that describes
  ;; the layout of the databse tables used to hold persistent
  ;; information. It is automatically created by
  ;; (define-persistent-struct) wouldn't normally be directly
  ;; accessed by the programmer.
  ;;
  ;; has-entity? : any -> boolean
  ;;
  ;; Returns #t if the argument is a persistent struct with
  ;; attached "entity" metadata.
  ;;
  ;; struct-entity : any -> (U entity #f) | exn:fail:contract
  ;;
  ;; Returns the "entity" object associated with a persistent
  ;; struct. This is wrapped in a non-starred version below that
  ;; raises exn:fail:snooze instead of exn:fail:contract.
  (define-values
    (prop:entity has-entity? struct-entity)
    (make-struct-type-property 'entity))
  
  ;; persistent-struct? : any -> boolean
  ;;
  ;; Whether or not something is a persistent struct depends purely
  ;; on whether or not it is bound to an entity.
  (define persistent-struct? has-entity?)
    
  ;; struct entity 
  ;;     : symbol 
  ;;       constructor 
  ;;       getter 
  ;;       setter
  ;;       (list-of (U attribute relation))
  ;;       (list-of (conn struct -> integer))
  ;;       (list-of (conn struct -> integer))
  ;;       (list-of (conn struct -> integer))
  ;;       (list-of (conn struct -> integer))
  (define-struct entity
    (name constructor getter setter fields save-pipeline insert-pipeline update-pipeline delete-pipeline)
    #f)

  ;; struct attribute : symbol symbol
  (define-struct attribute
    (name type)
    #f)
  
  ; Procedures -----------------------------------
  
  ;; add-entity-field! : entity field -> void
  (define (add-entity-field! entity field)
    (set-entity-fields!
     entity
     (append
      (entity-fields entity)
      (list field))))
  
  ;; get-id : persistent-struct -> (U integer #f)
  (define (get-id struct)
    (let* ((entity (struct-entity struct))
           (getter (entity-getter entity)))
      (getter struct 0)))
  
  ;; set-id! : persistent-struct (U integer #f) -> void
  (define (set-id! struct id)
    (let* ((entity (struct-entity struct))
           (setter (entity-setter entity)))
      (setter struct 0 id)))
  
  ;; get-revision : persistent-struct -> (U integer #f)
  (define (get-revision struct)
    (let* ((entity (struct-entity struct))
           (getter (entity-getter entity)))
      (getter struct 1)))
  
  ;; set-revision!/internal : persistent-struct (U integer #f) -> void
  ;;
  ;; Sets the revision number on this struct. Use set-revision! from
  ;; transaction.ss if you want to set the revision number and remember
  ;; the old value for transaction rollback (this bit of hackery is to
  ;; get around a cyclic dependency between era.ss and transaction.ss).
  (define (set-revision!/internal struct id)
    (let* ((entity (struct-entity struct))
           (setter (entity-setter entity)))
      (setter struct 1 id)))
  
  ;; call-with-entity : persistent-struct (entity -> any) -> any
  (define (call-with-entity struct body)
    (body (struct-entity struct)))
  
  ;; has-attribute? : entity symbol -> boolean
  (define (has-attribute? entity name)
    (if (find (lambda (attr)
                (eq? (attribute-name attr) name))
              (entity-fields entity))
        #t
        #f))
  
  ;; get-attribute : entity symbol -> attribute
  (define (get-attribute entity name)
    (let ((attrs (entity-fields entity)))
      (let ([attr 
             (find (lambda (attr)
                     (eq? (attribute-name attr) name))
                   attrs)])
        (if attr
            attr
            (raise-exn exn:fail:snooze
              (format "Could not find attribute \"~a\" in entity ~a" name entity))))))
  
  ;; get-attribute-index : entity symbol -> integer
  ;;
  ;; Returns the index of the specified attribute as it appears in the
  ;; persistent struct:
  ;;   - the id is at index 0
  ;;   - other attributes are arranged from index 1 upwards
  (define (get-attribute-index entity name)
    (let loop ([index 0] ; Skip ID
               [attrs (entity-fields entity)]) ; Skip ID
      (cond
        [(null? attrs)
         (raise-exn exn:fail:snooze
           (format "Could not find attribute \"~a\" in entity ~a" name entity))]
        [(eq? name (attribute-name (car attrs))) index]
        [else (loop (add1 index) (cdr attrs))])))
  
  ;; get-attribute-value : persistent-struct symbol -> any
  (define (get-attribute-value struct name)
    (call-with-entity struct
      (lambda (entity)
        (let loop ([names (map attribute-name (entity-fields entity))]
                   [vals (cdr (vector->list (struct->vector struct)))])
          (cond [(null? names)
                 (raise-exn exn:fail:snooze
                   (format "Could not find attribute \"~a\" in struct ~a" name struct))]
                [(eq? name (car names))
                 (car vals)]
                [else (loop (cdr names) (cdr vals))])))))
  
  ;; set-attribute-value! persistent-struct symbol any -> nothing
  (define (set-attribute-value! struct name value)
    (call-with-entity struct
      (lambda (entity)
        (let ([index (get-attribute-index entity name)])
          ((entity-setter entity) struct index value)))))
  
  ;; map-attributes/entity : (symbol type -> any) entity -> (list-of any)
  ;;
  ;; Maps the supplied procedure over all attributes in the entity except the
  ;; ID and revision attributes.
  (define (map-attributes/entity body entity)
    (map 
     body 
     (cddr (map attribute-name (entity-fields entity)))   ; cddr skips id and revision
     (cddr (map attribute-type (entity-fields entity))))) ; cddr skips id and revision
  
  ;; map-attributes/struct : (symbol type any -> any) persistent-struct -> (list-of any)
  ;;
  ;; Maps the supplied procedure over all attributes in the persistent struct except the
  ;; ID and revision attributes.
  (define (map-attributes/struct body struct)
    (call-with-entity struct
      (lambda (entity)
        (map 
         body 
         (cddr (map attribute-name (entity-fields entity))) ; cddr skips id and revision
         (cddr (map attribute-type (entity-fields entity))) ; cddr skips id and revision
         (cdddr (vector->list (struct->vector struct))))))) ; cdddr skips structure identifier, id and revision
  
  ;; for-each-attribute/entity : (symbol type -> any) entity -> nothing
  ;;
  ;; Applies the supplied procedure to all attributes in the entity except the
  ;; ID and revision attributes.
  (define (for-each-attribute/entity body entity)
    (for-each 
     body 
     (cddr (map attribute-name (entity-fields entity)))   ; cddr skips id and revision
     (cddr (map attribute-type (entity-fields entity))))) ; cddr skips id and revision
  
  ;; for-each-attribute/struct : (symbol type any -> any) persistent-struct -> nothing
  ;;
  ;; Applies the supplied procedure to all attributes in the entity except the
  ;; ID and revision attributes.
  (define (for-each-attribute/struct body struct)
    (call-with-entity struct
      (lambda (entity)
        (for-each 
         body 
         (cddr (map attribute-name (entity-fields entity))) ; cddr skips id and revision
         (cddr (map attribute-type (entity-fields entity))) ; cddr skips id and revision
         (cdddr (vector->list (struct->vector struct))))))) ; cdddr skips structure identifier, id and revision
  
  ;; persistent-struct->alist : persistent-struct -> (list-of (cons symbol any))
  ;;
  ;; Creates an association list containing all attributes of the supplied structure,
  ;; including id and revision.
  (define (persistent-struct->alist struct)
    (cons* (cons 'id (get-id struct))
           (cons 'revision (get-revision struct))
           (map-attributes/struct
            (lambda (id type value)
              (cons id value))
            struct)))
  
  ;; persistent-struct->hash-table : persistent-struct -> hash-table
  ;;
  ;; Creates a hash table containing all attributes of the supplied structure,
  ;; including id and revision.
  (define (persistent-struct->hash-table struct)
    (let ([table (make-hash-table)])
      (hash-table-put! table 'id (get-id struct))
      (hash-table-put! table 'revision (get-revision struct))
      (for-each-attribute/struct
       (lambda (id type value)
         (hash-table-put! table id value))
       struct)
      table))
  
  ;; make-blank-persistent-struct : entity -> persistent-struct
  ;;
  ;; Creates a blank persistent struct with #f id, #f revision and default
  ;; values for all data attributes.
  (define (make-blank-persistent-struct entity)
    (apply
     (entity-constructor entity)
     (cons*
      (type-null type:id)
      (type-null type:revision)
      (map-attributes/entity
       (lambda (name type)
         (type-null type))
       entity))))
  
  ;; hash-table->persistent-struct : entity hash-table -> persistent-struct
  ;;
  ;; Creates a new persistent-struct, populating it with attribute values from
  ;; the supplied hash table. The hash table is checked for each argument in the
  ;; structure constructor: if there is a key 'attribute-name in the table, the
  ;; associated value is passed to the constructor as the value of that attribute.
  ;; Otherwise, the relevant default attribute value is passed instead.
  ;;
  ;; Note that, as this procedure maps through the constructor arguments, IDs
  ;; and revisions are *not* copied from the hash table.
  (define (hash-table->persistent-struct entity table)
    (apply
     (entity-constructor entity)
     (cons* 
      #f
      #f
      (map-attributes/entity
       (lambda (name type)
         (hash-table-get/default table name (type-null type)))
       entity))))
  
  ;; set-attributes/alist! : persistent-struct (alist-of symbol any) -> void
  ;;
  ;; Updates a persistent struct with values from an alist. The procedure 
  ;; iterates through the data attributes in the structure: if there is a key
  ;; 'attribute-name in the list, the associated value is copied into the 
  ;; structure.
  ;;
  ;; Note that, this procedure does *not* update the id or revision attributes
  ;; in the structure.
  (define (set-attributes/alist! struct fields)
    (call-with-entity struct
      (lambda (entity)
        (alist-for-each
         (lambda (name value)
           (unless (or (eq? name 'id) 
                       (eq? name 'revision)
                       (not (has-attribute? entity name)))
             (set-attribute-value! struct name value)))
         fields))))
  
  ;; set-attributes/hash-table! : persistent-struct hash-table -> void
  ;;
  ;; Updates a persistent struct with values from a hash table. The procedure 
  ;; iterates through the data attributes in the structure: if there is a key
  ;; 'attribute-name in the table, the associated value is copied into the 
  ;; structure.
  ;;
  ;; Note that, this procedure does *not* update the id or revision attributes
  ;; in the structure.
  (define (set-attributes/hash-table! struct table)
    (call-with-entity struct
      (lambda (entity)
        (hash-table-for-each
         table
         (lambda (name value)
           (unless (or (eq? name 'id) 
                       (eq? name 'revision)
                       (not (has-attribute? entity name)))
             (set-attribute-value! struct name value)))))))

  ; Provide statements ---------------------------
  
  (provide prop:entity
           has-entity?
           persistent-struct?
           set-revision!/internal)
  
  (provide/contract
   [struct-entity                 (-> persistent-struct? entity?)]
   [struct entity                 ([name symbol?]
                                   [constructor     procedure?]
                                   [getter          procedure?]
                                   [setter          procedure?]
                                   [fields          (listof attribute?)]
                                   [save-pipeline   (listof stage?)]
                                   [insert-pipeline (listof stage?)]
                                   [update-pipeline (listof stage?)]
                                   [delete-pipeline (listof stage?)])]
   [struct attribute              ([name symbol?]
                                   [type type?])]
   [get-id                        (-> persistent-struct? (or/c integer? false/c))]
   [set-id!                       (-> persistent-struct? (or/c integer? false/c) void?)]
   [get-revision                  (-> persistent-struct? (or/c integer? false/c))]
   [call-with-entity              (-> persistent-struct? (-> entity? any/c) any/c)]
   [has-attribute?                (-> entity? symbol? boolean?)]
   [get-attribute                 (-> entity? symbol? attribute?)]
   [get-attribute-index           (-> entity? symbol? integer?)]
   [get-attribute-value           (-> persistent-struct? symbol? any/c)]
   [set-attribute-value!          (-> persistent-struct? symbol? any/c void?)]
   [map-attributes/entity         (-> (-> symbol? type? any/c) entity? list?)]
   [map-attributes/struct         (-> (-> symbol? type? any/c any/c) persistent-struct? list?)]
   [for-each-attribute/entity     (-> (-> symbol? type? any/c) entity? any/c)]
   [for-each-attribute/struct     (-> (-> symbol? type? any/c any/c) persistent-struct? any/c)]
   [persistent-struct->alist      (-> persistent-struct? (listof (cons/c symbol? any/c)))]
   [persistent-struct->hash-table (-> persistent-struct? hash-table?)]
   [make-blank-persistent-struct  (-> entity? persistent-struct?)]
   [hash-table->persistent-struct (-> entity? hash-table? persistent-struct?)]
   [set-attributes/hash-table!    (-> persistent-struct? hash-table? void?)]
   [set-attributes/alist!         (-> persistent-struct? (listof (cons/c symbol? any/c)) void?)])
    
  )
 