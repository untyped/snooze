#lang mzscheme

(require (for-syntax scheme/match
                     scheme/pretty
                     scheme/provide-transform
                     scheme/struct-info
                     (only srfi/1/list append-map)
                     srfi/26/cut
                     (planet untyped/unlib:3/debug)
                     (planet untyped/unlib:3/syntax)
                     "persistent-struct-info.ss"
                     "era/era.ss"
                     "sql/sql-syntax-util.ss")
         mzlib/kw
         (only scheme/base keyword-apply)
         scheme/serialize
         (only srfi/1/list iota filter)
         "base.ss"
         "persistent-struct.ss"
         "persistent-struct-util.ss"
         "era/era.ss")

; Helpers ----------------------------------------

; syntax symbol symbol syntax syntax -> (list-of syntax)
(define-for-syntax (make-attribute-ids stx before after entity-id attr-ids)
  (list* (make-id stx before entity-id '-id after)
         (make-id stx before entity-id '-revision after)
         (map (cut make-id stx before entity-id '- <> after)
              (syntax->list attr-ids))))

; Syntax -----------------------------------------

(define-syntax (define-persistent-struct stx)
  
  ; The following are accumulated from the syntactic form.
  ; Lists are accumulated in reverse order and re-reversed in "finish".
  
  ; Identifiers to be bound to entity-specific procedures and values:
  (define name-stx #f)                 ; person
  (define id-stx #f)                   ; entity:person
  (define struct-type-stx #f)          ; struct:person
  (define constructor-stx #f)          ; make-person
  (define predicate-stx #f)            ; person?
  (define constructor/defaults-stx #f) ; make-person/defaults
  (define copy-struct-stx #f)          ; copy-person
  (define deserialize-info-stx #f)     ; deserialize-info:person
  
  ; Lists of arguments for make-persistent-struct (reverse order):
  (define property-stxs null)          ; (... (cons prop:bar bar) (cons prop:foo foo))
  (define entity-arg-stxs null)        ; (... bar #:bar foo #:foo)
  
  ; Lists of identifiers to be bound to attribute-specific procedures and values (reverse order):
  (define attr-name-stxs null)         ; (gender age name)
  (define attr-id-stxs null)           ; (attr:person-gender attr:person-age attr:person-name attr:person-revision attr:person-id)
  (define attr-kw-stxs null)           ; (#:person-gender #:person-age #:person-name #:person-revision #:person-id)
  (define column-name-stxs null)       ; ('gender 'age 'name)
  (define accessor-stxs null)          ; (person-gender person-age person-name person-revision person-id)
  (define mutator-stxs null)           ; (set-person-gender! set-person-age! set-person-name! set-person-revision! set-person-id!)
  
  ; Other attribute-specific syntaxes (reverse order):
  (define attr-type-stxs null)         ; (type:symbol type:integer type:string)
  (define attr-kws null)               ; (list '#:gender '#:age '#:name)
  
  ; Parsing entity information:
  
  (define (parse-id+attrs stx)
    (syntax-case stx ()
      [(name attrs kw-arg ...)
       (begin (set! name-stx                 #'name)
              (set! id-stx                   (make-id #'name 'entity: #'name))
              (set! struct-type-stx          (make-id #'name 'struct: #'name))
              (set! constructor-stx          (make-id #'name 'make- #'name))
              (set! predicate-stx            (make-id #'name #'name '?))
              (set! constructor/defaults-stx (make-id #'name 'make- #'name '/defaults))
              (set! copy-struct-stx          (make-id #'name 'copy- #'name))
              (set! deserialize-info-stx     (make-id #'name 'deserialize-info: #'name '-v0))
              (set! attr-id-stxs             (list (make-id #'name 'attr: #'name '-revision)
                                                   (make-id #'name 'attr: #'name '-id)))
              (set! attr-kw-stxs             (list #`(quote #,(datum->syntax-object #f (string->keyword "revision")))
                                                   #`(quote #,(datum->syntax-object #f (string->keyword "id")))))
              (set! accessor-stxs            (list (make-id #'name #'name '-revision)
                                                   (make-id #'name #'name '-id)))
              (set! mutator-stxs             (list (make-id #'name 'set- #'name '-revision!)
                                                   (make-id #'name 'set- #'name '-id!)))
              (for-each parse-attr (syntax->list #'attrs))
              (parse-entity-kws #'(kw-arg ...)))]))
  
  (define (parse-attr stx)
    
    (define (parse-attr-kws stx)
      (syntax-case stx ()
        [(kw other ...) (parse-attr-kw #'kw #'(other ...))]
        [_              (finish-attr)]))
    
    (define (parse-attr-kw kw-stx other-stx)
      (match (syntax-object->datum kw-stx)
        ['#:column-name
         (syntax-case other-stx ()
           [(val other ...)
            (begin (set! column-name-stxs (cons #'val column-name-stxs))
                   (parse-attr-kws #'(other ...)))])]))
    
    (define (finish-attr)
      ; If no column-name was specified, substitute in a default:
      (when (< (length column-name-stxs) (length attr-name-stxs))
        (set! column-name-stxs (cons #`(quote #,(car attr-name-stxs)) column-name-stxs))))
    
    (syntax-case stx ()
      [(name type arg ...)
       (begin (set! attr-name-stxs (cons #'name attr-name-stxs))
              (set! attr-id-stxs   (cons (make-id name-stx 'attr: name-stx '- #'name) attr-id-stxs))
              (set! attr-type-stxs (cons #'type attr-type-stxs))
              (set! attr-kw-stxs   (cons #`(quote #,(datum->syntax-object #f (string->keyword (symbol->string (syntax-object->datum #'name))))) attr-kw-stxs))
              (set! accessor-stxs  (cons (make-id name-stx name-stx '- #'name) accessor-stxs))
              (set! mutator-stxs   (cons (make-id name-stx 'set- name-stx '- #'name '!) mutator-stxs))
              (parse-attr-kws #'(arg ...)))]))
  
  (define (parse-entity-kws stx)
    (syntax-case stx ()
      [(kw other ...) (parse-entity-kw #'kw #'(other ...))]
      [()             (finish-entity)]))
  
  (define (parse-entity-kw kw-stx other-stx)
    (match (syntax-object->datum kw-stx)
      ['#:property
       (syntax-case other-stx ()
         [(prop-id prop-val other ...)
          (identifier? #'prop-id)
          (begin (set! property-stxs (cons #'(cons prop-id prop-val) property-stxs))
                 (parse-entity-kws #'(other ...)))])]
      [_
       (syntax-case other-stx ()
         [(val other ...)
          (begin (set! entity-arg-stxs (list* #'val #`(quote #,kw-stx) entity-arg-stxs))
                 (parse-entity-kws #'(other ...)))])]))
  
  ; Finishing parsing: the actual output format:
  
  (define (finish-entity)
    
    (with-syntax ([name                 name-stx]
                  [entity               id-stx]
                  [struct-type          struct-type-stx]
                  [constructor          constructor-stx]
                  [predicate            predicate-stx]
                  [constructor/defaults constructor/defaults-stx]
                  [copy-struct          copy-struct-stx]
                  [deserialize-info     deserialize-info-stx]
                  [(attr-name ...)      (reverse attr-name-stxs)]
                  [(attr-id ...)        (reverse attr-id-stxs)]
                  [(attr-kw ...)        (reverse attr-kw-stxs)]
                  [(attr-type ...)      (reverse attr-type-stxs)]
                  [(column-name ...)    (reverse column-name-stxs)]
                  [(accessor ...)       (reverse accessor-stxs)]
                  [(mutator ...)        (reverse mutator-stxs)]
                  [(property ...)       (reverse property-stxs)]
                  [(entity-arg ...)     (reverse entity-arg-stxs)]
                  [(attr-name* ...)     (list* #'id #'revision (reverse attr-name-stxs))])
      ; Swap this 'begin' with a 'quote' to see what is going on in the macro.
      (quasisyntax/loc name-stx
        (begin (begin (define-values (entity struct-type constructor predicate)
                        (let ([entity-args (list entity-arg ...)])
                          (keyword-apply make-persistent-struct-type
                                         (append (list #:column-names #:properties)
                                                 (filter (lambda (x) 
                                                           (keyword? x)) 
                                                         entity-args))
                                         (append (list (list column-name ...) 
                                                       (let ([properties (list property ...)])
                                                         (if (reserved-properties? properties)
                                                             (raise-exn exn:fail:snooze
                                                               (format "~a: cannot specify prop:entity or prop:serialize as an argument to define-persistent-struct." 'name))
                                                             (cons (cons prop:serializable
                                                                         (make-serialize-info
                                                                          (lambda (struct)
                                                                            (list->vector (struct-attributes struct)))
                                                                          (quote-syntax deserialize-info)
                                                                          #t
                                                                          (or (current-load-relative-directory) (current-directory))))
                                                                   properties))))
                                                 (filter (lambda (x) 
                                                           (not (keyword? x)))
                                                         entity-args))
                                         (list 'name 
                                               (list 'attr-name ...)
                                               (list attr-type ...)))))
                      
                      (define-values (attr-id ...)
                        (apply values (entity-attributes entity)))
                      
                      (define-values (accessor ...)
                        (apply values (map attribute-accessor (entity-attributes entity))))
                      
                      (define-values (mutator ...)
                        (apply values (map attribute-mutator (entity-attributes entity))))
                      
                      (define/kw (constructor/defaults #:key #,@(append-map (lambda (attr name)
                                                                              (list #`[#,name (type-default (attribute-type #,attr))]))
                                                                            (syntax->list #'(attr-id ...))
                                                                            (syntax->list #'(attr-name* ...))))
                        ((entity-constructor entity) attr-name* ...))
                      
                      (define/kw (copy-struct original #:key #,@(append-map (lambda (accessor name)
                                                                              (list #`[#,name (#,accessor original)]))
                                                                            (syntax->list #'(accessor ...))
                                                                            (syntax->list #'(attr-name* ...))))
                        ((entity-constructor entity) attr-name* ...))
                      
                      (define deserialize-info
                        (make-deserialize-info
                         ; maker
                         (entity-constructor entity)
                         ; cycle-maker
                         (lambda ()
                           (values constructor/defaults
                                   copy-struct))))
                      
                      ; Transformer binding: makes things like (struct ...) in plt-match work.
                      ; Copied by-example from an expanded define-struct.
                      ; The syntax-quotes-within-syntax-quotes are intensional.
                      (define-syntaxes (name)
                        (let ([certify (syntax-local-certifier #t)])
                          ; Cache persistent-struct-specific compile time information:
                          (persistent-struct-info-set! (certify #'name)
                                                       (certify #'struct-type)
                                                       (certify #'entity)
                                                       (certify #'constructor)
                                                       (certify #'constructor/defaults)
                                                       (certify #'copy-struct)
                                                       (certify #'predicate)
                                                       (list (certify #'attr-id) ...)
                                                       (list (certify #'accessor) ...)
                                                       (list (certify #'mutator) ...)
                                                       (list 'attr-name* ...))
                          ; Return general compile-time information:
                          (make-struct-info 
                           (lambda ()
                             (list (certify #'struct-type)
                                   (certify #'constructor)
                                   (certify #'predicate)
                                   (reverse (list (certify #'accessor) ...))
                                   (reverse (list (certify #'mutator) ...))
                                   (certify #'persistent-struct)))))))))))
  
  ; Main transformer body:
  
  (syntax-case stx ()
    [(_ arg ...)
     (parse-id+attrs #'(arg ...))]))

; syntax provide-persistent-struct : identifier (identifier type) ...
(define-syntax (provide-persistent-struct stx)
  (syntax-case stx ()
    [(_ id (attr-id ...))
     (with-syntax ([transformer          (make-id #'id #'id)]
                   [struct-type          (make-id #'id 'struct: #'id)]
                   [constructor          (make-id #'id 'make- #'id)]
                   [constructor/defaults (make-id #'id 'make- #'id '/defaults)]
                   [copy                 (make-id #'id 'copy- #'id)]
                   [deserialize-info     (make-id #'id 'deserialize-info: #'id '-v0)]
                   [predicate            (make-id #'id #'id '?)]
                   [entity               (make-id #'id 'entity: #'id)]
                   [(attribute ...)      (make-attribute-ids #'id 'attr: '|| #'id #'(attr-id ...))]
                   [(accessor ...)       (make-attribute-ids #'id '||    '|| #'id #'(attr-id ...))]
                   [(mutator ...)        (make-attribute-ids #'id 'set-  '!  #'id #'(attr-id ...))])
       #'(provide entity
                  attribute ...
                  id
                  struct-type
                  constructor
                  constructor/defaults
                  copy
                  deserialize-info
                  predicate
                  accessor ...
                  mutator ...))]))

; syntax (_ struct-id (attr-id ...))
(define-syntax persistent-struct-out
  (make-provide-transformer
   (lambda (stx modes)
     ; syntax -> export
     (define (create-export id-stx)
       (make-export id-stx (syntax-object->datum id-stx) 0 #f id-stx))
     ; (listof export)
     (syntax-case stx ()
       [(_ id (attr-id ...))
        (let ([struct-type          (make-id #'id 'struct: #'id)]
              [constructor          (make-id #'id 'make- #'id)]
              [constructor/defaults (make-id #'id 'make- #'id '/defaults)]
              [copy                 (make-id #'id 'copy- #'id)]
              [deserialize-info     (make-id #'id 'deserialize-info: #'id '-v0)]
              [predicate            (make-id #'id #'id '?)]
              [entity               (make-id #'id 'entity: #'id)]
              [attributes           (make-attribute-ids #'id 'attr: '|| #'id #'(attr-id ...))]
              [accessors            (make-attribute-ids #'id '||    '|| #'id #'(attr-id ...))]
              [mutators             (make-attribute-ids #'id 'set-  '!  #'id #'(attr-id ...))])
          (map create-export 
               (append (list #'id
                             struct-type
                             constructor 
                             constructor/defaults
                             copy
                             deserialize-info
                             predicate
                             entity)
                       attributes
                       accessors
                       mutators)))]))))

; syntax define/provide-persistent-struct
(define-syntax (define/provide-persistent-struct stx)
  (syntax-case stx ()
    ; Fields only:
    [(_ name ([attr-id attr-arg ...] ...) entity-arg ...)
     #'(begin (define-persistent-struct name ([attr-id attr-arg ...] ...) entity-arg ...)
              (provide-persistent-struct name (attr-id ...)))]))

; Helpers ----------------------------------------

; (listof (cons property any)) -> boolean
(define (reserved-properties? prop-alist)
  (ormap (lambda (prop)
           (or (eq? prop prop:entity)
               (eq? prop prop:serializable)))
         (map car prop-alist)))

; Provide statements -----------------------------

(provide define-persistent-struct
         provide-persistent-struct
         persistent-struct-out
         define/provide-persistent-struct)
