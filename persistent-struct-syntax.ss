#lang scheme/base

(require (for-syntax scheme/base
                     scheme/match
                     scheme/pretty
                     scheme/provide-transform
                     scheme/struct-info
                     (only-in srfi/1/list append-map)
                     srfi/26/cut
                     (planet untyped/unlib:3/debug)
                     (planet untyped/unlib:3/syntax)
                     "persistent-struct-info.ss"
                     "era/era.ss"
                     "sql/sql.ss")
         scheme/serialize
         "base.ss"
         "persistent-struct.ss"
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
              (set! attr-kw-stxs             (list (datum->syntax #f (string->keyword "revision"))
                                                   (datum->syntax #f (string->keyword "id"))))
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
      (match (syntax->datum kw-stx)
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
              (set! attr-kw-stxs   (cons (datum->syntax #f (string->keyword (symbol->string (syntax->datum #'name)))) attr-kw-stxs))
              (set! accessor-stxs  (cons (make-id name-stx name-stx '- #'name) accessor-stxs))
              (set! mutator-stxs   (cons (make-id name-stx 'set- name-stx '- #'name '!) mutator-stxs))
              (parse-attr-kws #'(arg ...)))]))
  
  (define (parse-entity-kws stx)
    (syntax-case stx ()
      [(kw other ...) (parse-entity-kw #'kw #'(other ...))]
      [()             (finish-entity)]))
  
  (define (parse-entity-kw kw-stx other-stx)
    (match (syntax->datum kw-stx)
      ['#:property
       (syntax-case other-stx ()
         [(prop-id prop-val other ...)
          (identifier? #'prop-id)
          (begin (set! property-stxs (cons #'(cons prop-id prop-val) property-stxs))
                 (parse-entity-kws #'(other ...)))])]
      [_
       (syntax-case other-stx ()
         [(val other ...)
          (begin (set! entity-arg-stxs (list* #'val kw-stx entity-arg-stxs))
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
                  [(attr-name* ...)     (list* #'id #'revision (reverse attr-name-stxs))]
                  [all-properties       (if (eq? (syntax-local-context) 'module)
                                            #'(cons (cons prop:serializable
                                                          (make-serialize-info
                                                           (lambda (struct)
                                                             (list->vector (struct-attributes struct)))
                                                           (quote-syntax deserialize-info)
                                                           #t
                                                           (or (current-load-relative-directory) (current-directory))))
                                                    properties)
                                            #'properties)])
      ; Swap this 'begin' with a 'quote' to see what is going on in the macro.
      (quasisyntax/loc name-stx
        (begin (begin (define-values (entity struct-type constructor predicate)
                        (make-persistent-struct-type 'name
                                                     (list 'attr-name ...)
                                                     (list attr-type ...)
                                                     #:column-names
                                                     (list column-name ...)
                                                     #:properties   
                                                     (let ([properties (list property ...)])
                                                       (if (reserved-properties? properties)
                                                           (raise-exn exn:fail:snooze
                                                             (format "~a: cannot specify prop:entity or prop:serialize as an argument to define-persistent-struct." 'name))
                                                           all-properties))
                                                     entity-arg ...))
                      
                      (define-values (attr-id ...)
                        (apply values (entity-attributes entity)))
                      
                      (define-values (accessor ...)
                        (apply values (map attribute-accessor (entity-attributes entity))))
                      
                      (define-values (mutator ...)
                        (apply values (map attribute-mutator (entity-attributes entity))))
                      
                      (define (constructor/defaults #,@(append-map (lambda (kw attr name)
                                                                     (list kw #`[#,name (type-default (attribute-type #,attr))]))
                                                                   (syntax->list #'(attr-kw ...))
                                                                   (syntax->list #'(attr-id ...))
                                                                   (syntax->list #'(attr-name* ...))))
                        ((entity-constructor entity) attr-name* ...))
                      
                      (define (copy-struct original #,@(append-map (lambda (kw accessor name)
                                                                     (list kw #`[#,name (#,accessor original)]))
                                                                   (syntax->list #'(attr-kw ...))
                                                                   (syntax->list #'(accessor ...))
                                                                   (syntax->list #'(attr-name* ...))))
                        ((entity-constructor entity) attr-name* ...))
                      
                      #,(if (eq? (syntax-local-context) 'module)
                            #'(begin
                                (define deserialize-info
                                  (make-deserialize-info
                                   ; maker
                                   (entity-constructor entity)
                                   ; cycle-maker
                                   (lambda ()
                                     (values constructor/defaults
                                             copy-struct))))
                                (provide deserialize-info))
                            #'(begin))
                      
                      ; Transformer binding: makes things like (struct ...) in plt-match work.
                      ; Copied by-example from an expanded define-struct.
                      ; The syntax-quotes-within-syntax-quotes are intensional.
                      (define-syntaxes (name)
                        (let ([certify (syntax-local-certifier #t)])
                          ; Cache persistent-struct-specific compile time information:
                          (persistent-struct-info-set! (certify #'name)
                                                       (certify #'entity)
                                                       (list (certify #'attr-id) ...)
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

; (_ struct-id)
(define-syntax persistent-struct-extras-out
  (make-provide-transformer
   (lambda (stx modes)
     ; syntax -> export
     (define (create-export id-stx)
       (make-export id-stx (syntax->datum id-stx) 0 #f id-stx))
     ; (listof export)
     (syntax-case stx ()
       [(_ id)
        (let ([constructor/defaults (make-id #'id 'make- #'id '/defaults)]
              [copy                 (make-id #'id 'copy- #'id)]
              [entity               (make-id #'id 'entity: #'id)]
              [attributes           (persistent-struct-info-attribute-ids (persistent-struct-info-ref #'id))])
          (map create-export (append (list constructor/defaults copy entity) attributes)))]))))

; (_ struct-id (attr-id ...))
(define-syntax persistent-struct-out
  (make-provide-transformer
   (lambda (stx modes)
     ; (listof export)
     (syntax-case stx ()
       [(_ id)
        (append (expand-export #'(struct-out id) modes)
                (expand-export #'(persistent-struct-extras-out id) modes))]))))

; Helpers ----------------------------------------

; (listof (cons property any)) -> boolean
(define (reserved-properties? prop-alist)
  (ormap (lambda (prop)
           (or (eq? prop prop:entity)
               (eq? prop prop:serializable)))
         (map car prop-alist)))

; Provide statements -----------------------------

(provide define-persistent-struct
         persistent-struct-out
         persistent-struct-extras-out)
