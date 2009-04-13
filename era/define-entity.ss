#lang scheme/base

(require "../base.ss")

(require (for-syntax "../base.ss"
                     scheme/base
                     scheme/provide-transform
                     scheme/struct-info
                     (only-in srfi/1 append-map)
                     (unlib-in syntax)
                     "info.ss")
         scheme/serialize
         (except-in "core.ss" make-entity)
         "cached-struct.ss"
         "entity.ss"
         "info.ss"
         #;(prefix-in sql: "sql/sql-lang.ss"))

; Syntax -----------------------------------------

(define-syntax (define-entity complete-stx)
  
  ; The following are accumulated from the syntactic form.
  ; Lists are accumulated in reverse order and re-reversed in "finish".
  
  ; Entity:
  (define entity-stx #f)                  ; person
  (define entity-private-stx #f)          ; entity:person
  (define struct-type-stx #f)             ; struct:person
  (define constructor-stx #f)             ; make-person
  (define predicate-stx #f)               ; person?
  (define entity-guid-stx #f)             ; guid:person
  (define entity-guid-struct-type-stx #f) ; struct:guid:person
  (define entity-guid-constructor-stx #f) ; make-guid:person
  (define entity-guid-predicate-stx #f)   ; guid:person?
  (define constructor/defaults-stx #f)    ; make-person/defaults
  (define copy-struct-stx #f)             ; copy-person
  (define deserialize-info-stx #f)        ; deserialize-info:person
  (define property-stxs null)             ; (... (cons prop:bar bar) (cons prop:foo foo))
  (define entity-kw-stxs null)            ; #:table-name 'Person ...
  
  ; Attributes:
  (define attr-stxs null)                 ; (gender age name)
  (define attr-private-stxs null)         ; (attr:person-gender attr:person-age attr:person-name attr:person-revision attr:person-guid)
  (define attr-type-stxs null)            ; (type:symbol type:integer type:string)
  (define attr-kw-stxs null)              ; (#:person-gender #:person-age #:person-name #:person-revision #:person-guid)
  (define column-stxs null)               ; ('gender 'age 'name)
  (define accessor-stxs null)             ; (person-gender person-age person-name person-revision person-guid)
  (define mutator-stxs null)              ; (set-person-gender! set-person-age! set-person-name! set-person-revision! set-person-guid!)
  
  ; Parsing entity information:
  
  (define (parse-name stx)
    (syntax-case stx ()
      [(name other ...)
       (begin (set! entity-stx                  #'name)
              (set! entity-private-stx          (make-id #f 'entity: #'name))
              (set! struct-type-stx             (make-id #'name 'struct: #'name))
              (set! constructor-stx             (make-id #'name 'make- #'name))
              (set! predicate-stx               (make-id #'name #'name '?))
              (set! entity-guid-stx             (make-id #'name 'guid: #'name))
              (set! entity-guid-struct-type-stx (make-id #'name 'struct:guid: #'name))
              (set! entity-guid-constructor-stx (make-id #'name 'make-guid: #'name))
              (set! entity-guid-predicate-stx   (make-id #'name 'guid: #'name '?))
              (set! constructor/defaults-stx    (make-id #'name 'make- #'name '/defaults))
              (set! copy-struct-stx             (make-id #'name 'copy- #'name))
              (set! deserialize-info-stx        (make-id #'name 'deserialize-info: #'name '-v0))
              (parse-attrs #'(other ...)))]))
  
  (define (parse-attrs stx)
    (syntax-case stx ()
      [((attr ...) entity-kw ...)
       (begin (for-each parse-attr (syntax->list #'(attr ...)))
              (parse-entity-kws #'(entity-kw ...)))]))
  
  (define (parse-attr stx)
    (define my-name-stx #f)
    (define my-private-stx #f)
    (define my-type-stx #f)
    (define my-kw-stx #f)
    (define my-accessor-stx #f)
    (define my-mutator-stx #f)
    (define my-column-stx #f)
    
    (define (parse-attr-kws stx)
      (syntax-case stx ()
        [(#:column-name val other ...)
         (begin (set! my-column-stx #'val)
                (parse-attr-kws #'(other ...)))]
        [(kw other ...)
         (raise-syntax-error #f "unrecognised attribute keyword" complete-stx #'kw)]
        [() (finish-attr)]))
    
    (define (finish-attr)
      (set! attr-stxs         (cons my-name-stx     attr-stxs))
      (set! attr-private-stxs (cons my-private-stx  attr-private-stxs))
      (set! attr-type-stxs    (cons my-type-stx     attr-type-stxs))
      (set! attr-kw-stxs      (cons my-kw-stx       attr-kw-stxs))
      (set! accessor-stxs     (cons my-accessor-stx accessor-stxs))
      (set! mutator-stxs      (cons my-mutator-stx  mutator-stxs))
      (set! column-stxs       (cons my-column-stx   column-stxs)))
    
    (syntax-case stx ()
      [(name type arg ...)
       (identifier? #'name)
       (begin (set! my-name-stx     #'name)
              (set! my-private-stx  (make-id #f 'attr: entity-stx '- #'name))
              (set! my-type-stx     #'type)
              (set! my-kw-stx       (datum->syntax #f (string->keyword (symbol->string (syntax->datum #'name)))))
              (set! my-accessor-stx (make-id entity-stx entity-stx '- #'name))
              (set! my-mutator-stx  (make-id entity-stx 'set- entity-stx '- #'name '!))
              (set! my-column-stx   #''name)
              (parse-attr-kws #'(arg ...)))]))
  
  (define (parse-entity-kws stx)
    (syntax-case stx ()
      [(#:property prop:entity val other ...)
       (raise-syntax-error #f"prop:entity cannot be specified in a define-entity form" complete-stx #'(#:property prop:entity val))]
      [(#:property prop:serializable val other ...)
       (raise-syntax-error #f"prop:serializable cannot be specified in a define-entity form" complete-stx #'(#:property prop:entity val))]
      [(#:property name val other ...)
       (identifier? #'name)
       (begin (set! property-stxs (cons #'(cons name val) property-stxs))
              (parse-entity-kws #'(other ...)))]
      [(kw val other ...)
       (begin (set! entity-kw-stxs (list* #'val #'kw entity-kw-stxs))
              (parse-entity-kws #'(other ...)))]
      [() (finish-entity)]))
  
  ; Finishing parsing: the actual output format:
  
  (define (finish-entity)
    (with-syntax (; Entity:
                  [entity                   entity-stx]
                  [entity-private           entity-private-stx]
                  [struct-type              struct-type-stx]
                  [constructor              constructor-stx]
                  [predicate                predicate-stx]
                  [entity-guid              entity-guid-stx]
                  [entity-guid-struct-type  entity-guid-struct-type-stx]
                  [entity-guid-constructor  entity-guid-constructor-stx]
                  [entity-guid-predicate    entity-guid-predicate-stx]
                  [constructor/defaults     constructor/defaults-stx]
                  [copy-struct              copy-struct-stx]
                  [deserialize-info         deserialize-info-stx]
                  [(property ...)           (reverse property-stxs)]
                  [(entity-kw ...)          (reverse entity-kw-stxs)]
                  ; Attributes:
                  [(guid-attr revision-attr attr ...)               (list* #'guid
                                                                           #'revision
                                                                           (reverse attr-stxs))]
                  [(guid-private revision-private attr-private ...) (list* (make-id #f 'attr: entity-stx '-revision)
                                                                           (make-id #f 'attr: entity-stx '-guid)
                                                                           (reverse attr-private-stxs))]
                  [(guid-kw revision-kw attr-kw ...)                (list* '#:guid
                                                                           '#:revision
                                                                           (reverse attr-kw-stxs))]
                  [(attr-type ...)                                  (reverse attr-type-stxs)]
                  [(guid-accessor revision-accessor accessor ...)   (list* (make-id entity-stx entity-stx '-guid)
                                                                           (make-id entity-stx entity-stx '-revision)
                                                                           (reverse accessor-stxs))]
                  [(revision-mutator mutator ...)                   (list* (make-id entity-stx 'set- entity-stx '-revision!)
                                                                           (reverse mutator-stxs))]
                  [(guid-column revision-column column ...)         (list* #''id
                                                                           #''revision
                                                                           (reverse column-stxs))])
      (quasisyntax/loc entity-stx
        (begin (define-guid-type entity-guid)
               
               (define-values (entity-private struct-type constructor predicate)
                 (make-entity 'entity
                              (list 'attr ...)
                              (list attr-type ...)
                              entity-guid-constructor
                              entity-guid-predicate
                              #:column-names
                              (list column ...)
                              #:properties   
                              #,(if (eq? (syntax-local-context) 'module)
                                    #'(list (cons prop:serializable
                                                  (make-serialize-info
                                                   (lambda (struct) (list->vector (snooze-struct-ref* struct)))
                                                   (quote-syntax deserialize-info)
                                                   #t
                                                   (or (current-load-relative-directory) (current-directory))))
                                            property ...)
                                    #'(list property ...))
                              entity-kw ...))
               
               (set-box! (guid-entity-box entity-guid-struct-type) entity-private)
               
               (define-values (guid-private revision-private attr-private ...)
                 (apply values (entity-attributes entity-private)))
               
               (define-values (guid-accessor revision-accessor accessor ...)
                 (apply values (map attribute-cached-accessor (entity-attributes entity-private))))
               
               (define-values (revision-mutator mutator ...)
                 (apply values (map attribute-cached-mutator (cdr (entity-attributes entity-private)))))
               
               (define (constructor/defaults #,@(append-map (lambda (kw attr name)
                                                              (list kw #`[#,name (type-default (attribute-type #,attr))]))
                                                            (syntax->list #'(attr-kw ...))
                                                            (syntax->list #'(attr-private ...))
                                                            (syntax->list #'(attr ...))))
                 ((entity-cached-constructor entity-private) attr ...))
               
               (define (copy-struct
                        original
                        #,@(append-map (lambda (kw accessor name)
                                         (list kw #`[#,name (#,accessor original)]))
                                       (syntax->list #'(attr-kw ...))
                                       (syntax->list #'(accessor ...))
                                       (syntax->list #'(attr ...))))
                 (let ([ans ((entity-cached-constructor entity-private) attr ...)])
                   (set-struct-id! ans (struct-id original))
                   (set-struct-revision! ans (struct-revision original))
                   ans))
               
               #,(if (eq? (syntax-local-context) 'module)
                     #'(begin (define deserialize-info
                                (make-deserialize-info
                                 ; maker
                                 (lambda args
                                   (let ([struct (apply (entity-private-constructor entity-private) args)])
                                     (send (current-snooze) cache-add! struct)
                                     struct))
                                 ; cycle-maker
                                 (lambda ()
                                   (values constructor/defaults copy-struct))))
                              (provide deserialize-info))
                     #'(begin))
               
               (define default-alias 'entity #;(sql:alias 'entity entity-private))
               
               ; Transformer binding: makes things like (struct ...) in plt-match work.
               ; Copied by-example from an expanded define-struct.
               ; The syntax-quotes-within-syntax-quotes are intensional.
               (define-syntaxes (entity)
                 (let ([certify (syntax-local-certifier #t)])
                   ; Cache persistent-struct-specific compile time information:
                   (entity-info-add!
                    (make-entity-info
                     (lambda ()
                       (list (certify #'struct-type)
                             (certify #'constructor)
                             (certify #'predicate)
                             (reverse (list (certify #'accessor) ...
                                            (certify #'revision-accessor)
                                            (certify #'guid-accessor)))
                             (reverse (list (certify #'mutator) ...
                                            (certify #'revision-mutator)
                                            #f))
                             #t))
                     (certify #'entity)
                     (certify #'entity-private)
                     (certify #'struct-type)
                     (certify #'constructor)
                     (certify #'predicate)
                     (certify #'constructor/defaults)
                     (certify #'copy-struct)
                     (certify #'entity-guid)
                     (certify #'entity-guid-constructor)
                     (certify #'entity-guid-predicate)
                     (certify #'default-alias)
                     (list (make-attribute-info
                            (certify #'guid-attr)
                            (certify #'guid-private)
                            (certify #'guid-accessor)
                            #f)
                           (make-attribute-info
                            (certify #'revision-attr)
                            (certify #'revision-private)
                            (certify #'revision-accessor)
                            (certify #'revision-mutator))
                           (make-attribute-info
                            (certify #'attr)
                            (certify #'attr-private)
                            (certify #'accessor)
                            (certify #'mutator))
                           ...)))))))))
  
  ; Main transformer body:
  
  (syntax-case complete-stx ()
    [(_ arg ...)
     (parse-name #'(arg ...))]))

; (_ struct-id (attr-id ...))
(define-syntax entity-out
  (make-provide-transformer
   (lambda (stx modes)
     ; (listof export)
     (syntax-case stx ()
       [(_ id)
        (append (expand-export #'(struct-out id) modes)
                (expand-export #'(entity-extras-out id) modes))]))))

; (_ id id) -> attribute
(define-syntax (attr stx)
  (syntax-case* stx () symbolic-identifier=?
    [(_ entity attr)
     (and (identifier? #'entity)
          (identifier? #'attr))
     (let ([info     (entity-info-ref #'entity)]
           [attr-sym (syntax->datum #'attr)])
       (or (for/or ([attr-info (in-list (entity-info-attribute-info info))])
             (and (eq? attr-sym (syntax->datum (attribute-info-id attr-info)))
                  (attribute-info-private-id attr-info)))
           (raise-syntax-error #f "attribute not found" stx stx)))]))

; Helpers ----------------------------------------

; (_ struct-id)
(define-syntax entity-extras-out
  (make-provide-transformer
   (lambda (stx modes)
     ; syntax -> export
     (define (create-export id-stx)
       (make-export id-stx (syntax->datum id-stx) 0 #f id-stx))
     ; (listof export)
     (syntax-case stx ()
       [(_ id)
        (let ([info (entity-info-ref #'id)])
          (map create-export (list (entity-info-constructor/defaults-id info)
                                   (entity-info-copy-struct-id          info))))]))))

; (listof (cons property any)) -> boolean
(define (reserved-properties? prop-alist)
  (ormap (lambda (prop)
           (or (eq? prop prop:entity)
               (eq? prop prop:serializable)))
         (map car prop-alist)))

; Provide statements -----------------------------

(provide define-entity
         entity-out
         attr)
