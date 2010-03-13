#lang scheme/base

(require "../base.ss")

(require (for-syntax "../base.ss"
                     scheme/base
                     scheme/struct-info
                     (only-in srfi/1 append-map)
                     (cce-scheme-in syntax)
                     (unlib-in syntax)
                     "pretty.ss"
                     "quick-find.ss"
                     "syntax-info.ss")
         scheme/serialize
         "../sql/sql.ss"
         (except-in "struct.ss" make-entity)
         "entity.ss"
         "pretty.ss"
         "snooze-struct.ss"
         "syntax-info.ss"
         (prefix-in sql: "../sql/sql-lang.ss"))

; Syntax -----------------------------------------

(define-syntax (define-entity complete-stx)
  
  ; The following are accumulated from the syntactic form.
  ; Lists are accumulated in reverse order and re-reversed in "finish".
  
  ; Entity:
  (define entity-id-stx #f)                    ; person
  (define plural-id-stx #f)                    ; people
  (define private-id-stx #f)                   ; entity:person
  (define struct-type-stx #f)                  ; struct:person
  (define constructor-stx #f)                  ; make-person
  (define predicate-stx #f)                    ; person?
  (define entity-guid-stx #f)                  ; person-guid
  (define entity-guid-struct-type-stx #f)      ; struct:person-guid
  (define entity-guid-constructor-stx #f)      ; make-person-guid
  (define entity-guid-predicate-stx #f)        ; person-guid?
  (define entity-guid-type-stx #f)             ; person-guid-type
  (define entity-guid-type-struct-type-stx #f) ; struct:person-guid-type
  (define entity-guid-type-constructor-stx #f) ; make-person-guid-type
  (define entity-guid-type-predicate-stx #f)   ; person-guid-type?
  
  (define id-accessor-stx #f)                  ; person-id
  (define saved-predicate-stx #f)              ; person-saved?
  (define pretty-formatter-stx #f)             ; format-person
  (define defaults-constructor-stx #f)         ; make-person/defaults
  (define copy-constructor-stx #f)             ; person-set
  (define default-order-stx #f)                ; (sql-list (asc person.name) ...)
  (define deserialize-info-stx #f)             ; deserialize-info:person
  (define property-stxs null)                  ; (... (cons prop:bar bar) (cons prop:foo foo))
  (define entity-kw-stxs null)                 ; #:table-name 'Person ...
  
  ; Attributes:
  (define attr-stxs null)                      ; (gender age name)
  (define attr-private-stxs null)              ; (attr:person-gender attr:person-age attr:person-name attr:person-revision attr:person-guid)
  (define attr-type-stxs null)                 ; (boolean integer person ...)
  (define attr-type-expr-stxs null)            ; ((make-symbol-type #t) (make-integer-type #f) ...)
  (define attr-default-stxs null)              ; (#t 123 ...)
  (define attr-kw-stxs null)                   ; (#:person-gender #:person-age #:person-name #:person-revision #:person-guid)
  (define attr-column-stxs null)               ; ('gender 'age 'name)
  (define attr-accessor-stxs null)             ; (person-gender person-age person-name person-revision person-guid)
  (define attr-guid-accessor-stxs null)        ; (#f #f pet-owner-guid #f) <= people don't have foreign-keys so had to use pet for this example
  (define attr-mutator-stxs null)              ; (set-person-gender! set-person-age! set-person-name! set-person-revision! set-person-guid!)
  (define attr-pretty-stxs null)
  (define attr-pretty-plural-stxs null)
  
  ; Parsing entity information:
  
  (define (parse-name stx)
    (syntax-case stx ()
      [(name other ...)
       (begin (set! entity-id-stx                    #'name)
              (set! plural-id-stx                    (make-id #'name (name->plural-name (syntax->datum #'name))))
              (set! private-id-stx                   (make-id #f 'entity: #'name))
              (set! struct-type-stx                  (make-id #'name 'struct: #'name))
              (set! constructor-stx                  (make-id #'name 'make- #'name))
              (set! predicate-stx                    (make-id #'name #'name '?))
              (set! entity-guid-stx                  (make-id #f #'name '-guid))
              (set! entity-guid-struct-type-stx      (make-id #f 'struct: #'name '-guid))
              (set! entity-guid-constructor-stx      (make-id #f 'make- #'name '-guid))
              (set! entity-guid-predicate-stx        (make-id #f #'name '-guid?))
              (set! entity-guid-type-stx             (make-id #'name #'name '-guid-type))
              (set! entity-guid-type-struct-type-stx (make-id #'name 'struct: #'name '-guid-type))
              (set! entity-guid-type-constructor-stx (make-id #'name 'make- #'name '-guid-type))
              (set! entity-guid-type-predicate-stx   (make-id #'name #'name '-guid-type?))
              (set! id-accessor-stx                  (make-id #'name #'name '-id))
              (set! saved-predicate-stx              (make-id #'name #'name '-saved?))
              (set! pretty-formatter-stx             (make-id #'name 'format- #'name))
              (set! defaults-constructor-stx         (make-id #'name 'make- #'name '/defaults))
              (set! copy-constructor-stx             (make-id #'name #'name '-set))
              (set! default-order-stx                #'null)
              (set! deserialize-info-stx             (make-id #'name 'deserialize-info: #'name '-v0))
              (parse-attrs #'(other ...)))]))
  
  (define (parse-attrs stx)
    (syntax-case stx ()
      [((attr ...) entity-kw ...)
       (begin (for-each parse-attr (syntax->list #'(attr ...)))
              (parse-entity-kws #'(entity-kw ...)))]))
  
  (define (parse-attr stx)
    (define my-name-stx          #f)
    (define my-private-stx       #f)
    (define my-allows-null-stx   #'#t)
    (define my-min-value-stx     #'#f)
    (define my-max-value-stx     #'#f)
    (define my-max-length-stx    #'#f)
    (define my-values-stx        #f)
    (define my-default-stx       #'(lambda () #f))
    (define my-type-stx          #f)
    (define my-type-expr-stx     #f)
    (define my-kw-stx            #f)
    (define my-accessor-stx      #f)
    (define my-guid-accessor-stx #f)
    (define my-mutator-stx       #f)
    (define my-column-stx        #f)
    (define my-pretty-stx        #'name->pretty-name)
    (define my-pretty-plural-stx #'pretty-name->pretty-name-plural)
    
    (define (parse-attr-type stx)
      (with-syntax ([allows-null? my-allows-null-stx]
                    [min-value    my-min-value-stx]
                    [max-value    my-max-value-stx]
                    [max-length   my-max-length-stx]
                    [values       my-values-stx])
        (set! my-type-stx stx)
        (syntax-case* stx (boolean integer real symbol string time-tai time-utc enum binary) symbolic-identifier=?
          [boolean  (set! my-type-expr-stx #'(make-boolean-type allows-null?))]
          [integer  (set! my-type-expr-stx #'(make-integer-type allows-null? min-value max-value))]
          [real     (set! my-type-expr-stx #'(make-real-type allows-null? min-value max-value))]
          [symbol   (set! my-type-expr-stx #'(make-symbol-type allows-null? max-length))]
          [string   (set! my-type-expr-stx #'(make-string-type allows-null? max-length))]
          [time-tai (set! my-type-expr-stx #'(make-time-tai-type allows-null?))]
          [time-utc (set! my-type-expr-stx #'(make-time-utc-type allows-null?))]
          [enum     (if my-values-stx
                        (set! my-type-expr-stx #'(create-enum-type allows-null? values))
                        (raise-syntax-error #f "required keyword missing: #:values" complete-stx stx))]
          [binary   (set! my-type-expr-stx #'(make-binary-type allows-null?))]
          [entity   (if (or (bound-identifier=? #'entity entity-id-stx)
                            (with-handlers ([exn? (lambda _ #f)]) (entity-info-ref #'entity)))
                        (begin (set! my-type-expr-stx #'(entity-make-guid-type entity allows-null?))
                               (set! my-guid-accessor-stx (make-id entity-id-stx entity-id-stx '- my-name-stx '-guid)))
                        (raise-syntax-error #f "not a valid attribute type" complete-stx stx))])))
    
    (define (parse-attr-kws stx)
      (syntax-case stx ()
        [(#:allow-null? val other ...)
         (begin (set! my-allows-null-stx #'val)
                (parse-attr-kws #'(other ...)))]
        [(#:min-value val other ...)
         (begin (set! my-min-value-stx #'val)
                (parse-attr-kws #'(other ...)))]
        [(#:max-value val other ...)
         (begin (set! my-max-value-stx #'val)
                (parse-attr-kws #'(other ...)))]
        [(#:max-length val other ...)
         (begin (set! my-max-length-stx #'val)
                (parse-attr-kws #'(other ...)))]
        [(#:values val other ...)
         (begin (set! my-values-stx #'val)
                (parse-attr-kws #'(other ...)))]
        [(#:default val other ...)
         (begin (set! my-default-stx #'(lambda () val))
                (parse-attr-kws #'(other ...)))]
        [(#:default-proc val other ...)
         (begin (set! my-default-stx #'val)
                (parse-attr-kws #'(other ...)))]
        [(#:column-name val other ...)
         (begin (set! my-column-stx #'val)
                (parse-attr-kws #'(other ...)))]
        [(#:pretty-name val other ...)
         (begin (set! my-pretty-stx #'(lambda _ val))
                (parse-attr-kws #'(other ...)))]
        [(#:pretty-name-plural val other ...)
         (begin (set! my-pretty-plural-stx #'(lambda _ val))
                (parse-attr-kws #'(other ...)))]
        [(kw other ...)
         (raise-syntax-error #f "unrecognised attribute keyword" complete-stx #'kw)]
        [() (void)]))
    
    (define (finish-attr)
      (set! attr-stxs               (cons my-name-stx          attr-stxs))
      (set! attr-private-stxs       (cons my-private-stx       attr-private-stxs))
      (set! attr-type-stxs          (cons my-type-stx          attr-type-stxs))
      (set! attr-type-expr-stxs     (cons my-type-expr-stx     attr-type-expr-stxs))
      (set! attr-default-stxs       (cons my-default-stx       attr-default-stxs))
      (set! attr-kw-stxs            (cons my-kw-stx            attr-kw-stxs))
      (set! attr-accessor-stxs      (cons my-accessor-stx      attr-accessor-stxs))
      (set! attr-guid-accessor-stxs (cons my-guid-accessor-stx attr-guid-accessor-stxs))
      (set! attr-mutator-stxs       (cons my-mutator-stx       attr-mutator-stxs))
      (set! attr-column-stxs        (cons my-column-stx        attr-column-stxs))
      (set! attr-pretty-stxs        (cons my-pretty-stx        attr-pretty-stxs))
      (set! attr-pretty-plural-stxs (cons my-pretty-plural-stx attr-pretty-plural-stxs)))
    
    (syntax-case stx ()
      [(name type arg ...)
       (and (identifier? #'name)
            (identifier? #'type))
       (begin (set! my-name-stx          #'name)
              (set! my-private-stx       (make-id #f 'attr: entity-id-stx '- #'name))
              (set! my-kw-stx            (datum->syntax #f (string->keyword (symbol->string (syntax->datum #'name)))))
              (set! my-accessor-stx      (make-id entity-id-stx entity-id-stx '- #'name))
              (set! my-mutator-stx       (make-id entity-id-stx 'set- entity-id-stx '- #'name '!))
              (set! my-column-stx        #'(name->database-name 'name))
              (parse-attr-kws #'(arg ...))
              (parse-attr-type #'type)
              (finish-attr))]))
  
  (define (parse-entity-kws stx)
    (syntax-case stx ()
      [(#:plural val other ...)
       (if (identifier? #'val)
           (begin (set! plural-id-stx #'val)
                  (parse-entity-kws #'(other ...)))
           (raise-syntax-error #f "#:plural must be an identifier" complete-stx #'(#:plural val)))]
      [(#:order (expr ...) other ...)
       (begin (set! default-order-stx #'(sql-list expr ...))
              (parse-entity-kws #'(other ...)))]
      [(#:order expr other ...)
       (raise-syntax-error #f "#:order must be in the form ((asc foo.bar) ...)"
                           complete-stx #'expr)]
      [(#:property prop:snooze-struct-entity val other ...)
       (raise-syntax-error #f "prop:snooze-struct-entity cannot be specified in a define-entity form"
                           complete-stx #'(#:property prop:snooze-struct-entity val))]
      [(#:property prop:serializable val other ...)
       (raise-syntax-error #f "prop:serializable cannot be specified in a define-entity form"
                           complete-stx #'(#:property prop:snooze-struct-entity val))]
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
                  [entity                       entity-id-stx]
                  [plural                       plural-id-stx]
                  [entity-private               private-id-stx]
                  [struct-type                  struct-type-stx]
                  [constructor                  constructor-stx]
                  [predicate                    predicate-stx]
                  [entity-guid                  entity-guid-stx]
                  [struct:entity-guid           entity-guid-struct-type-stx]
                  [entity-guid-constructor      entity-guid-constructor-stx]
                  [entity-guid-predicate        entity-guid-predicate-stx]
                  [entity-guid-type             entity-guid-type-stx]
                  [struct:entity-guid-type      entity-guid-type-struct-type-stx]
                  [entity-guid-type-constructor entity-guid-type-constructor-stx]
                  [entity-guid-type-predicate   entity-guid-type-predicate-stx]
                  
                  [find-one                     (make-id entity-id-stx 'find- entity-id-stx)]
                  [find-all                     (if (eq? (syntax->datum entity-id-stx)
                                                         (syntax->datum plural-id-stx))
                                                    (make-id entity-id-stx 'find-all- plural-id-stx)
                                                    (make-id entity-id-stx 'find- plural-id-stx))]
                  [find-count                   (make-id entity-id-stx 'find-count- plural-id-stx)]
                  [g:find                       (make-id entity-id-stx 'g: plural-id-stx)]
                  
                  [id-accessor                  id-accessor-stx]
                  [saved-predicate              saved-predicate-stx]
                  [pretty-formatter             pretty-formatter-stx]
                  [defaults-constructor         defaults-constructor-stx]
                  [copy-constructor             copy-constructor-stx]
                  [default-order                default-order-stx]
                  [deserialize-info             deserialize-info-stx]
                  [(property ...)               (reverse property-stxs)]
                  [(entity-kw ...)              (reverse entity-kw-stxs)]
                  ; Attributes:
                  [(guid-attr revision-attr attr ...)                     (list* #'guid
                                                                                 #'revision
                                                                                 (reverse attr-stxs))]
                  [(guid-private revision-private attr-private ...)       (list* (make-id #f 'attr: entity-id-stx '-revision)
                                                                                 (make-id #f 'attr: entity-id-stx '-guid)
                                                                                 (reverse attr-private-stxs))]
                  [(guid-kw revision-kw attr-kw ...)                      (list* '#:guid
                                                                                 '#:revision
                                                                                 (reverse attr-kw-stxs))]
                  [(attr-type ...)                                        (reverse attr-type-stxs)]
                  [(attr-type-expr ...)                                   (reverse attr-type-expr-stxs)]
                  [(attr-default ...)                                     (reverse attr-default-stxs)]
                  [(attr-pretty ...)                                      (reverse attr-pretty-stxs)]
                  [(attr-pretty-plural ...)                               (reverse attr-pretty-plural-stxs)]
                  [(guid-accessor revision-accessor accessor ...)         (list* (make-id entity-id-stx entity-id-stx '-guid)
                                                                                 (make-id entity-id-stx entity-id-stx '-revision)
                                                                                 (reverse attr-accessor-stxs))]
                  [(attr-guid-accessor+false ...)                         (reverse attr-guid-accessor-stxs)]
                  [(attr-guid-accessor ...)                               (filter (lambda (x) x)
                                                                                  (reverse attr-guid-accessor-stxs))]
                  [(revision-mutator mutator ...)                         (list* (make-id entity-id-stx 'set- entity-id-stx '-revision!)
                                                                                 (reverse attr-mutator-stxs))]
                  [(guid-column revision-column column ...)               (list* #''id
                                                                                 #''revision
                                                                                 (reverse attr-column-stxs))])
      (quasisyntax/loc entity-id-stx
        (begin (define-serializable-struct (entity-guid guid)
                 ()
                 #:transparent
                 #:property
                 prop:guid-entity-box
                 (box #f)
                 #:property
                 prop:custom-write
                 (lambda (guid out write?)
                   ((if write? write display)
                    (vector 'entity-guid (guid-id guid))
                    out)))
               
               (define-serializable-struct (entity-guid-type guid-type)
                 ()
                 #:transparent
                 #:property
                 prop:guid-type-entity-box
                 (box #f))
               
               (define-values (entity-private struct-type constructor predicate)
                 (let* ([attr-names               (list 'attr ...)]
                        [attr-pretty-names        (map (lambda (proc arg) (proc arg))
                                                       (list attr-pretty ...)
                                                       attr-names)]
                        [attr-pretty-names-plural (map (lambda (proc arg) (proc arg))
                                                       (list attr-pretty-plural ...)
                                                       attr-pretty-names)]
                        [make-attr-types          (lambda (entity) (list attr-type-expr ...))])
                   (make-entity (current-model)
                                'entity
                                'plural
                                attr-names
                                make-attr-types
                                (list attr-default ...)
                                entity-guid-constructor
                                entity-guid-predicate
                                entity-guid-type-constructor
                                entity-guid-type-predicate
                                #:attr-column-names        (list column ...)
                                #:attr-pretty-names        attr-pretty-names
                                #:attr-pretty-names-plural attr-pretty-names-plural
                                #:properties   
                                #,(if (eq? (syntax-local-context) 'module)
                                      (syntax/loc entity-id-stx
                                        (list (cons prop:serializable
                                                    (make-serialize-info
                                                     (lambda (struct) (list->vector (snooze-struct-raw-ref* struct)))
                                                     (quote-syntax deserialize-info)
                                                     #t
                                                     (or (current-load-relative-directory) (current-directory))))
                                              property ...))
                                      (syntax/loc entity-id-stx
                                        (list property ...)))
                                entity-kw ...)))
               
               (set-box! (guid-entity-box      struct:entity-guid)      entity-private)
               (set-box! (guid-type-entity-box struct:entity-guid-type) entity-private)
               
               (define-values (guid-private revision-private attr-private ...)
                 (apply values (entity-attributes entity-private)))
               
               (define-values (guid-accessor revision-accessor accessor ...)
                 (apply values (map attribute-accessor (entity-attributes entity-private))))
               
               (define-values (attr-guid-accessor ...)
                 (apply values (map attribute-guid-accessor 
                                    (filter (lambda (att)
                                              (guid-type? (attribute-type att)))
                                            (entity-data-attributes entity)))))
               
               (define-values (revision-mutator mutator ...)
                 (apply values (map attribute-mutator (cdr (entity-attributes entity-private)))))
               
               (define id-accessor snooze-struct-id)
               (define saved-predicate snooze-struct-saved?)
               (define pretty-formatter format-snooze-struct)
               
               (define (defaults-constructor
                         #,@(append-map (lambda (kw attr name)
                                          (list kw #`[#,name (attribute-default #,attr)]))
                                        (syntax->list #'(attr-kw ...))
                                        (syntax->list #'(attr-private ...))
                                        (syntax->list #'(attr ...))))
                 ((entity-constructor entity-private)
                  (entity-make-temporary-guid entity-private)
                  #f attr ...))
               
               (define (copy-constructor
                        original
                        #,@(append-map (lambda (kw accessor name)
                                         (list kw #`[#,name (#,accessor original)]))
                                       (syntax->list #'(attr-kw ...))
                                       (syntax->list #'(accessor ...))
                                       (syntax->list #'(attr ...))))
                 (let* ([guid     (snooze-struct-guid original)]
                        [revision (snooze-struct-revision original)])
                   ((entity-constructor entity-private) guid revision attr ...)))
               
               #,(if (eq? (syntax-local-context) 'module)
                     (syntax/loc entity-id-stx
                       (begin (define deserialize-info
                                (make-deserialize-info
                                 ; maker
                                 (lambda args
                                   (apply (entity-private-constructor entity-private) args))
                                 ; cycle-maker
                                 (lambda ()
                                   (values defaults-constructor copy-constructor))))
                              (provide deserialize-info)))
                     #'(begin))
               
               (define default-alias
                 (sql:alias 'entity entity-private))
               
               (define default-order-private
                 (let-alias ([entity entity-private])
                   default-order))

               (set-entity-defaults-constructor! entity-private defaults-constructor)
               (set-entity-copy-constructor!     entity-private copy-constructor)
               (set-entity-default-alias!        entity-private default-alias)
               (set-entity-default-order!        entity-private default-order-private)
                              
               (define-values (find-one find-all find-count g:find)
                 #,(make-quick-finds
                    entity-id-stx
                    (list #'find-one #'find-all #'find-count #'g:find)
                    #'default-alias
                    (syntax->list #'(guid-attr revision-attr attr ...))
                    #'default-order-private))
               
               ; Transformer binding: makes things like (struct ...) in plt-match work.
               ; Copied by-example from an expanded define-struct.
               ; The syntax-quotes-within-syntax-quotes are intensional.
               (define-syntaxes (entity)
                 (let ([certify (syntax-local-certifier #t)])
                   ; Cache snooze-struct-specific compile time information:
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
                                            (certify #'revision-mutator)))
                             #t))
                     (certify #'entity)
                     (certify #'entity-private)
                     (certify #'struct-type)
                     (certify #'constructor)
                     (certify #'predicate)
                     (certify #'id-accessor)
                     (certify #'saved-predicate)
                     (certify #'pretty-formatter)
                     (certify #'defaults-constructor)
                     (certify #'copy-constructor)
                     (certify #'default-order-private)
                     (certify #'entity-guid)
                     (certify #'entity-guid-constructor)
                     (certify #'entity-guid-predicate)
                     (certify #'default-alias)
                     (certify #'default-order)
                     (certify #'find-one)
                     (certify #'find-all)
                     (certify #'find-count)
                     (certify #'g:find)
                     (list (make-attribute-info
                            (certify #'guid-attr)
                            (certify #'guid-private)
                            (certify #'entity)
                            (certify #'guid-accessor)
                            #f
                            #f)
                           (make-attribute-info
                            (certify #'revision-attr)
                            (certify #'revision-private)
                            (certify #'integer)
                            (certify #'revision-accessor)
                            #f
                            (certify #'revision-mutator))
                           (make-attribute-info
                            (certify #'attr)
                            (certify #'attr-private)
                            (certify #'attr-type)
                            (certify #'accessor)
                            (and (syntax->datum #'attr-guid-accessor+false)
                                 (certify #'attr-guid-accessor+false))
                            (certify #'mutator))
                           ...)))))))))
  
  ; Main transformer body:
  
  (syntax-case complete-stx ()
    [(_ arg ...)
     (parse-name #'(arg ...))]))

; (_ id id) -> attribute
(define-syntax (attr stx)
  (syntax-case* stx () symbolic-identifier=?
    [(_ id attr-id)
     (and (identifier? #'id) (identifier? #'attr-id))
     (let ([info     (entity-info-ref #'id)]
           [attr-sym (syntax->datum #'attr-id)])
       (or (for/or ([attr-info (in-list (entity-info-attribute-info info))])
             (and (eq? attr-sym (syntax->datum (attribute-info-id attr-info)))
                  (attribute-info-private-id attr-info)))
           (raise-syntax-error #f "attribute not found" stx stx)))]))

; (_ id id) -> attribute
(define-syntax (attr-list stx)
  (syntax-case stx ()
    [(_ id attr-id ...) 
     (syntax/loc stx
       (list (attr id attr-id) ...))]))

; Helpers ----------------------------------------

; (listof (cons property any)) -> boolean
(define (reserved-properties? prop-alist)
  (ormap (lambda (prop)
           (or (eq? prop prop:snooze-struct-entity)
               (eq? prop prop:serializable)))
         (map car prop-alist)))

; Provide statements -----------------------------

(provide define-entity
         attr
         attr-list)
