#lang scheme/base

(require "../base.ss")

(require (for-syntax "../base.ss"
                     scheme/base
                     scheme/provide-transform
                     scheme/struct-info
                     (only-in srfi/1 append-map)
                     (cce-scheme-in syntax)
                     (unlib-in syntax)
                     "syntax-info.ss")
         scheme/serialize
         (except-in "core.ss" make-entity)
         "cached-struct.ss"
         "entity.ss"
         (prefix-in real: "snooze-struct.ss")
         "syntax-info.ss"
         "pretty.ss"
         (prefix-in sql: "../sql/sql-lang.ss"))

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
  
  (define id-accessor-stx #f)             ; person-id
  (define saved-predicate-stx #f)         ; person-saved?
  (define vanilla-predicate-stx #f)       ; person-vanilla?
  (define local-predicate-stx #f)         ; person-local?
  (define pretty-formatter-stx #f)        ; format-person
  (define defaults-constructor-stx #f)    ; make-person/defaults
  (define copy-constructor-stx #f)        ; person-set
  (define deserialize-info-stx #f)        ; deserialize-info:person
  (define property-stxs null)             ; (... (cons prop:bar bar) (cons prop:foo foo))
  (define entity-kw-stxs null)            ; #:table-name 'Person ...
  
  ; Attributes:
  (define attr-stxs null)                 ; (gender age name)
  (define attr-private-stxs null)         ; (attr:person-gender attr:person-age attr:person-name attr:person-revision attr:person-guid)
  (define attr-type-stxs null)            ; (boolean integer person ...)
  (define attr-type-expr-stxs null)       ; ((make-symbol-type #t) (make-integer-type #f) ...)
  (define attr-default-stxs null)         ; (#t 123 ...)
  (define attr-kw-stxs null)              ; (#:person-gender #:person-age #:person-name #:person-revision #:person-guid)
  (define attr-column-stxs null)          ; ('gender 'age 'name)
  (define attr-accessor-stxs null)        ; (person-gender person-age person-name person-revision person-guid)
  (define attr-mutator-stxs null)         ; (set-person-gender! set-person-age! set-person-name! set-person-revision! set-person-guid!)
  (define attr-pretty-stxs null)
  (define attr-pretty-plural-stxs null)
  
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
              (set! id-accessor-stx             (make-id #'name #'name '-id))
              (set! saved-predicate-stx         (make-id #'name #'name '-saved?))
              (set! vanilla-predicate-stx       (make-id #'name #'name '-vanilla?))
              (set! local-predicate-stx         (make-id #'name #'name '-local?))
              (set! pretty-formatter-stx        (make-id #'name 'format- #'name))
              (set! defaults-constructor-stx    (make-id #'name 'make- #'name '/defaults))
              (set! copy-constructor-stx        (make-id #'name #'name '-set))
              (set! deserialize-info-stx        (make-id #'name 'deserialize-info: #'name '-v0))
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
    (define my-max-length-stx    #'#f)
    (define my-default-stx       #'(lambda () #f))
    (define my-type-stx          #f)
    (define my-type-expr-stx     #f)
    (define my-kw-stx            #f)
    (define my-accessor-stx      #f)
    (define my-mutator-stx       #f)
    (define my-column-stx        #f)
    (define my-pretty-stx        #'name->pretty-name)
    (define my-pretty-plural-stx #'pluralize-pretty-name)
    
    (define (parse-attr-type stx)
      (with-syntax ([allows-null? my-allows-null-stx])
        (set! my-type-stx stx)
        (syntax-case* stx (boolean integer real symbol string time-tai time-utc) symbolic-identifier=?
          [boolean  (set! my-type-expr-stx #'(make-boolean-type allows-null?))]
          [integer  (set! my-type-expr-stx #'(make-integer-type allows-null?))]
          [real     (set! my-type-expr-stx #'(make-real-type allows-null?))]
          [symbol   (set! my-type-expr-stx (with-syntax ([max-length my-max-length-stx])
                                             #'(make-symbol-type allows-null? max-length)))]
          [string   (set! my-type-expr-stx (with-syntax ([max-length my-max-length-stx])
                                             #`(make-string-type allows-null? max-length)))]
          [time-tai (set! my-type-expr-stx #'(make-time-tai-type allows-null?))]
          [time-utc (set! my-type-expr-stx #'(make-time-utc-type allows-null?))]
          [entity   (set! my-type-expr-stx #'(make-guid-type allows-null? entity))])))
    
    (define (parse-attr-kws stx)
      (syntax-case stx ()
        [(#:allow-null? val other ...)
         (begin (set! my-allows-null-stx #'val)
                (parse-attr-kws #'(other ...)))]
        [(#:max-length val other ...)
         (begin (set! my-max-length-stx #'val)
                (parse-attr-kws #'(other ...)))]
        [(#:default val other ...)
         (begin (set! my-default-stx #'(lambda () val))
                (parse-attr-kws #'(other ...)))]
        [(#:default-maker val other ...)
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
      (set! attr-mutator-stxs       (cons my-mutator-stx       attr-mutator-stxs))
      (set! attr-column-stxs        (cons my-column-stx        attr-column-stxs))
      (set! attr-pretty-stxs        (cons my-pretty-stx        attr-pretty-stxs))
      (set! attr-pretty-plural-stxs (cons my-pretty-plural-stx attr-pretty-plural-stxs)))
    
    (syntax-case stx ()
      [(name type arg ...)
       (and (identifier? #'name)
            (identifier? #'type))
       (begin (set! my-name-stx          #'name)
              (set! my-private-stx       (make-id #f 'attr: entity-stx '- #'name))
              (set! my-kw-stx            (datum->syntax #f (string->keyword (symbol->string (syntax->datum #'name)))))
              (set! my-accessor-stx      (make-id entity-stx entity-stx '- #'name))
              (set! my-mutator-stx       (make-id entity-stx 'set- entity-stx '- #'name '!))
              (set! my-column-stx        #'(name->database-name 'name))
              (parse-attr-kws #'(arg ...))
              (parse-attr-type #'type)
              (finish-attr))]))
  
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
                  
                  [id-accessor              id-accessor-stx]
                  [saved-predicate          saved-predicate-stx]
                  [vanilla-predicate        vanilla-predicate-stx]
                  [local-predicate          local-predicate-stx]
                  [pretty-formatter         pretty-formatter-stx]
                  [defaults-constructor     defaults-constructor-stx]
                  [copy-constructor         copy-constructor-stx]
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
                  [(attr-type-expr ...)                             (reverse attr-type-expr-stxs)]
                  [(attr-default ...)                               (reverse attr-default-stxs)]
                  [(attr-pretty ...)                                (reverse attr-pretty-stxs)]
                  [(attr-pretty-plural ...)                         (reverse attr-pretty-plural-stxs)]
                  [(guid-accessor revision-accessor accessor ...)   (list* (make-id entity-stx entity-stx '-guid)
                                                                           (make-id entity-stx entity-stx '-revision)
                                                                           (reverse attr-accessor-stxs))]
                  [(revision-mutator mutator ...)                   (list* (make-id entity-stx 'set- entity-stx '-revision!)
                                                                           (reverse attr-mutator-stxs))]
                  [(guid-column revision-column column ...)         (list* #''id
                                                                           #''revision
                                                                           (reverse attr-column-stxs))])
      (quasisyntax/loc entity-stx
        (begin (define-guid-type entity-guid)
               
               (define-values (entity-private struct-type constructor predicate)
                 (let* ([attr-names               (list 'attr ...)]
                        [attr-pretty-names        (map (lambda (proc arg) (proc arg))
                                                       (list attr-pretty ...)
                                                       attr-names)]
                        [attr-pretty-names-plural (map (lambda (proc arg) (proc arg))
                                                       (list attr-pretty-plural ...)
                                                       attr-pretty-names)]
                        [make-attr-types          (lambda (entity) (list attr-type-expr ...))])
                   (make-entity 'entity
                                attr-names
                                make-attr-types
                                (list attr-default ...)
                                entity-guid-constructor
                                entity-guid-predicate
                                #:attr-column-names        (list column ...)
                                #:attr-pretty-names        attr-pretty-names
                                #:attr-pretty-names-plural attr-pretty-names-plural
                                #:properties   
                                #,(if (eq? (syntax-local-context) 'module)
                                      (syntax/loc entity-stx
                                        (list (cons prop:serializable
                                                    (make-serialize-info
                                                     (lambda (struct) (list->vector (real:snooze-struct-ref* struct)))
                                                     (quote-syntax deserialize-info)
                                                     #t
                                                     (or (current-load-relative-directory) (current-directory))))
                                              property ...))
                                      (syntax/loc entity-stx
                                        (list property ...)))
                                entity-kw ...)))
               
               (set-box! (guid-entity-box entity-guid-struct-type) entity-private)
               
               (define-values (guid-private revision-private attr-private ...)
                 (apply values (entity-attributes entity-private)))
               
               (define-values (guid-accessor revision-accessor accessor ...)
                 (apply values (map attribute-cached-accessor (entity-attributes entity-private))))
               
               (define-values (revision-mutator mutator ...)
                 (apply values (map attribute-cached-mutator (cdr (entity-attributes entity-private)))))
               
               (define id-accessor snooze-struct-id)
               (define saved-predicate snooze-struct-saved?)
               (define vanilla-predicate guid-vanilla?)
               (define local-predicate guid-local?)
               (define pretty-formatter format-snooze-struct)
               
               (define (defaults-constructor
                         #:snooze   [snooze   (current-snooze)]
                         #,@(append-map (lambda (kw attr name)
                                          (list kw #`[#,name (attribute-default #:snooze snooze #,attr)]))
                                        (syntax->list #'(attr-kw ...))
                                        (syntax->list #'(attr-private ...))
                                        (syntax->list #'(attr ...))))
                 ((entity-cached-constructor entity-private) #:snooze snooze #f #f attr ...))
               
               (define (copy-constructor
                        original
                        #:snooze   [snooze   (guid-snooze original)]
                        #,@(append-map (lambda (kw accessor name)
                                         (list kw #`[#,name (#,accessor original)]))
                                       (syntax->list #'(attr-kw ...))
                                       (syntax->list #'(accessor ...))
                                       (syntax->list #'(attr ...))))
                 (let* ([struct   (guid-ref original)]
                        [guid     (real:snooze-struct-guid struct)]
                        [revision (real:snooze-struct-revision struct)])
                   ((entity-cached-constructor entity-private) #:snooze snooze guid revision attr ...)))
               
               #,(if (eq? (syntax-local-context) 'module)
                     (syntax/loc entity-stx
                       (begin (define deserialize-info
                                (make-deserialize-info
                                 ; maker
                                 (lambda args
                                   (let ([struct (apply (entity-private-constructor entity-private) args)])
                                     (send (send (current-snooze) get-current-cache) cache-add! struct)
                                     struct))
                                 ; cycle-maker
                                 (lambda ()
                                   (values defaults-constructor copy-constructor))))
                              (provide deserialize-info)))
                     #'(begin))
               
               (define default-alias (sql:alias 'entity entity-private))
               
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
                     (certify #'vanilla-predicate)
                     (certify #'local-predicate)
                     (certify #'pretty-formatter)
                     (certify #'defaults-constructor)
                     (certify #'copy-constructor)
                     (certify #'entity-guid)
                     (certify #'entity-guid-constructor)
                     (certify #'entity-guid-predicate)
                     (certify #'default-alias)
                     (list (make-attribute-info
                            (certify #'guid-attr)
                            (certify #'guid-private)
                            (certify #'entity)
                            (certify #'guid-accessor)
                            #f)
                           (make-attribute-info
                            (certify #'revision-attr)
                            (certify #'revision-private)
                            (certify #'integer)
                            (certify #'revision-accessor)
                            (certify #'revision-mutator))
                           (make-attribute-info
                            (certify #'attr)
                            (certify #'attr-private)
                            (certify #'attr-type)
                            (certify #'accessor)
                            (certify #'mutator))
                           ...)))))))))
  
  ; Main transformer body:
  
  (syntax-case complete-stx ()
    [(_ arg ...)
     (parse-name #'(arg ...))]))

; (_ struct-id)
(define-syntax entity-out
  (make-provide-transformer
   (lambda (stx modes)
     ; (listof export)
     (syntax-case stx ()
       [(_ id)
        (append (expand-export #'(struct-out id) modes)
                (expand-export #'(entity-extras-out id) modes))]))))

; (_ struct-id)
(define-syntax (provide-entity/contract stx)
  
  ;  (list a1 a2 ...)
  ;  (list b1 b2 ...)
  ;  ...
  ; ->
  ;  (list a1 b1 ... a2 b2 ...)
  (define (interleave . lists)
    (if (ormap null? lists)
        null
        (append (map car lists)
                (apply interleave (map cdr lists)))))
  
  (syntax-case stx ()
    [(_ id)
     (let* ([info      (entity-info-ref #'id)]
            [attr-info (entity-info-attribute-info info)])
       (with-syntax* ([struct-type          (entity-info-struct-type-id          info)]
                      [predicate            (entity-info-predicate-id            info)]
                      [guid-predicate       (entity-info-guid-predicate-id       info)]
                      [constructor          (entity-info-constructor-id          info)]
                      [id-accessor          (entity-info-id-accessor-id          info)]
                      [saved-predicate      (entity-info-saved-predicate-id      info)]
                      [vanilla-predicate    (entity-info-vanilla-predicate-id    info)]
                      [local-predicate      (entity-info-local-predicate-id      info)]
                      [pretty-formatter     (entity-info-pretty-formatter-id     info)]
                      [defaults-constructor (entity-info-defaults-constructor-id info)]
                      [copy-constructor     (entity-info-copy-constructor-id     info)]
                      [guid-accessor        (attribute-info-accessor-id (car attr-info))]
                      [guid-contract        #'(or/c predicate #f)]
                      [revision-accessor    (attribute-info-accessor-id (cadr attr-info))]
                      [revision-contract    #'(or/c natural-number/c #f)]
                      [([attr-kw attr-accessor attr-contract] ...)
                       (for/list ([info (in-list (cddr attr-info))])
                         (list (string->keyword (symbol->string (syntax->datum (attribute-info-id info))))
                               (attribute-info-accessor-id info)
                               (syntax-case* (attribute-info-type-id info) (boolean integer real symbol string time-tai time-utc) symbolic-identifier=?
                                 [boolean  #'boolean?]
                                 [integer  #'(or/c integer? #f)]
                                 [real     #'(or/c number? #f)]
                                 [symbol   #'(or/c symbol? #f)]
                                 [string   #'(or/c string? #f)]
                                 [time-tai #'(or/c time-tai? #f)]
                                 [time-utc #'(or/c time-utc? #f)]
                                 [entity   #`(or/c #,(entity-info-guid-predicate-id (entity-info-ref #'entity)) #f)])))])
         (quasisyntax/loc stx
           (begin (provide id)
                  (provide/contract
                   [struct-type          struct-type?]
                   [predicate            (-> any/c boolean?)]
                   [constructor          (->* (attr-contract ...)
                                              (#:snooze (is-a?/c snooze<%>))
                                              guid-predicate)]
                   [id-accessor          (-> guid-predicate (or/c natural-number/c #f))]
                   [saved-predicate      (-> guid-predicate boolean?)]
                   [vanilla-predicate    (-> guid-predicate boolean?)]
                   [local-predicate      (-> guid-predicate boolean?)]
                   [pretty-formatter     (->* (guid-predicate) () #:rest any/c string?)]
                   [defaults-constructor (->* ()
                                              (#:snooze (is-a?/c snooze<%>)
                                                        #,@(interleave (syntax->list #'(attr-kw ...))
                                                                       (syntax->list #'(attr-contract ...))))
                                              guid-predicate)]
                   [copy-constructor     (->* (guid-predicate)
                                              (#:snooze (is-a?/c snooze<%>)
                                                        #,@(interleave (syntax->list #'(attr-kw ...))
                                                                       (syntax->list #'(attr-contract ...))))
                                              guid-predicate)]
                   [guid-accessor        (-> guid-predicate guid-contract)]
                   [revision-accessor    (-> guid-predicate revision-contract)]
                   [attr-accessor        (-> guid-predicate attr-contract)]
                   ...)))))]))

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
          (map create-export (list (entity-info-id-accessor-id          info)
                                   (entity-info-saved-predicate-id      info)
                                   (entity-info-vanilla-predicate-id    info)
                                   (entity-info-local-predicate-id      info)
                                   (entity-info-pretty-formatter-id     info)
                                   (entity-info-defaults-constructor-id info)
                                   (entity-info-copy-constructor-id     info))))]))))

; (listof (cons property any)) -> boolean
(define (reserved-properties? prop-alist)
  (ormap (lambda (prop)
           (or (eq? prop prop:entity)
               (eq? prop prop:serializable)))
         (map car prop-alist)))

; Provide statements -----------------------------

(provide define-entity
         entity-out
         provide-entity/contract
         attr)
