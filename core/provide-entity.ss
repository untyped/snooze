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
         (except-in "struct.ss" make-entity)
         (prefix-in sql: "../sql/sql-lang.ss")
         "syntax-info.ss")

; Helpers ----------------------------------------

; Returns two lists of clauses:
;   - a list to put in a provide statement;
;   - a list to put in a provide/contract statement.
;
; syntax -> (listof syntax) (listof syntax)
(define-for-syntax (entity-provide/contract-clauses id-stx)
  
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
  
  (let* ([info      (entity-info-ref id-stx)]
         [attr-info (entity-info-attribute-info info)])
    (with-syntax* ([struct-type          (entity-info-struct-type-id          info)]
                   [predicate            (entity-info-predicate-id            info)]
                   [guid-predicate       (entity-info-guid-predicate-id       info)]
                   [constructor          (entity-info-constructor-id          info)]
                   [id-accessor          (entity-info-id-accessor-id          info)]
                   [saved-predicate      (entity-info-saved-predicate-id      info)]
                   [pretty-formatter     (entity-info-pretty-formatter-id     info)]
                   [defaults-constructor (entity-info-defaults-constructor-id info)]
                   [copy-constructor     (entity-info-copy-constructor-id     info)]
                   [find-one             (entity-info-find-one-id             info)]
                   [find-all             (entity-info-find-all-id             info)]
                   [find-count           (entity-info-find-count-id           info)]
                   [g:find               (entity-info-g:find-id               info)]
                   [guid-accessor        (attribute-info-accessor-id (car attr-info))]
                   [guid-contract        #'(or/c predicate #f)]
                   [revision-accessor    (attribute-info-accessor-id (cadr attr-info))]
                   [revision-contract    #'(or/c natural-number/c #f)]
                   [([guid-kw     guid-accessor     guid-contract]
                     [revision-kw revision-accessor revision-contract]
                     [attr-kw     attr-accessor     attr-contract] ...)
                    (for/list ([info (in-list attr-info)])
                      (list (string->keyword (symbol->string (syntax->datum (attribute-info-id info))))
                            (attribute-info-accessor-id info)
                            #`(type-contract (attribute-type #,(attribute-info-private-id info)))))]
                   [([attr-guid-accessor attr-guid-contract] ...)
                    (for/fold ([accum null])
                              ([info (in-list attr-info)])
                              (if (attribute-info-guid-accessor-id info)
                                  (with-syntax ([id       (attribute-info-guid-accessor-id info)]
                                                [contract #`(or/c (entity-guid-predicate (guid-type-entity (attribute-type #,(attribute-info-private-id info)))) #f)])
                                    (cons (list #'id #'contract) accum))
                                  accum))]
                   [(make-kw-arg ...)    (interleave (syntax->list #'(attr-kw ...))
                                                     (syntax->list #'(attr-contract ...)))]
                   [(find-kw-arg ...)    (interleave (syntax->list #'(guid-kw revision-kw attr-kw ...))
                                                     (syntax->list #'((or/c guid-contract
                                                                            (listof guid-contract)
                                                                            procedure?
                                                                            void?)
                                                                      (or/c revision-contract
                                                                            (listof revision-contract)
                                                                            procedure?
                                                                            void?)
                                                                      (or/c attr-contract
                                                                            (listof attr-contract)
                                                                            procedure?
                                                                            void?)
                                                                      ...)))])
      (values
       ; provide
       (list id-stx)
       ; provide/contract
       (syntax->list
        (quasisyntax/loc id-stx
          ([struct-type          struct-type?]
           [predicate            (-> any/c boolean?)]
           [constructor          (-> attr-contract ... predicate)]
           [id-accessor          (-> predicate (or/c natural-number/c #f))]
           [saved-predicate      (-> predicate boolean?)]
           [pretty-formatter     (->* (predicate) () #:rest any/c string?)]
           [defaults-constructor (->* ()          (make-kw-arg ...) predicate)]
           [copy-constructor     (->* (predicate) (make-kw-arg ...) predicate)]
           [find-one             (->* () (#:snooze (is-a?/c snooze<%>)
                                                   find-kw-arg ...
                                                   #:what   sql:select-what/c
                                                   #:where  sql:select-where/c
                                                   #:order  sql:select-order/c
                                                   #:group  sql:select-group/c
                                                   #:limit  sql:select-limit/c
                                                   #:offset sql:select-offset/c)
                                      (or/c predicate #f))]
           [find-all             (->* () (#:snooze (is-a?/c snooze<%>)
                                                   find-kw-arg ...
                                                   #:what   sql:select-what/c
                                                   #:where  sql:select-where/c
                                                   #:order  sql:select-order/c
                                                   #:group  sql:select-group/c
                                                   #:limit  sql:select-limit/c
                                                   #:offset sql:select-offset/c)
                                      (listof predicate))]
           [find-count           (->* () (#:snooze (is-a?/c snooze<%>)
                                                   find-kw-arg ...
                                                   #:what   sql:select-what/c
                                                   #:where  sql:select-where/c
                                                   #:order  sql:select-order/c
                                                   #:group  sql:select-group/c
                                                   #:limit  sql:select-limit/c
                                                   #:offset sql:select-offset/c)
                                      natural-number/c)]
           [g:find               (->* () (#:snooze (is-a?/c snooze<%>)
                                                   find-kw-arg ...
                                                   #:what   sql:select-what/c
                                                   #:where  sql:select-where/c
                                                   #:order  sql:select-order/c
                                                   #:group  sql:select-group/c
                                                   #:limit  sql:select-limit/c
                                                   #:offset sql:select-offset/c)
                                      procedure?)]
           [attr-accessor        (-> predicate attr-contract)]
           ...
           [attr-guid-accessor   (-> predicate attr-guid-contract)]
           ...)))))))

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
                                   (entity-info-pretty-formatter-id     info)
                                   (entity-info-defaults-constructor-id info)
                                   (entity-info-copy-constructor-id     info)
                                   (entity-info-find-one-id             info)
                                   (entity-info-find-all-id             info)
                                   (entity-info-find-count-id           info)
                                   (entity-info-g:find-id               info))))]))))

; Syntax -----------------------------------------

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
(define-syntax (provide-entity/contract complete-stx)
  (syntax-case complete-stx ()
    [(_ id)
     (let-values ([(provides provide/contracts) (entity-provide/contract-clauses #'id)])
       (quasisyntax/loc complete-stx
         (begin (provide #,@(reverse provides))
                (provide/contract #,@(reverse provide/contracts)))))]))

; (_ struct-id)
(define-syntax (provide/contract/entities complete-stx)
  
  ; (listof syntax) [(listof syntax)] [(listof syntax)] -> (listof syntax) (listof syntax)
  (define (fold-clauses clauses [provide-accum null] [provide/contract-accum null])
    (match clauses
      [(list) (values provide-accum
                      provide/contract-accum)]
      [(list-rest stx rest)
       (let-values ([(provides provide/contracts)
                     (syntax-case* stx (entity) symbolic-identifier=?
                       [(entity id) (entity-provide/contract-clauses #'id)]
                       [clause      (values null (list #'clause))])])
         (fold-clauses rest
                       (append (reverse provides) provide-accum)
                       (append (reverse provide/contracts) provide/contract-accum)))]))
  
  (syntax-case complete-stx ()
    [(_ clause ...)
     (let-values ([(provides provide/contracts) (fold-clauses (syntax->list #'(clause ...)))])
       (quasisyntax/loc complete-stx
         (begin (provide #,@(reverse provides))
                (provide/contract #,@(reverse provide/contracts)))))]))

; Provide statements -----------------------------

(provide entity-out
         #;provide-entity/contract
         provide/contract/entities)
