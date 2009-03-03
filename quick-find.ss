#lang scheme/base

(require (for-syntax scheme/base
                     scheme/match
                     srfi/26/cut
                     (planet untyped/unlib:3/syntax)
                     "base.ss"
                     "persistent-struct-info.ss")
         scheme/class
         "quick-find-internal.ss"
         "sql/sql.ss")

; Helpers ----------------------------------------

; syntax boolean syntax syntax syntax syntax -> syntax
(define-for-syntax (make-quick-find stx count? struct-stx snooze-stx method-stx order-stxs)
  (let* ([info       ; persistent-struct-info
          (with-handlers ([exn? (lambda (exn) 
                                  (raise-syntax-error #f "not a persistent struct" stx struct-stx))])
            (persistent-struct-info-ref struct-stx))]
         
         [entity-stx ; syntax  e.g. entity:struct
          (persistent-struct-info-entity-id info)]
         
         [attr-stxs  ; (listof syntax)  e.g.  (list attr:attr1 atr:attr2 ...)
          (persistent-struct-info-attribute-ids info)]
         
         [key-stxs ; (listof syntax)  e.g.  (list #:attr1 #:attr2 ...)
          (map (lambda (sym)
                 (string->keyword (symbol->string sym)))
               (persistent-struct-info-attribute-names info))]
         
         ; (listof syntax)  e.g.  (list attr1 attr2 ...)
         [arg-stxs
          (map (cut datum->syntax #f <>) 
               (persistent-struct-info-attribute-names info))]
         
         ; (listof syntax)  e.g.  (list #:attr1 [attr1 (void)] #:attr2 [attr2 (void)] ...)
         [key+arg-stxs
          (let loop ([key-stxs key-stxs] [arg-stxs arg-stxs])
            (if (null? key-stxs)
                null
                (list* (car key-stxs)
                       #`(#,(car arg-stxs) (void))
                       (loop (cdr key-stxs) (cdr arg-stxs)))))])
    
    (with-syntax ([struct              struct-stx]
                  [struct-id           (make-id struct-stx struct-stx '-id)]
                  [snooze              snooze-stx]
                  [find-whatever       method-stx]
                  [entity              entity-stx]
                  [(attr ...)          attr-stxs]
                  [(key ...)           key-stxs]
                  [(arg ...)           arg-stxs]
                  [(key+arg ...)       key+arg-stxs]
                  [(default-order ...) order-stxs])
      (with-syntax ([what (if count? #'(count struct-id) #'struct)])
        (syntax/loc stx
          (let-alias ([struct struct])
            (lambda (key+arg ... #:limit [limit  #f] #:offset [offset #f])
              (send snooze find-whatever
                    (sql (select #:what   what
                                 #:from   struct
                                 #:where  ,(sql:and (or (void? arg)
                                                        (quick-find-expression
                                                         (sql:attr struct attr) 
                                                         arg))
                                                    ...)
                                 #:order  (default-order ...)
                                 #:limit  ,limit
                                 #:offset ,offset))))))))))

; syntax syntax -> syntax
(define-for-syntax (parse-kws stx kw-stx)
  ; (listof syntax)
  (define order-stxs null)
  
  (let loop ([kw-stx kw-stx])
    (syntax-case kw-stx ()
      [()
       (values order-stxs)]
      [(kw)
       (if (keyword? (syntax->datum #'kw))
           (raise-syntax-error #f "no value for keyword" stx #'kw)
           (raise-syntax-error #f "not a valid keyword" stx #'kw))]
      [(#:order val rest ...)
       (begin (set! order-stxs (syntax->list #'val))
              (loop #'(rest ...)))]
      [(_ val rest ...)
       (raise-syntax-error #f "not a valid keyword" stx #'kw)])))

; Syntax -----------------------------------------

; (_ struct-id snooze) -> (#:attr any ... -> (U persistent-struct #f))
(define-syntax (custom-find-count stx)
  (syntax-case stx ()
    [(_ struct snooze kw ...) 
     (make-quick-find stx #t #'struct #'snooze #'find-one null)]))

; (_ struct-id snooze) -> (#:attr any ... -> (U persistent-struct #f))
(define-syntax (custom-find-one stx)
  (syntax-case stx ()
    [(_ struct snooze kw ...) 
     (let ([order-stxs (parse-kws stx #'(kw ...))])
       (make-quick-find stx #f #'struct #'snooze #'find-one order-stxs))]))

; (_ struct-id snooze) -> (#:attr any ... -> (listof persistent-struct))
(define-syntax (custom-find-all stx)
  (syntax-case stx ()
    [(_ struct snooze kw ...) 
     (let ([order-stxs (parse-kws stx #'(kw ...))])
       (make-quick-find stx #f #'struct #'snooze #'find-all order-stxs))]))

; (_ struct-id snooze) -> (#:attr any ... -> (gen-> persistent-struct))
(define-syntax (custom-g:find stx)
  (syntax-case stx ()
    [(_ struct snooze kw ...)
     (let ([order-stxs (parse-kws stx #'(kw ...))])
       (make-quick-find stx #f #'struct #'snooze #'g:find order-stxs))]))

; Provide statements -----------------------------

(provide custom-find-count
         custom-find-one
         custom-find-all
         custom-g:find)
