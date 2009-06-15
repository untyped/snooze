#lang scheme/base

(require (for-syntax scheme/base
                     scheme/match
                     srfi/26/cut
                     (planet untyped/unlib:3/syntax)
                     "base.ss"
                     "core/syntax-info.ss")
         scheme/class
         "quick-find-internal.ss"
         "snooze-api.ss"
         "sql/sql.ss")

; Helpers ----------------------------------------

; syntax boolean syntax syntax syntax syntax -> syntax
(define-for-syntax (make-quick-find stx count? struct-stx method-stx order-stxs)
  (let* (; entity-info
         [info             (with-handlers ([exn? (lambda (exn) 
                                                   (raise-syntax-error #f "not a persistent struct" stx struct-stx))])
                             (entity-info-ref struct-stx))]
         ; syntax  e.g. entity:struct
         [entity-stx       (entity-info-id info)]
         ; (listof syntax)  e.g.  (list attr:attr1 atr:attr2 ...)
         [attr-stxs        (map attribute-info-private-id (entity-info-attribute-info info))]
         ; (listof syntax)  e.g.  (list #:attr1 #:attr2 ...)
         [key-stxs         (map (compose string->keyword symbol->string syntax->datum attribute-info-id)
                                (entity-info-attribute-info info))]
         ; (listof syntax)  e.g.  (list attr1 attr2 ...)
         [arg-stxs         (map attribute-info-id (entity-info-attribute-info info))]
         ; (listof syntax)  e.g.  (list attr:attr1 atr:attr2 ...)
         [entity.attr-stxs (map (cut make-id stx entity-stx '|.| <>) arg-stxs)]
         ; (listof syntax)  e.g.  (list #:attr1 [attr1 (void)] #:attr2 [attr2 (void)] ...)
         [key+arg-stxs     (let loop ([key-stxs key-stxs] [arg-stxs arg-stxs])
                             (if (null? key-stxs)
                                 null
                                 (list* (car key-stxs)
                                        #`(#,(car arg-stxs) (void))
                                        (loop (cdr key-stxs) (cdr arg-stxs)))))])
    
    (with-syntax ([struct            struct-stx]
                  [struct.guid       (make-id struct-stx struct-stx '.guid)]
                  [find-whatever     method-stx]
                  [entity            entity-stx]
                  [proc              (make-id struct-stx 'custom- method-stx '/ struct-stx)]
                  [(attr ...)        attr-stxs]
                  [(entity.attr ...) entity.attr-stxs]
                  [(key ...)         key-stxs]
                  [(arg ...)         arg-stxs]
                  [(key+arg ...)     key+arg-stxs]
                  [(*order* ...)     order-stxs])
      (quasisyntax/loc stx
        (let-alias ([struct struct])
           (letrec ([default-what #,(if count?
                                        #'(sql (count struct.guid))
                                        #'(sql struct))]
                    [default-order (list (sql *order*) ...)]
                    [proc          (lambda (key+arg ...
                                            #:snooze [snooze (current-snooze)]
                                            #:what   [what   #f]
                                            #:order  [order  #f]
                                            #:limit  [limit  #f]
                                            #:offset [offset #f])
                                     (find-whatever #:snooze snooze
                                                    (sql (select #:what   ,(or what default-what)
                                                                 #:from   struct
                                                                 #:where  ,(sql:and (or (void? arg)
                                                                                        (quick-find-expression
                                                                                         (sql entity.attr) 
                                                                                         arg))
                                                                                    ...)
                                                                 #:order  ,(or order default-order)
                                                                 #:limit  ,limit
                                                                 #:offset ,offset))))])
             proc))))))

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

; (_ struct-id snooze) -> (#:attr any ... -> (U snooze-struct #f))
(define-syntax (custom-find-count stx)
  (syntax-case stx ()
    [(_ struct kw ...) 
     (make-quick-find stx #t #'struct #'find-one null)]))

; (_ struct-id snooze) -> (#:attr any ... -> (U snooze-struct #f))
(define-syntax (custom-find-one stx)
  (syntax-case stx ()
    [(_ struct kw ...) 
     (let ([order-stxs (parse-kws stx #'(kw ...))])
       (make-quick-find stx #f #'struct #'find-one order-stxs))]))

; (_ struct-id snooze) -> (#:attr any ... -> (listof snooze-struct))
(define-syntax (custom-find-all stx)
  (syntax-case stx ()
    [(_ struct kw ...) 
     (let ([order-stxs (parse-kws stx #'(kw ...))])
       (make-quick-find stx #f #'struct #'find-all order-stxs))]))

; (_ struct-id snooze) -> (#:attr any ... -> (gen-> snooze-struct))
(define-syntax (custom-g:find stx)
  (syntax-case stx ()
    [(_ struct kw ...)
     (let ([order-stxs (parse-kws stx #'(kw ...))])
       (make-quick-find stx #f #'struct #'g:find order-stxs))]))

; Provide statements -----------------------------

(provide custom-find-count
         custom-find-one
         custom-find-all
         custom-g:find)
