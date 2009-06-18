#lang scheme/base

(require "../base.ss")

(require (planet untyped/unlib:3/syntax)
         (for-template scheme/base
                       scheme/class
                       "../sql/sql.ss"
                       "quick-find-internal.ss"
                       "struct.ss"))

;  syntax
;  (list syntax syntax syntax syntax)
;  syntax
;  (listof syntax)
;  syntax
; ->
;  syntax
(define (make-quick-finds stx proc-stxs alias-stx attr-stxs order-stx)
  (match proc-stxs
    [(list find-one-stx find-all-stx find-count-stx g:find-stx)
     #`(values #,(make-quick-find stx #f find-one-stx   #'find-one alias-stx attr-stxs order-stx)
               #,(make-quick-find stx #f find-all-stx   #'find-all alias-stx attr-stxs order-stx)
               #,(make-quick-find stx #t find-count-stx #'find-one alias-stx attr-stxs order-stx)
               #,(make-quick-find stx #f g:find-stx     #'g:find   alias-stx attr-stxs order-stx))]))

;  syntax
;  boolean
;  syntax
;  syntax
;  syntax
;  (listof syntax)
;  syntax
; ->
;  syntax
(define (make-quick-find stx count? proc-stx method-stx alias-stx attr-stxs order-stx)
  (let* ([key-stxs         (map (compose string->keyword symbol->string syntax->datum) attr-stxs)]
         [key+arg-stxs     (let loop ([key-stxs key-stxs] [arg-stxs attr-stxs])
                             (if (null? key-stxs)
                                 null
                                 (list* (car key-stxs)
                                        #`(#,(car arg-stxs) (void))
                                        (loop (cdr key-stxs) (cdr arg-stxs)))))])
    (with-syntax ([proc              proc-stx]
                  [find-whatever     method-stx]
                  [*entity*          alias-stx]
                  [entity.guid       (make-id #f 'entity.guid)]
                  [(entity.attr ...) (map (cut make-id #f 'entity. <>) attr-stxs)]
                  [(attr ...)        attr-stxs]
                  [(key ...)         key-stxs]
                  [(arg ...)         attr-stxs]
                  [(key+arg ...)     key+arg-stxs]
                  [(*order* ...)     order-stx])
      (quasisyntax/loc stx
        (let-sql ([entity *entity*])
          (letrec ([default-what #,(if count?
                                       #'(sql (count entity.guid))
                                       #'(sql entity))]
                   [default-order (sql-list *order* ...)]
                   [proc          (lambda (key+arg ...
                                           #:snooze [snooze (current-snooze)]
                                           #:what   [what   #f]
                                           #:order  [order  #f]
                                           #:limit  [limit  #f]
                                           #:offset [offset #f])
                                    (send snooze find-whatever
                                          (sql (select
                                                #:what   ,(or what default-what)
                                                #:from   entity
                                                #:where  ,(sql:and (or (void? arg)
                                                                       (quick-find-expression
                                                                        (sql entity.attr) 
                                                                        arg))
                                                                   ...)
                                                #:order  ,(or order default-order)
                                                #:limit  ,limit
                                                #:offset ,offset))))])
            proc))))))

; Provide statements -----------------------------

(provide/contract
 [make-quick-finds (-> syntax?
                       (list/c syntax? syntax? syntax? syntax?)
                       syntax?
                       (listof syntax?)
                       syntax?
                       syntax?)]
 [make-quick-find  (-> syntax?
                       boolean?
                       syntax?
                       syntax?
                       syntax?
                       (listof syntax?)
                       syntax?
                       syntax?)])
