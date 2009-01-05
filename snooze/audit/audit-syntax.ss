#lang scheme/base

(require (for-syntax scheme/base
                     scheme/class
                     scheme/provide-transform
                     srfi/26/cut
                     (planet untyped/unlib:3/syntax)
                     (file "audit-class.ss"))
         scheme/class)

; syntax symbol (U symbol #f) -> (listof syntax)
(define-for-syntax (audit-ids stx prefix)
  (map (if prefix
           (cut make-id stx prefix <>)
           (cut make-id stx <>))
       (interface->method-names audit-trail<%>)))

; syntax (U symbol #f) syntax -> syntax
(define-for-syntax (prefixed-id stx prefix id)
  (if prefix
      (make-id stx prefix id)
      (make-id stx id)))

; syntax (U symbol #f) syntax -> syntax
(define-for-syntax (make-audit-interface stx prefix audit)
  (let ([procedure-ids       (audit-ids audit prefix)]
        [method-ids          (audit-ids stx '||)])
    (with-syntax ([audit audit])
      #`(define-values (#,@procedure-ids)
          (values #,@(map (lambda (method-id)
                            #`(lambda args
                                (send/apply audit #,method-id args)))
                          method-ids))))))

; (_ [prefix-id] audit-object)
(define-syntax (define-audit-interface stx)
  (syntax-case stx ()
    [(define-audit-interface audit)
     (make-audit-interface stx #f #'audit)]
    [(define-audit-interface prefix audit)
     (identifier? #'prefix)
     (make-audit-interface stx (syntax->datum #'prefix) #'audit)]))

; (_ [prefix-id])
(define-syntax audit-interface-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-case stx ()
       [(_)
        (let ([procedure-ids       (audit-ids stx '||)]
              [with-connection-id  (prefixed-id stx '|| 'with-connection)]
              [with-transaction-id (prefixed-id stx '|| 'with-transaction)])
          (map (lambda (id)
                 (make-export id (syntax->datum id) 0 #f id))
               procedure-ids))]
       [(_ prefix)
        (let ([procedure-ids       (audit-ids stx #'prefix)]
              [with-connection-id  (prefixed-id stx #'prefix 'with-connection)]
              [with-transaction-id (prefixed-id stx #'prefix 'with-transaction)])
          (map (lambda (id)
                 (make-export id (syntax->datum id) 0 #f id))
               procedure-ids))]))))

; (_)
(define-syntax (provide-audit-interface stx)
  (syntax-case stx () 
    [(provide-audit-interface)
     #`(begin (provide #,@(audit-ids stx '||)))]
    [(provide-audit-interface prefix)
     (identifier? #'prefix)
     #`(begin (provide #,@(audit-ids stx (syntax->datum #'prefix))))]))

; Provide statements -----------------------------

(provide define-audit-interface
         provide-audit-interface
         audit-interface-out)
