#lang scheme/base

(require (for-syntax scheme/base
                     scheme/class
                     scheme/provide-transform
                     srfi/26/cut
                     (planet untyped/unlib:3/syntax)
                     "snooze-interface.ss")
         scheme/class)

; syntax symbol (U symbol #f) -> (listof syntax)
(define-for-syntax (snooze-ids stx prefix)
  (map (if prefix
           (cut make-id stx prefix <>)
           (cut make-id stx <>))
       (interface->method-names snooze<%>)))

; syntax (U symbol #f) syntax -> syntax
(define-for-syntax (prefixed-id stx prefix id)
  (if prefix
      (make-id stx prefix id)
      (make-id stx id)))

; syntax (U symbol #f) syntax -> syntax
(define-for-syntax (make-snooze-interface stx prefix-stx snooze-stx)
  (let ([procedure-ids (snooze-ids snooze-stx prefix-stx)]
        [method-ids    (snooze-ids stx '||)])
    (with-syntax ([snooze-in snooze-stx])
      ; lots of procedures ...
      #`(define-values (#,@procedure-ids)
          (let ([snooze snooze-in])
            (values #,@(map (lambda (method-id)
                              #`(lambda args
                                  (send/apply snooze #,method-id args)))
                            method-ids)))))))

; syntax (_ [prefix-id] snooze-object)
(define-syntax (define-snooze-interface stx)
  (syntax-case stx ()
    [(define-snooze-interface snooze)
     (make-snooze-interface stx #f #'snooze)]
    [(define-snooze-interface prefix snooze)
     (identifier? #'prefix)
     (make-snooze-interface stx (syntax->datum #'prefix) #'snooze)]))

; syntax (_ [prefix-id])
(define-syntax snooze-interface-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-case stx ()
       [(_)
        (map (lambda (id)
               (make-export id (syntax->datum id) 0 #f id))
             (snooze-ids stx '||))]
       [(_ prefix)
        (map (lambda (id)
               (make-export id (syntax->datum id) 0 #f id))
             (snooze-ids stx #'prefix))]))))

; syntax (provide-snooze-interface)
(define-syntax (provide-snooze-interface stx)
  (syntax-case stx () 
    [(provide-snooze-interface)
     #`(provide #,@(snooze-ids stx '||))]
    [(provide-snooze-interface prefix)
     (identifier? #'prefix)
     #`(provide #,@(snooze-ids stx (syntax->datum #'prefix)))]))

; Provide statements -----------------------------

(provide define-snooze-interface
         provide-snooze-interface
         snooze-interface-out)
