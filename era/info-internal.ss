#lang scheme/base

(require "../base.ss")

(require (for-syntax scheme/base
                     (unlib-in syntax))
         scheme/contract
         scheme/struct-info)

(define-syntax (define/provide-info-struct stx)
  (syntax-case stx ()
    [(_ info (field ...))
     #'(define/provide-info-struct info (field ...) #f)]
    [(_ info (field ...) proc-field)
     (with-syntax ([struct:info    (make-id #'info 'struct: #'info)]
                   [make-info      (make-id #'info 'make- #'info)]
                   [info?          (make-id #'info #'info '?)]
                   [info-ref       (make-id #'info #'info '-ref)]
                   [info-set!      (make-id #'info #'info '-set!)]
                   [num-fields     (length (syntax->list #'(field ...)))]
                   [(accessor ...) (for/list ([field-stx (in-list (syntax->list #'(field ...)))])
                                          (make-id #'info #'info '- field-stx))])
       (with-syntax ([proc (let ([proc-field (syntax->datum #'proc-field)])
                             (if proc-field
                                 (with-syntax ([accessor
                                                (or (for/or ([field    (in-list (map syntax->datum (syntax->list #'(field ...))))]
                                                             [accessor (in-list (syntax->list #'(accessor ...)))])
                                                      (and (eq? proc-field field) accessor))
                                                    (raise-syntax-error #f "proc-field must be a member of fields" stx #'proc-field))])
                                   #'(lambda (info stx)
                                       (syntax-case stx ()
                                         [id (identifier? #'id) (accessor info)])))
                                 #'#f))])
         #'(begin (define-values (make-info info? accessor ...)
                     (letrec-values ([(struct:info make-info info? info-ref info-set!)
                                      (make-struct-type 'info struct:struct-info num-fields 0 #f null #f proc)]
                                     [(accessor ...)
                                      (apply values (for/list ([index (in-naturals)]
                                                               [name  (in-list '(field ...))])
                                                      (make-struct-field-accessor info-ref index name)))])
                       (values make-info info? accessor ...)))
                   (provide make-info info? accessor ...))))]))

; Provide statements -----------------------------

(provide define/provide-info-struct)