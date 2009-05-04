#lang scheme/base

(require (planet untyped/unlib:3/symbol)
         "../era/info.ss"
         (for-template scheme/base))

; symbol
;
; Used to mark (using syntax-property) the hygienically introduced "private" identifiers for aliases.
(define private-sql-identifier-key
  'snooze-sql-identifier)

; [symbol] -> syntax
;
; Returns a hygienically introduced "private" alias identifier, that can be bound to an alias value
; and referred to from within an alias transformer procedure.
(define (make-private-sql-identifier [prefix #f])
  (define private 
    (if prefix
        (datum->syntax #f (symbol-append prefix '-private))
        #'private))
  (syntax-property private private-sql-identifier-key #t))

; syntax -> boolean
;
; Returns #t if the supplied identifier has the private-sql-identifier-key
; syntax-property set to #t.
(define (private-sql-identifier? stx)
  (and (member private-sql-identifier-key (syntax-property-symbol-keys stx))
       (eq? (syntax-property stx private-sql-identifier-key) #t)))

; syntax -> syntax
(define (make-sql-transformer secret-binding-stx)
  (with-syntax ([secret-binding secret-binding-stx])
    #'(case-lambda
        [(stx) #'secret-binding]
        [()    #'secret-binding])))

; syntax -> boolean
;
; Returns #t id stx is an identifier bound with define-alias. These identifiers are bound to 
; procedures in the transformer environment, that return hygienically introduced "private"
; identifiers bound to the run-time value of the alias (you're right - it *is* a tad complex). 
; The private identifiers have the sql-identifier-key syntax-property set to #t.
(define (sql-identifier? stx)
  (and (identifier? stx)
       (with-handlers ([exn? (lambda _ #f)])
         (let ([proc (syntax-local-value stx #f)])
           (and (procedure? proc)
                (private-sql-identifier? (proc)))))))

; syntax -> boolean
(define (self-quoting-literal? stx)
  (define datum (syntax->datum stx))
  (or (boolean? datum)
      (integer? datum)
      (real? datum)
      (string? datum)
      (and (pair? datum)
           (memq (car datum) '(quote quasiquote)))))

; syntax -> boolean
(define (entity-identifier? stx)
  (and (identifier? stx)
       (entity-info-set? stx)))

; Provide statements -----------------------------

(provide entity-identifier?
         make-private-sql-identifier
         make-sql-transformer
         sql-identifier?
         self-quoting-literal?)
