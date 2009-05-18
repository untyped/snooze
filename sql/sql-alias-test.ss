#lang scheme/base

(require (for-syntax scheme/base
                     "sql-syntax-util.ss")
         "../test-base.ss"
         "../era/era.ss"
         "sql.ss")

; Helpers ----------------------------------------

(define-syntax (check-identifier stx)
  (syntax-case stx ()
    [(_ id)
     (if (sql-identifier? #'id)
         #'(void)
         #`(fail (format "Not an SQL identifier: ~a" #,(syntax->datum #'(quote id)))))]))

; Tests ------------------------------------------

; test-suite
(define sql-alias-tests
  (test-suite "sql-alias.ss"
    
    (test-case "define-sql"
      (define-sql a 1)
      (check-identifier a)
      (check-equal? a 1 "value"))
    
    (test-case "let-sql"
      (let-sql ([a 1] [b 2] [c 3])
        
        (check-identifier a)
        (check-identifier b)
        (check-identifier c)
        
        (check-equal? a 1 "a value")
        (check-equal? b 2 "b value")
        (check-equal? c 3 "c value")))
    
    (test-case "define-alias : entity"
      (define a* (sql:alias 'a person))
      (define b* (sql:alias 'b pet))
      
      (define-alias a person)
      (define-alias b pet)
      (check-identifier a)
      (check-identifier b)
      (check-equal? a a*)
      (check-equal? b b*))
    
    (test-case "let-alias : entity"
      (define a* (sql:alias 'a person))
      (define b* (sql:alias 'b pet))
      (let-alias ([a person] [b pet])
        (check-identifier a)
        (check-identifier b)
        (check-equal? a a*)
        (check-equal? b b*)))
    
    (test-case "define-alias : query"
      (define-alias a person)
      (define query (sql:select #:from a))
      (define-alias b query)
      (check-identifier b)
      (check-equal? b (sql:alias 'b query)))
    
    (test-case "let-alias : query"
      (define-alias a person)
      (define query (sql:select #:from a))
      (let-alias ([b query])
        (check-identifier b)
        (check-equal? b (sql:alias 'b query))))
    
    (test-case "define-alias : expression"
      (define-alias a person)
      (define expr (sql:= (sql a.guid) 123))
      (define-alias b expr)
      (check-identifier b)
      (check-equal? b (sql:alias 'b expr)))
    
    (test-case "let-alias : expression"
      (define-alias a person)
      (define expr (sql:= (sql a.guid) 123))
      (let-alias ([b expr])
        (check-identifier b)
        (check-equal? b (sql:alias 'b expr))))))

; Provide statements -----------------------------

(provide sql-alias-tests)
