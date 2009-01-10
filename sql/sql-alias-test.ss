#lang scheme/base

(require (for-syntax scheme/base
                     (file "sql-syntax-util.ss"))
         mzlib/etc
         (file "../test-base.ss")
         (file "../test-data.ss")
         (file "sql-alias.ss")
         (prefix-in sql: (file "sql-lang.ss")))

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
      (begin-with-definitions
        (define-sql a 1)
        (check-identifier a)
        (check-equal? a 1 "value")))
    
    (test-case "let-sql"
      (let-sql ([a 1] [b 2] [c 3])
        
        (check-identifier a)
        (check-identifier b)
        (check-identifier c)
        
        (check-equal? a 1 "a value")
        (check-equal? b 2 "b value")
        (check-equal? c 3 "c value")))
    
    (test-case "define-alias : entity"
      (begin-with-definitions
        
        (define a* (sql:alias 'a entity:person))
        (define b* (sql:alias 'b entity:pet))
        
        (define-alias a person)
        (define-alias b pet)
        
        (check-identifier a)
        (check-identifier a-id)
        (check-identifier a-revision)
        (check-identifier a-name)
        
        (check-identifier b)
        (check-identifier b-id)
        (check-identifier b-revision)
        (check-identifier b-owner-id)
        (check-identifier b-name)
        
        (check-equal? a a* "a")
        (check-equal? a-id (sql:alias a* attr:person-id) "a-id")
        (check-equal? a-revision (sql:alias a* attr:person-revision) "a-revision")
        (check-equal? a-name (sql:alias a* attr:person-name) "a-name")
        
        (check-equal? b b* "b")
        (check-equal? b-id (sql:alias b* attr:pet-id) "b-id")
        (check-equal? b-revision (sql:alias b* attr:pet-revision) "b-revision")
        (check-equal? b-owner-id (sql:alias b* attr:pet-owner-id) "b-owner-id")
        (check-equal? b-name (sql:alias b* attr:pet-name) "b-name")))
    
    (test-case "let-alias : entity"
      (begin-with-definitions
        
        (define a* (sql:alias 'a entity:person))
        (define b* (sql:alias 'b entity:pet))
        
        (let-alias ([a person] [b pet])
          
          (check-identifier a)
          (check-identifier a-id)
          (check-identifier a-revision)
          (check-identifier a-name)
          
          (check-identifier b)
          (check-identifier b-id)
          (check-identifier b-revision)
          (check-identifier b-owner-id)
          (check-identifier b-name)
          
          (check-equal? a a* "a")
          (check-equal? a-id (sql:alias a* attr:person-id) "a-id")
          (check-equal? a-revision (sql:alias a* attr:person-revision) "a-revision")
          (check-equal? a-name (sql:alias a* attr:person-name) "a-name")
          
          (check-equal? b b* "b")
          (check-equal? b-id (sql:alias b* attr:pet-id) "b-id")
          (check-equal? b-revision (sql:alias b* attr:pet-revision) "b-revision")
          (check-equal? b-owner-id (sql:alias b* attr:pet-owner-id) "b-owner-id")
          (check-equal? b-name (sql:alias b* attr:pet-name) "b-name"))))
    
    (test-case "let-alias : entity"
      (begin-with-definitions
        
        (define a* (sql:alias 'a entity:person))
        (define b* (sql:alias 'b entity:pet))
        
        (let-alias ([a person] [b pet])
          
          (check-identifier a)
          (check-identifier a-id)
          (check-identifier a-revision)
          (check-identifier a-name)
          
          (check-identifier b)
          (check-identifier b-id)
          (check-identifier b-revision)
          (check-identifier b-owner-id)
          (check-identifier b-name)
          
          (check-equal? a a* "a")
          (check-equal? a-id (sql:alias a* attr:person-id) "a-id")
          (check-equal? a-revision (sql:alias a* attr:person-revision) "a-revision")
          (check-equal? a-name (sql:alias a* attr:person-name) "a-name")
          
          (check-equal? b b* "b")
          (check-equal? b-id (sql:alias b* attr:pet-id) "b-id")
          (check-equal? b-revision (sql:alias b* attr:pet-revision) "b-revision")
          (check-equal? b-owner-id (sql:alias b* attr:pet-owner-id) "b-owner-id")
          (check-equal? b-name (sql:alias b* attr:pet-name) "b-name"))))
    
    (test-case "define-alias : query"
      (begin-with-definitions
        (define-alias a person)
        (define query (sql:select #:from a))
        (define-alias b query)
        (check-identifier b)
        (check-equal? b (sql:alias 'b query) "value")))
    
    (test-case "let-alias : query"
      (begin-with-definitions
        (define-alias a person)
        (define query (sql:select #:from a))
        (let-alias ([b query])
          (check-identifier b)
          (check-equal? b (sql:alias 'b query) "value"))))

    (test-case "define-alias : expression"
      (begin-with-definitions
        (define-alias a person)
        (define expr (sql:= a-id 123))
        (define-alias b expr)
        (check-identifier b)
        (check-equal? b (sql:alias 'b expr) "value")))
    
    (test-case "let-alias : expression"
      (begin-with-definitions
        (define-alias a person)
        (define expr (sql:= a-id 123))
        (let-alias ([b expr])
          (check-identifier b)
          (check-equal? b (sql:alias 'b expr) "value"))))))

; Provide statements -----------------------------

(provide sql-alias-tests)
