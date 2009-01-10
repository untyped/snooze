#lang scheme/base

(require (for-syntax scheme/base
                     (file "sql-syntax-internal.ss"))
         (file "../persistent-struct-syntax.ss")
         (file "../test-base.ss")
         (file "../test-data.ss")
         (prefix-in sql: (file "sql-lang.ss"))
         (file "sql-alias.ss")
         (file "sql-syntax.ss"))

; Helpers ----------------------------------------

(define-syntax (check-not-sql-exn stx)
  (syntax-case stx (sql)
    [(_ (sql expr))
     (with-handlers
      ((exn? (lambda (e) #`(fail #,(exn-message e)))))
       (expand-top-level (syntax expr)))]))

(define-syntax (check-sql-exn stx)
  (syntax-case stx (sql)
    [(_ (sql expr))
     (with-handlers
      ((exn? (lambda (e) #`(void))))
       (expand-top-level (syntax expr))
       #`(fail (format "No exception raised: expanded to ~a" (syntax->datum (expand-top-level (syntax expr))))))]))

; Tests ------------------------------------------

; test-suite
(define sql-syntax-tests
  (test-suite "sql-syntax.ss"
    
    (test-case "top-level unquote"
      (check-sql-exn (sql ,1)))
    
    (test-case "literal"
      (check-equal? (sql 1) (sql:literal 1)))
    
    (test-case "expression"
      (check-equal? (sql (= 1 2)) (sql:= 1 2))
      (check-equal? (sql (+ 2 4)) (sql:+ 2 4)))
    
    (test-case "expression : normal identifier with unquote"
      (let ([a 2])
        (check-not-sql-exn (sql (= ,a 2)))))
    
    (test-case "expression : normal identifier without unquote"
      (let ([a 2])
        (check-sql-exn (sql (= a 2)))))
    
    (test-case "expression : SQL identifier without unquote"
      (let-sql ([a 2])
        (check-not-sql-exn (sql (= a 2)))))
    
    (test-case "columns (for use in #:what and #:group clauses)"
      (let-alias ([a     person]
                  [expr1 (sql (+ a-id 1))])
        (check-not-sql-exn (sql a-id))
        (check-not-sql-exn (sql a-revision))
        (check-not-sql-exn (sql expr1))))
    
    (test-case "sources (for use in #:from clauses)"
      (let-alias ([a person]
                  [b person]
                  [q (sql:select #:from b)])
        (check-not-sql-exn (sql a))
        (check-not-sql-exn (sql q))
        (check-not-sql-exn (sql (outer a q)))
        (check-not-sql-exn (sql (inner a q (= a-id b-id))))))
    
    (test-case "orders (for use in #:order clauses)"
      (let-alias ([a person])
        (let ([x 'asc])
          (check-not-sql-exn (sql (asc a-id)))
          (check-not-sql-exn (sql (desc a-id)))
          (check-not-sql-exn (sql (order a-id 'asc)))
          (check-not-sql-exn (sql (order a-id 'desc)))
          (check-not-sql-exn (sql (order a-id ,x))))))
    
    (test-case "select : #:what clause"
      (let-alias ([a person])
        (check-equal? (sql (select #:what (a-id a-name) #:from a))
                      (sql:select #:what (list a-id a-name) #:from a))))
    
    (test-case "select : #:order clause"
      (let-alias ([a person])
        (check-equal? (sql (select #:from a #:order ((asc a-id))))
                      (sql:select #:from a #:order (list (sql:asc a-id))))))))

; Provide statements -----------------------------

(provide sql-syntax-tests)
