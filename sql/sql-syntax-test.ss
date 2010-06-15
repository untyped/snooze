#lang scheme/base

(require (for-syntax scheme/base
                     "sql-syntax-internal.ss")
         "../test-base.ss"
         "../core/core.ss"
         (prefix-in sql: "sql-lang.ss")
         "sql-alias.ss"
         "sql-struct.ss"
         "sql-syntax.ss")

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
    
    (test-case "dotted identifier"
      (let-alias ([p person])
        (check-pred attribute-alias? (sql p.guid))))
    
    (test-case "default entity alias"
      (check-equal? (sql person)
                    (sql:alias 'person person))
      (check-equal? (sql person.name)
                    (sql:alias (sql:alias 'person person)
                               (attr person name))))
    
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
                  [c     course]
                  [expr1 (sql (+ c.value 1))])
        (check-not-sql-exn (sql a.guid))
        (check-not-sql-exn (sql a.revision))
        (check-not-sql-exn (sql expr1))))
    
    (test-case "sources (for use in #:from clauses)"
      (let-alias ([a person]
                  [b person]
                  [q (sql:select #:from b)])
        (check-not-sql-exn (sql a))
        (check-not-sql-exn (sql q))
        (check-not-sql-exn (sql (outer a q)))
        (check-not-sql-exn (sql (inner a q (= a.guid b.guid))))))
    
    (test-case "orders (for use in #:order clauses)"
      (let-alias ([a person])
        (let ([x 'asc])
          (check-not-sql-exn (sql (asc a.guid)))
          (check-not-sql-exn (sql (desc a.guid)))
          (check-not-sql-exn (sql (order a.guid 'asc)))
          (check-not-sql-exn (sql (order a.guid 'desc)))
          (check-not-sql-exn (sql (order a.guid ,x))))))
    
    (test-case "select : #:what clause"
      (let-alias ([a person])
        (check-equal? (sql (select #:what (a.guid a.name) #:from a))
                      (sql:select #:what (list (sql a.guid) (sql a.name)) #:from a))))
    
    (test-case "select : #:order clause"
      (let-alias ([a person])
        (check-equal? (sql (select #:from a #:order ((asc a.guid))))
                      (sql:select #:from a #:order (list (sql:asc (sql a.guid)))))))
    
    (test-case "select : #:distinct clause"
      (let-alias ([a person])
        (check-equal? (sql (select #:distinct #t #:from a))
                      (sql:select #:distinct #t #:from a))))
    
    (test-case "select : #:limit and #:offset clauses"
      (let-alias ([a person])
        (check-not-exn (cut sql (select #:from a #:limit 10 #:offset 10)))
        (check-not-exn (cut sql (select #:from a #:limit '10 #:offset '10)))
        (check-not-exn (cut sql (select #:from a #:limit `10 #:offset `10)))
        (check-not-exn (cut sql (select #:from a #:limit ,10 #:offset ,10)))))
    
    (test-case "serialize"
      (let ([original (sql (select #:from person #:order ((asc person.guid))))])
        (check-equal? (deserialize (serialize original)) original)))))

; Provide statements -----------------------------

(provide sql-syntax-tests)
