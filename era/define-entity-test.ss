#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/26
         (unlib-in hash)
         "era.ss")

; Tests ----------------------------------------

(define define-entity-tests
  (test-suite "define-entity.ss"
    
    (test-case "entity"
      (check-pred entity? person))
    
    (test-case "struct-type"
      (check-pred struct-type? struct:person))
    
    (test-case "constructor and predicate"
      (check-pred procedure? make-person)
      (check-pred procedure? person?)
      (check-true  (person? (make-person "Dave")))
      (check-false (person? (make-pet "Odie" 123))))
    
    (test-case "struct equality"
      (let ([a1 (make-person "A")]
            [a2 (make-person "A")]
            [b  (make-person "B")])
        (check-equal?     a1 a2)
        (check-not-eq?    a1 a2)
        (check-not-equal? a1 b)
        (check-not-eq?    a1 b)))
    
    (test-case "keyword constructor"
      (check-equal? (make-person/defaults #:name "Dave")
                    (make-person "Dave"))
      (check-equal? (make-person/defaults #:name "Dave")
                    (make-person "Dave"))
      (check-equal? (make-person/defaults #:name "Dave")
                    (person-set (make-person "Dave"))))
    
    (test-case "copy constructor"
      (check-equal? (make-person/defaults #:name "Dave")
                    (person-set (make-person "Dave"))))))

; Provide statements -----------------------------

(provide define-entity-tests)
