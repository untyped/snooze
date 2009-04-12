#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/26
         (unlib-in hash)
         "../test-data.ss"
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
      (check-false (person? (make-pet "Odie" 123))))))

; Provide statements -----------------------------

(provide define-entity-tests)
