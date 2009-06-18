#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/19
         srfi/26
         (unlib-in hash)
         "core.ss")

(require/expose "entity.ss"
  (name->database-name))

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
      (check-false (person? (make-pet #f "Odie")))
      (check-false (person? #f)))
    
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
                    (person-set (make-person "Dave"))))
    
    (test-case "name->table-name"
      (check-equal? (name->database-name 'person)        'person)
      (check-equal? (name->database-name 'person-record) 'personrecord)
      (check-equal? (name->database-name 'PersonRecord)  'personrecord))
    
    (test-case "attr"
      (let ([a (attr person guid)]
            [b (attr person name)])
        (check-true (andmap attribute? (list a b)))
        (check-not-eq? a b)
        (check-equal? (map attribute-name (list a b)) '(guid name))
        (check-equal? (map attribute-entity (list a b)) (list person person))))
    
    (test-case "attr-list"
      (let ([attrs (attr-list person guid name)])
        (check-equal? (attr-list person guid name)
                      (list (attr person guid)
                            (attr person name)))))
    
    (test-case "contracts"
      (check-exn exn:fail:contract? (cut make-course/defaults #:code #f))
      (check-exn exn:fail:contract? (cut make-course/defaults #:code "not a symbol"))
      ; symbol/string length aren't checked by contracts:
      (check-exn exn:fail:contract? (cut make-course/defaults #:code 'bittoolong))
      (check-exn exn:fail:contract? (cut make-course/defaults #:name (make-string #\a 129)))
      (check-not-exn (cut make-course/defaults #:value 0))
      (check-not-exn (cut make-course/defaults #:value 5))
      (check-exn exn:fail:contract? (cut make-course/defaults #:value -1))
      (check-exn exn:fail:contract? (cut make-course/defaults #:value 6))
      (check-not-exn (cut make-course/defaults #:rating 0.0))
      (check-not-exn (cut make-course/defaults #:rating 1.0))
      (check-exn exn:fail:contract? (cut make-course/defaults #:rating -0.001))
      (check-exn exn:fail:contract? (cut make-course/defaults #:rating 1.001))
      (check-exn exn:fail:contract? (cut make-course/defaults #:start (current-time time-utc)))
      (check-not-exn (cut make-tree-node/defaults #:color 'red))
      (check-not-exn (cut make-tree-node/defaults #:color (color red)))
      (check-exn exn:fail:contract? (cut make-tree-node/defaults #:color 'white)))))

; Provide statements -----------------------------

(provide define-entity-tests)
