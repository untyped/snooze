#lang scheme/base

(require "../test-base.ss")

(require srfi/26
         (planet untyped/unlib:3/hash)
         "../persistent-struct.ss"
         "../test-data.ss"
         "../test-util.ss"
         "era.ss")

; Helpers --------------------------------------

(define-struct normal (a b c) #:transparent)

; Tests ----------------------------------------

(define era-tests
  (test-suite "era.ss"
    
    (test-case "struct-id and set-struct-id!"
      (begin-with-definitions
        (define struct (make-person "Dave"))
        (check-equal? (struct-id struct) #f)
        (set-struct-id! struct 1)
        (check-equal? (struct-id struct) 1)))
    
    (test-case "struct-saved?"
      (begin-with-definitions
        (define struct (make-person "Dave"))
        (check-false (struct-saved? struct))
        (set-struct-id! struct 1)
        (check-true (struct-saved? struct))))
    
    (test-case "struct-revision and set-struct-revision!"
      (begin-with-definitions
        (define struct (make-person "Dave"))
        (check-equal? (struct-revision struct) #f)
        (set-struct-revision! struct 1000)
        (check-equal? (struct-revision struct) 1000)))
    
    (test-case "field accessors and mutators"
      (begin-with-definitions
        (define struct (make-pet 3 "Garfield"))
        (set-struct-id! struct 1)
        (set-struct-revision! struct 2)
        
        (check-equal? (pet-owner-id struct) 3)
        (check-equal? (pet-name struct)     "Garfield")
        
        (set-pet-owner-id! struct 4)
        (set-pet-name! struct "Odie")
        
        (check-equal? (pet-owner-id struct) 4)
        (check-equal? (pet-name struct)     "Odie")))
    
    (test-eq? "struct-entity"
      (struct-entity (make-person "Dave"))
      person)
    
    (test-exn "struct- raises exception when argument isn't a persistent struct"
      exn:fail:contract?
      (lambda ()
        (define-struct test (a b c))
        (struct-entity (make-test 1 2 3))))
    
    (test-case "entity-attributes"
      (begin-with-definitions
        (define attrs (entity-attributes pet))
        (check-equal? (map attribute-name attrs)
                      '(id revision owner-id name))
        (check-equal? (map attribute-column-name attrs)
                      '(id revision ownerID name))
        (check-equal? (map attribute-index attrs)
                      '(0 1 2 3))
        (check-equal? (map attribute-type attrs)
                      (list type:id type:revision type:integer type:string))
        (check-exn exn:fail:contract?
          (cut entity-attributes #f))))
    
    (test-case "entity-has-attribute?"
      (check-true  (entity-has-attribute? pet 'id))
      (check-true  (entity-has-attribute? pet (attr pet id)))
      (check-true  (entity-has-attribute? pet 'revision))
      (check-true  (entity-has-attribute? pet (attr pet revision)))
      (check-true  (entity-has-attribute? pet 'owner-id))
      (check-true  (entity-has-attribute? pet (attr pet owner-id)))
      (check-true  (entity-has-attribute? pet 'name))
      (check-true  (entity-has-attribute? pet (attr pet name)))
      (check-false (entity-has-attribute? pet 'NAME))
      (check-false (entity-has-attribute? pet 'fake))
      (check-exn exn:fail:contract?
        (cut entity-has-attribute? #f 'name)))
    
    (test-case "entity-attribute"
      (begin-with-definitions
        
        ; attribute -> (list symbol integer boolean booleab type)
        (define (testable-attribute-bits attr)
          (list (attribute-name attr)
                (attribute-column-name attr)
                (attribute-index attr)
                (procedure? (attribute-accessor attr))
                (procedure? (attribute-mutator attr))
                (attribute-type attr)))
        
        (define-check (check-attribute attr+name expected)
          (check-equal? (testable-attribute-bits (entity-attribute pet attr+name)) expected))
        
        (let ([expected (list 'id 'id 0 #t #t type:id)])
          (check-attribute 'id expected)
          (check-attribute (attr pet id) expected))
        
        (let ([expected (list 'revision 'revision 1 #t #t type:revision)])
          (check-attribute 'revision expected)
          (check-attribute (attr pet revision) expected))
        
        (let ([expected (list 'owner-id 'ownerID 2 #t #t type:integer)])
          (check-attribute 'owner-id expected)
          (check-attribute (attr pet owner-id) expected))
        
        (let ([expected (list 'name 'name 3 #t #t type:string)])
          (check-attribute 'name expected)
          (check-attribute (attr pet name) expected))

        (check-exn exn:fail:contract?
          (cut entity-attribute #f 'name))))
    
    (test-case "attribute-entity"
      (check-equal? (attribute-entity (attr person id)) person)
      (check-equal? (attribute-entity (attr pet id)) pet))
    
    (test-case "struct-has-attribute?"
      (begin-with-definitions
        
        (define struct (make-pet 3 "Garfield"))
        (set-struct-id! struct 1)
        (set-struct-revision! struct 2)
        
        (check-true (struct-has-attribute? struct 'id))
        (check-true (struct-has-attribute? struct (attr pet id)))
        (check-true (struct-has-attribute? struct 'revision))
        (check-true (struct-has-attribute? struct (attr pet revision)))
        (check-true (struct-has-attribute? struct 'owner-id))
        (check-true (struct-has-attribute? struct (attr pet owner-id)))
        
        (define struct2 (make-normal 1 2 3))
        (check-exn exn:fail:contract?
          (cut struct-has-attribute? struct2 'id))))
    
    (test-case "struct-attribute"
      (begin-with-definitions
        
        (define struct (make-pet 3 "Garfield"))
        (set-struct-id! struct 1)
        (set-struct-revision! struct 2)
        
        (check-equal? (struct-attribute struct 'id) 1)
        (check-equal? (struct-attribute struct (attr pet id)) 1)
        (check-equal? (struct-attribute struct 'revision) 2)
        (check-equal? (struct-attribute struct (attr pet revision)) 2)
        (check-equal? (struct-attribute struct 'owner-id) 3)
        (check-equal? (struct-attribute struct (attr pet owner-id)) 3)
        (check-equal? (struct-attribute struct 'name) "Garfield")
        (check-equal? (struct-attribute struct (attr pet name)) "Garfield")
        
        (define struct2 (make-normal 1 2 3))
        
        (check-exn exn:fail:contract?
          (cut struct-attribute struct2 'id))))
    
    (test-case "set-struct-attribute!"
      (begin-with-definitions
        
        (define struct (make-pet 3 "Garfield"))
        
        (define-check (check-attribute attr+name expected)
          (check-false (equal? (struct-attribute struct attr+name) expected))
          (set-struct-attribute! struct attr+name expected)
          (check-equal? (struct-attribute struct attr+name) expected))
        
        (check-attribute 'id 100)
        (check-attribute (attr pet id) 300)
        (check-attribute 'revision 100)
        (check-attribute (attr pet revision) 300)
        (check-attribute 'owner-id 100)
        (check-attribute (attr pet owner-id) 200)
        (check-attribute 'name "Odie")
        (check-attribute (attr pet name) "Nermal")))
    
    (test-case "struct-attributes"
      (begin-with-definitions
        
        (define struct (make-pet 3 "Garfield"))
        (set-struct-id! struct 1)
        (set-struct-revision! struct 2)
        
        (check-equal? (struct-attributes struct)
                      (list 1 2 3 "Garfield"))
        
        (define struct2 (make-normal 1 2 3))
        
        (check-exn exn:fail:contract?
          (cut struct-attributes struct2))))
    
    (test-case "make-persistent-struct/defaults"
      (check-equal? (check-not-exn (cut make-persistent-struct/defaults person))
                    (make-person #f)
                    "no attributes")
      (check-equal? (check-not-exn (cut make-persistent-struct/defaults person (attr person name) "Dave"))
                    (make-person "Dave")
                    "attribute from the type in question")
      (check-equal? (struct-id (check-not-exn (cut make-persistent-struct/defaults person 'id 123)))
                    123
                    "attribute from super type")
      (check-exn exn:fail:contract?
        (cut struct-id (make-persistent-struct/defaults person (attr pet name) 123))
        "attribute from different type")
      (check-exn exn:fail:contract?
        (cut make-persistent-struct/defaults person (attr person name))
        "no value for attribute")
      (check-exn exn:fail:contract?
        (cut make-persistent-struct/defaults person (attr person name) (attr person id))
        "consecutive attributes")
      (check-exn exn:fail:contract?
        (cut make-persistent-struct/defaults person (attr person name) "Dave" (attr person name) "Dave")
        "repeated attributes"))
    
    (test-case "copy-persistent-struct"
      (begin-with-definitions
        
        (define struct1
          (let ([ans (make-person "Dave")])
            (set-struct-id! ans 1)
            (set-struct-revision! ans 200)
            ans))
        
        (check-pred integer? (struct-id struct1))
        
        (let ([struct2 (copy-persistent-struct struct1)])
          ; Check struct1 and struct2 are different structures:
          (check-false (eq? struct2 struct1))
          (check-equal? struct2 struct1)
          ; Check struct1 and struct2 have the same ID, revision and attributes:
          (check-equal? (struct-id struct2)        (struct-id struct1))
          (check-equal? (struct-revision struct2)  (struct-revision struct1))
          (check-equal? (person-name struct2)      (person-name struct1)))
        
        (let ([struct2 (copy-persistent-struct struct1 (attr person name) "Noel")])
          ; Check struct1 and struct2 are different structures:
          (check-false (eq? struct2 struct1))
          (check-false (eq? struct2 struct1))
          ; Check struct1 and struct2 have the same ID, revision and attributes:
          (check-equal? (struct-id struct2)        (struct-id struct1))
          (check-equal? (struct-revision struct2)  (struct-revision struct1))
          (check-false  (equal? (person-name struct2) (person-name struct1))))))
    
    (test-case "update-persistent-struct-from-copy!"
      (begin-with-definitions
        
        (define pet1 (make-pet 1 "Garfield"))
        (define pet2 (make-pet 2 "Heathcliff"))
        
        (for-each set-struct-id! 
                  (list pet1 pet2)
                  (list 1 2))
        (for-each set-struct-revision!
                  (list pet1 pet2)
                  (list 1 2))
        
        (update-persistent-struct-from-copy! pet1 pet2)
        
        (check-equal? (pet-id pet1) 2)
        (check-equal? (pet-revision pet1) 2)
        (check-equal? (pet-owner-id pet1) 2)
        (check-equal? (pet-name pet1) "Heathcliff")
        (check-equal? pet1 pet2)
        (check-false (eq? pet1 pet2))))))

; Provide statements -----------------------------

(provide era-tests)
