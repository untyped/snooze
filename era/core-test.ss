#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/26
         (unlib-in hash)
         "../test-data.ss"
         "era.ss")

; Helpers --------------------------------------

(define-struct normal (a b c) #:transparent)

; Guid tests -----------------------------------

(define guid-tests
  (test-suite "guid"
    
    (test-case "make-guid"
      (check-pred guid? (make-guid 123))
      (check-pred guid? (make-guid #f))
      (check-exn exn:fail:contract? (cut make-guid -123)))
    
    (test-case "make-person-guid"
      (check-equal? (guid-entity (person-guid (make-person "Dave"))) person))))

; Type tests -----------------------------------

(define type-tests
  (test-suite "type"))

; Entity tests ---------------------------------

(define entity-tests
  (test-suite "entity"
    
    (test-case "entity-name"
      (check-equal? (entity-name person) 'person))
    
    (test-case "entity-table-name"
      (check-equal? (entity-table-name person) 'Person))
    
    (test-case "entity-struct-type"
      (check-pred struct-type? (entity-struct-type person))
      (check-equal? (entity-struct-type person) struct:person))
    
    (test-case "entity-constructor"
      (check-pred procedure? (entity-constructor person))
      (check-pred procedure? (entity-guid-constructor person))
      (check-equal? ((entity-constructor person)
                     ((entity-guid-constructor person) #f)
                     #f
                     "Dave")
                    (make-person "Dave")))
    
    (test-case "entity-attributes"
      (let ([attrs (entity-attributes pet)])
        (check-equal? (map attribute-name        attrs) '(guid revision owner-id name))
        (check-equal? (map attribute-column-name attrs) '(id revision ownerID name))
        (check-equal? (map attribute-index       attrs) '(0 1 2 3))
        (check-equal? (cddr (map attribute-type attrs))
                      (list type:integer
                            type:string))))
    
    (test-case "entity-has-attribute?"
      (check-true  (entity-has-attribute? pet 'guid))
      (check-true  (entity-has-attribute? pet (attr pet guid)))
      (check-true  (entity-has-attribute? pet 'revision))
      (check-true  (entity-has-attribute? pet (attr pet revision)))
      (check-true  (entity-has-attribute? pet 'owner-id))
      (check-true  (entity-has-attribute? pet (attr pet owner-id)))
      (check-true  (entity-has-attribute? pet 'name))
      (check-true  (entity-has-attribute? pet (attr pet name)))
      (check-false (entity-has-attribute? pet 'NAME))
      (check-false (entity-has-attribute? pet 'fake)))
    
    (test-case "entity-attribute"
      
      ; attribute natural -> (list symbol integer boolean boolean type)
      (define (testable-attribute-bits attr)
        (list (attribute-name attr)
              (attribute-column-name attr)
              (attribute-index attr)
              (procedure? (attribute-accessor attr))
              (procedure? (attribute-mutator attr))
              (attribute-type attr)))
      
      (define-check (check-attribute attr+name expected)
        (check-equal? (take (testable-attribute-bits (entity-attribute pet attr+name))
                            (length expected))
                      expected))
      
      (let ([expected (list 'guid 'id 0 #t #t)])
        (check-attribute 'guid expected)
        (check-attribute (attr pet guid) expected))
      
      (let ([expected (list 'revision 'revision 1 #t #t)])
        (check-attribute 'revision expected)
        (check-attribute (attr pet revision) expected))
      
      (let ([expected (list 'owner-id 'ownerID 2 #t #t type:integer)])
        (check-attribute 'owner-id expected)
        (check-attribute (attr pet owner-id) expected))
      
      (let ([expected (list 'name 'name 3 #t #t type:string)])
        (check-attribute 'name expected)
        (check-attribute (attr pet name) expected))
      
      (check-exn exn:fail:contract?
        (cut entity-attribute #f 'name)))))

; Relationship tests ---------------------------

(define relationship-tests
  (test-suite "relationship"))

; Attribute tests ------------------------------

(define attribute-tests
  (test-suite "attribute"
    
    (test-case "attribute-entity"
      (check-equal? (attribute-entity (attr person guid)) person)
      (check-equal? (attribute-entity (attr pet guid)) pet))))

; Snooze struct tests --------------------------

(define snooze-struct-tests
  (test-suite "snooze struct"
    
    (test-case "struct-entity"
      (check-eq? (struct-entity (make-person "Dave")) person)
      (check-exn exn:fail? (cut struct-entity (make-normal 1 2 3))))

    (test-case "struct-guid"
      (let ([struct (make-person "Dave")])
        (check-equal? (struct-guid struct)
                      ((entity-guid-constructor person) #f))))
    
    (test-case "struct-saved?"
      (let ([struct (make-person "Dave")])
        (check-false (struct-saved? struct))
        (set-guid-id! (struct-guid struct) 1)
        (check-true (struct-saved? struct))))
    
    (test-case "struct-revision and set-struct-revision!"
      (let ([struct (make-person "Dave")])
        (check-equal? (struct-revision struct) #f)
        (set-struct-revision! struct 1000)
        (check-equal? (struct-revision struct) 1000)))

    (test-case "field accessors and mutators"
      (let ([struct (make-pet 3 "Garfield")])
        (set-struct-id! struct 1)
        (set-struct-revision! struct 2)
        
        (check-equal? (pet-owner-id struct) 3)
        (check-equal? (pet-name struct) "Garfield")
        
        (set-pet-owner-id! struct 4)
        (set-pet-name! struct "Odie")
        
        (check-equal? (pet-owner-id struct) 4)
        (check-equal? (pet-name struct) "Odie")))
    
    (test-case "snooze-struct-ref"
      (let ([struct (make-pet 3 "Garfield")]
            [guid1  ((entity-guid-constructor pet) 1)])
        (set-struct-id! struct 1)
        (set-struct-revision! struct 2)
        
        (check-equal? (snooze-struct-ref struct 'guid) guid1)
        (check-equal? (snooze-struct-ref struct (attr pet guid)) guid1)
        (check-equal? (snooze-struct-ref struct 'revision) 2)
        (check-equal? (snooze-struct-ref struct (attr pet revision)) 2)
        (check-equal? (snooze-struct-ref struct 'owner-id) 3)
        (check-equal? (snooze-struct-ref struct (attr pet owner-id)) 3)
        (check-equal? (snooze-struct-ref struct 'name) "Garfield")
        (check-equal? (snooze-struct-ref struct (attr pet name)) "Garfield")
        
        (check-exn exn:fail? (cut snooze-struct-ref (make-normal 1 2 3) 'guid))))
    
    (test-case "snooze-struct-ref*"
      (let ([struct (make-pet 3 "Garfield")])
        (set-struct-id! struct 1)
        (set-struct-revision! struct 2)
        
        (check-equal? (snooze-struct-ref* struct)
                      (list ((entity-guid-constructor pet) 1)
                            2
                            3
                            "Garfield"))
        
        (check-exn exn:fail? (cut snooze-struct-ref* (make-normal 1 2 3)))))
    
    (test-case "snooze-struct-set!"
      
      (define-check (check-attribute struct attr+name expected)
        (check-false (equal? (snooze-struct-ref struct attr+name) expected))
        (snooze-struct-set! struct attr+name expected)
        (check-equal? (snooze-struct-ref struct attr+name) expected))
      
      (let ([struct (make-pet 3 "Garfield")])
        (check-attribute struct 'guid 100)
        (check-attribute struct (attr pet guid) 300)
        (check-attribute struct 'revision 100)
        (check-attribute struct (attr pet revision) 300)
        (check-attribute struct 'owner-id 100)
        (check-attribute struct (attr pet owner-id) 200)
        (check-attribute struct 'name "Odie")
        (check-attribute struct (attr pet name) "Nermal")))
    
    (test-case "make-snooze-struct/defaults"
      (check-equal? (check-not-exn (cut make-snooze-struct/defaults person))
                    (make-person #f))
      (check-equal? (check-not-exn (cut make-snooze-struct/defaults person (attr person name) "Dave"))
                    (make-person "Dave"))
      (check-equal? (struct-id (check-not-exn (cut make-snooze-struct/defaults person (attr person guid) 123)))
                    123)
      (check-exn exn:fail:contract?
        (cut struct-id (make-snooze-struct/defaults person (attr pet name) 123))
        "attribute from different type")
      (check-exn exn:fail:contract?
        (cut make-snooze-struct/defaults person (attr person name))
        "no value for attribute")
      (check-exn exn:fail:contract?
        (cut make-snooze-struct/defaults person (attr person name) (attr person guid))
        "consecutive attributes")
      (check-exn exn:fail:contract?
        (cut make-snooze-struct/defaults person (attr person name) "Dave" (attr person name) "Dave")
        "repeated attributes"))
    
    (test-case "copy-snooze-struct"
      (let ([struct1 (make-person/defaults #:id 1 #:revision 200 #:name "Dave")])
        (let ([struct2 (copy-snooze-struct struct1)])
          (check-pred integer? (struct-id struct1))
          ; Check struct1 and struct2 are different structures:
          (check-false (eq? struct2 struct1))
          (check-equal? struct2 struct1)
          ; Check struct1 and struct2 have the same ID, revision and attributes:
          (check-equal? (struct-id struct2)        (struct-id struct1))
          (check-equal? (struct-revision struct2)  (struct-revision struct1))
          (check-equal? (person-name struct2)      (person-name struct1)))
        
        (let ([struct2 (copy-snooze-struct struct1 (attr person name) "Noel")])
          ; Check struct1 and struct2 are different structures:
          (check-false (eq? struct2 struct1))
          (check-false (eq? struct2 struct1))
          ; Check struct1 and struct2 have the same ID, revision and attributes:
          (check-equal? (struct-id struct2)        (struct-id struct1))
          (check-equal? (struct-revision struct2)  (struct-revision struct1))
          (check-false  (equal? (person-name struct2) (person-name struct1))))))
    
    (test-case "update-snooze-struct-from-copy!"
      (let ([pet1 (make-pet/defaults #:id 1 #:revision 100 #:owner-id 1000 #:name "Garfield")]
            [pet2 (make-pet/defaults #:id 2 #:revision 200 #:owner-id 2000 #:name "Heathcliff")])
        
        (update-snooze-struct-from-copy! pet1 pet2)
        
        (check-equal? (pet-guid pet1) 2)
        (check-equal? (pet-revision pet1) 200)
        (check-equal? (pet-owner-id pet1) 2000)
        (check-equal? (pet-name pet1) "Heathcliff")
        (check-equal? pet1 pet2)
        (check-false (eq? pet1 pet2))))))

; Tests ----------------------------------------

(define core-tests
  (test-suite "core.ss"
    
    guid-tests
    type-tests
    entity-tests
    relationship-tests
    attribute-tests
    snooze-struct-tests))

; Provide statements -----------------------------

(provide core-tests)
