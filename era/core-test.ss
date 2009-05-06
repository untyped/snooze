#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/26
         (unlib-in hash)
         "../test-data.ss"
         "core.ss"
         (only-in "define-entity.ss" attr))

; Guid tests -----------------------------------

(define guid-tests
  (test-suite "guid"
    
    (test-case "make-guid"
      (check-pred guid? (make-guid 123))
      (check-pred guid? (make-guid #f))
      (check-exn exn:fail:contract? (cut make-guid -123)))
    
    (test-case "make-person-guid"
      (let ([guid (make-person "Dave")])
        (check-equal? (guid-entity guid) person)
        (check-equal? (guid-id guid) #f)))))

; Type tests -----------------------------------

(define type-tests
  (test-suite "type"
    
    (test-case "type-name"
      (check-equal? (type-name (make-guid-type #f person)) 'person))))

; Entity tests ---------------------------------

(define entity-tests
  (test-suite "entity"
    
    (test-case "entity-name"
      (check-equal? (entity-name person) 'person))
    
    (test-case "entity-table-name"
      (check-equal? (entity-table-name person) 'people))
    
    (test-case "entity-struct-type"
      (check-pred struct-type? (entity-struct-type person))
      (check-equal? (entity-struct-type person) struct:person))
    
    (test-case "entity-private-constructor"
      (check-pred procedure? (entity-private-constructor person))
      (check-pred procedure? (entity-guid-constructor person)))
    
    (test-case "entity-cached-constructor"
      ; see cache-test.ss for more tests
      (check-pred procedure? (entity-cached-constructor person)))
    
    (test-case "entity-private-predicate"
      (check-pred procedure? (entity-private-predicate person))
      (check-pred (entity-private-predicate person)
                  ((entity-private-constructor person)
                   ((entity-guid-constructor person) (current-snooze) #f)
                   #f
                   "Dave")))
    
    (test-case "entity-cached-predicate"
      ; see cache-test.ss for more tests
      (check-pred procedure? (entity-cached-predicate person)))
    
    (test-case "entity-guid-constructor"
      (check-pred procedure? (entity-guid-constructor person)))
    
    (test-case "entity-guid-predicate"
      (check-pred procedure? (entity-guid-predicate person)))
    
    (test-case "entity-make-guid"
      (let ([guid (entity-make-guid person)])
        (check-pred (entity-guid-predicate person) guid)
        (check-equal? (guid-id guid) #f))
      (let ([guid (entity-make-guid person 123)])
        (check-pred (entity-guid-predicate person) guid)
        (check-equal? (guid-id guid) 123)))
    
    (test-case "entity-guid?"
      (check-true  (entity-guid? person (entity-make-guid person)))
      (check-false (entity-guid? person (entity-make-guid pet))))
    
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
    
    (test-case "entity-guid-attribute?"
      (check-true  (entity-guid-attribute? pet 'guid))
      (check-true  (entity-guid-attribute? pet (attr pet guid)))
      (check-false (entity-guid-attribute? pet 'name))
      (check-false (entity-guid-attribute? pet (attr pet name))))
    
    (test-case "entity-attribute"
      
      ; attribute natural -> (list symbol integer boolean boolean type)
      (define (testable-attribute-bits attr)
        (list (attribute-name attr)
              (attribute-column-name attr)
              (attribute-index attr)
              (procedure? (attribute-private-accessor attr))
              (procedure? (attribute-private-mutator attr))
              (procedure? (attribute-cached-accessor attr))
              (procedure? (attribute-cached-mutator attr))
              (attribute-type attr)))
      
      (define-check (check-attribute attr+name expected)
        (check-equal? (take (testable-attribute-bits (entity-attribute pet attr+name))
                            (length expected))
                      expected))
      
      (let ([expected (list 'guid 'id 0 #t #t #t #t)])
        (check-attribute 'guid expected)
        (check-attribute (attr pet guid) expected)
        ; We can't make a type that is equal? to a guid type,
        ; so we test the type using type-compatible? instead:
        (check-true (type-compatible? (attribute-type (attr pet guid))
                                      (make-guid-type #f pet))))
      
      (let ([expected (list 'revision 'revision 1 #t #t #t #t)])
        (check-attribute 'revision expected)
        (check-attribute (attr pet revision) expected)
        ; We can't make a type that is equal? to a revision type,
        ; so we test the type using type-compatible? instead:
        (check-true (type-compatible? (attribute-type (attr pet revision))
                                      type:integer)))
      
      (let ([expected (list 'owner-id 'ownerID 2 #t #t #t #t type:integer)])
        (check-attribute 'owner-id expected)
        (check-attribute (attr pet owner-id) expected))
      
      (let ([expected (list 'name 'name 3 #t #t #t #t type:string)])
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

; Tests ----------------------------------------

(define core-tests
  (test-suite "core.ss"
    guid-tests
    type-tests
    entity-tests
    relationship-tests
    attribute-tests))

; Provide statements -----------------------------

(provide core-tests)
