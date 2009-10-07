#lang scheme/base

(require "../test-base.ss")

(require scheme/dict
         (only-in srfi/1 take)
         srfi/26
         (unlib-in hash)
         "struct.ss"
         (only-in "define-entity.ss" attr))

(define (hash->alist hash)
  (for/list ([item (in-dict-pairs hash)]) item))

; Guid tests -----------------------------------

(define guid-tests
  (test-suite "guid"
    
    #:before
    recreate-test-tables
    
    (test-case "entity-make-guid, guid-id, guid-serial"
      (let* ([temp1 (entity-make-guid person 'unsaved)]
             [temp2 (entity-make-guid person 'unsaved)]
             [data1 (entity-make-guid person 123)]
             [data2 (entity-make-guid person 123)])
        (check-pred temporary-guid? temp1)
        (check-pred database-guid?  data1)
        (check-equal? temp1 temp2)
        (check-equal? data1 data2)
        (check-not-eq? temp1 temp2)
        (check-not-eq? data1 data2)
        (check-pred symbol? (guid-id temp1))
        (check-pred number? (guid-id data1))))))

; Type tests -----------------------------------

(define type-tests
  (test-suite "type"
    
    (test-case "type-name"
      (check-equal? (type-name (make-guid-type #f person)) 'person))))

; Entity tests ---------------------------------

#;(define entity-tests
    (test-suite "entity"
      
      (test-case "entity-name"
        (check-equal? (entity-name person) 'person))
      
      (test-case "entity-table-name"
        (check-equal? (entity-table-name person) 'person))
      
      (test-case "entity-struct-type"
        (check-pred struct-type? (entity-struct-type person))
        (check-equal? (entity-struct-type person) struct:person))
      
      (test-case "entity-private-constructor"
        (check-pred procedure? (entity-private-constructor person)))
      
      (test-case "entity-guid-constructor"
        (check-pred procedure? (entity-guid-constructor person)))
      
      (test-case "entity-constructor"
        (check-pred procedure? (entity-constructor person)))
      
      (test-case "entity-private-predicate"
        (check-pred procedure? (entity-private-predicate person))
        (check-pred (entity-private-predicate person)
                    ((entity-private-constructor person)
                     #f
                     #f
                     "Dave")))
      
      (test-case "entity-predicate"
        (check-pred procedure? (entity-predicate person)))
      
      (test-case "entity-guid-constructor"
        (check-pred procedure? (entity-guid-constructor person)))
      
      (test-case "entity-guid-predicate"
        (check-pred procedure? (entity-guid-predicate person)))
      
      (test-case "entity-make-guid"
        (let ([guid (entity-make-guid person #f)])
          (check-pred (entity-guid-predicate person) guid)
          (check-equal? (guid-id guid) #f))
        (let ([guid (entity-make-guid person 123)])
          (check-pred (entity-guid-predicate person) guid)
          (check-equal? (guid-id guid) 123))
        (let ([guid (entity-make-guid person 123 'guid456)])
          (check-pred (entity-guid-predicate person) guid)
          (check-equal? (guid-id guid) 123)
          (check-equal? (guid-serial guid) 'guid456)))
      
      (test-case "entity-guid?"
        (check-true  (entity-guid? person (entity-make-guid person #f)))
        (check-false (entity-guid? person (entity-make-guid pet #f))))
      
      (test-case "entity-attributes"
        (let ([attrs (entity-attributes pet)])
          (check-equal? (map attribute-name        attrs) '(guid revision owner name))
          (check-equal? (map attribute-column-name attrs) '(id revision owner name))
          (check-equal? (map attribute-index       attrs) '(0 1 2 3))
          (check-equal? (cddr (map attribute-type attrs))
                        (list (make-guid-type #t person)
                              (make-string-type #t #f)))))
      
      (test-case "entity-has-attribute?"
        (check-true  (entity-has-attribute? pet 'guid))
        (check-true  (entity-has-attribute? pet (attr pet guid)))
        (check-true  (entity-has-attribute? pet 'revision))
        (check-true  (entity-has-attribute? pet (attr pet revision)))
        (check-true  (entity-has-attribute? pet 'owner))
        (check-true  (entity-has-attribute? pet (attr pet owner)))
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
                (procedure? (attribute-accessor attr))
                (attribute-type attr)))
        
        (define-check (check-attribute attr+name expected)
          (check-equal? (take (testable-attribute-bits (entity-attribute pet attr+name))
                              (length expected))
                        expected))
        
        (let ([expected (list 'guid 'id 0 #t #t (make-guid-type #f pet))])
          (check-attribute 'guid expected)
          (check-attribute (attr pet guid) expected))
        
        (let ([expected (list 'revision 'revision 1 #t #t (make-integer-type #f #f #f))])
          (check-attribute 'revision expected)
          (check-attribute (attr pet revision) expected))
        
        (let ([expected (list 'owner 'owner 2 #t #t (make-guid-type #t person))])
          (check-attribute 'owner expected)
          (check-attribute (attr pet owner) expected))
        
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

; Tests ----------------------------------------

(define core-tests
  (test-suite "struct.ss"
    guid-tests
    type-tests
    ;entity-tests
    relationship-tests
    attribute-tests))

; Provide statements -----------------------------

(provide core-tests)
