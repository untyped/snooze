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

(define-test-suite guid-tests
    
    #:before
    recreate-test-tables
    
    (test-case "entity-make-guid, guid-id"
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
        (check-pred number? (guid-id data1)))))

; Type tests -----------------------------------

(define-test-suite type-tests
  (test-case "type-name"
    (check-equal? (type-name (entity-make-guid-type person #f)) 'person)))

; Entity tests ---------------------------------

(define-test-suite entity-tests
      
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
        (let ([guid (entity-make-guid person 123)])
          (check-pred (entity-guid-predicate person) guid)
          (check-equal? (guid-id guid) 123))
        (let ([guid (entity-make-guid person 'guid456)])
          (check-pred (entity-guid-predicate person) guid)
          (check-equal? (guid-id guid) 'guid456)))
      
      (test-case "entity-guid?"
        (check-true  (entity-guid? person (entity-make-temporary-guid person)))
        (check-false (entity-guid? person (entity-make-temporary-guid pet))))
      
      (test-case "entity-attributes"
        (let ([attrs (entity-attributes pet)])
          (check-equal? (map attribute-name        attrs) '(guid revision owner name))
          (check-equal? (map attribute-column-name attrs) '(guid revision owner name))
          (check-equal? (map attribute-index       attrs) '(0 1 2 3))
          (check-equal? (cddr (map attribute-type attrs))
                        (list (entity-make-guid-type person #t)
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
          (for ([actual   (in-list (testable-attribute-bits (entity-attribute pet attr+name)))]
                [expected (in-list expected)]
                [index    (in-naturals)])
            (check-equal? actual expected (format "index ~a" index))))
        
        (let ([expected (list 'guid 'guid 0 #t #t (entity-make-guid-type pet #f))])
          (check-attribute 'guid expected)
          (check-attribute (attr pet guid) expected))
        
        (let ([expected (list 'revision 'revision 1 #t #t (make-integer-type #f 0 #f))])
          (check-attribute 'revision expected)
          (check-attribute (attr pet revision) expected))
        
        (let ([expected (list 'owner 'owner 2 #t #t (entity-make-guid-type person #t))])
          (check-attribute 'owner expected)
          (check-attribute (attr pet owner) expected))
        
        (let ([expected (list 'name 'name 3 #t #t type:string)])
          (check-attribute 'name expected)
          (check-attribute (attr pet name) expected))
        
        (check-exn exn:fail:contract?
          (cut entity-attribute #f 'name)))
      
      (test-case "entity-guid-attribute"
        (check-equal? (entity-guid-attribute person)
                      (attr person guid)))
      
      (test-case "entity-revision-attribute"
        (check-equal? (entity-revision-attribute person)
                      (attr person revision)))
      
      (test-case "entity-data-attribute"
        (check-equal? (entity-data-attributes person)
                      (list (attr person name)))))

; Relationship tests ---------------------------

(define-test-suite relationship-tests)

; Attribute tests ------------------------------

(define-test-suite attribute-tests
  (test-case "attribute-entity"
    (check-equal? (attribute-entity (attr person guid)) person)
    (check-equal? (attribute-entity (attr pet guid)) pet)))

; Tests ----------------------------------------

(define core-tests
  (test-suite "struct.ss"
    guid-tests
    type-tests
    entity-tests
    attribute-tests
    relationship-tests))

; Provide statements -----------------------------

(provide core-tests)
