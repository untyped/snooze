#lang scheme/base

(require "snooze.ss"
         "test-base.ss"
         "test-data.ss"
         "test-util.ss")

(provide schema-tests)

(define schema-tests
  (test-suite "schema.ss"
    
    (test-case "schema-entities"
      (let ([entities (schema-entities)])
        ; Three entities are defined in test-base.ss, so there should be a least three in the schema
        (check >= (length entities) 3)
        (for-each (lambda (entity)
                    (check-not-false (schema-entity (entity-name entity))))
                  entities)
        (check-false (schema-entity 'some-bogus-name))))
    
    (test-exn "add-schema-entity!: entity already exists"
      exn:fail:snooze?
      (lambda ()
        (define-persistent-struct person ())
        (void)))
    
    ))
