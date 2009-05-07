#lang scheme/base

(require "../test-base.ss")

(require "../test-data.ss"
         "era.ss")

; Tests ----------------------------------------

(define pretty-tests
  (test-suite "pretty.ss"
    
    (test-case "entity-pretty-name"
      (check-equal? (entity-pretty-name person) "person")
      (check-equal? (entity-pretty-name pet)    "pet"))
    
    (test-case "entity-pretty-name-plural"
      (check-equal? (entity-pretty-name-plural person) "people")
      (check-equal? (entity-pretty-name-plural pet)    "pets"))
    
    (test-case "attribte-pretty-name"
      (check-equal? (attribute-pretty-name (attr person guid))    "unique identifier")
      (check-equal? (attribute-pretty-name (attr person name))    "name")
      (check-equal? (attribute-pretty-name (attr course active?)) "active flag"))
    
    (test-case "attribte-pretty-name-plural"
      (check-equal? (attribute-pretty-name-plural (attr person guid))    "unique identifiers")
      (check-equal? (attribute-pretty-name-plural (attr person name))    "names")
      (check-equal? (attribute-pretty-name-plural (attr course active?)) "active flags"))))

; Provide statements -----------------------------

(provide pretty-tests)
