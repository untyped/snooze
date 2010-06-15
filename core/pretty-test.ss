#lang scheme/base

(require "../test-base.ss")

(require "core.ss")

; Tests ----------------------------------------

(define pretty-tests
  (test-suite "pretty.ss"
    
    (test-case "entity-pretty-name"
      (check-equal? (entity-pretty-name person) "person")
      (check-equal? (entity-pretty-name pet)    "pet"))
    
    (test-case "entity-pretty-name-plural"
      (check-equal? (entity-pretty-name-plural person) "people")
      (check-equal? (entity-pretty-name-plural pet)    "pets"))
    
    (test-case "attribute-pretty-name"
      (check-equal? (attribute-pretty-name (attr person guid))    "unique ID")
      (check-equal? (attribute-pretty-name (attr person name))    "name")
      (check-equal? (attribute-pretty-name (attr course active?)) "active flag"))
    
    (test-case "attribute-pretty-name-plural"
      (check-equal? (attribute-pretty-name-plural (attr person guid))    "unique IDs")
      (check-equal? (attribute-pretty-name-plural (attr person name))    "names")
      (check-equal? (attribute-pretty-name-plural (attr course active?)) "active flags"))))

; Provide statements -----------------------------

(provide pretty-tests)
