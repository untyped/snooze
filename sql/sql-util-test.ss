#lang scheme/base

(require "../test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "sql-alias.ss"
         (prefix-in sql: "sql-lang.ss")
         "sql-struct.ss"
         "sql-syntax.ss"
         "sql-util.ss")

; Tests ----------------------------------------

(define sql-util-tests
  (test-suite "sql-util.ss"
    
    (test-equal? "source->sources"
      (source->sources (sql (inner pet person (= pet.owner person.guid))))
      (sql-list pet person))
    
    (test-case "source->columns"
      (let-values ([(local imported)
                    (source->columns (sql (inner pet person (= pet.owner person.guid))))])
        (check-equal? local (sql-list pet.guid
                                      pet.revision
                                      pet.owner
                                      pet.name
                                      person.guid
                                      person.revision
                                      person.name))
        (check-equal? imported null)))
    
    (test-case "source->foreign-keys"
      (let-alias ([other pet])
        ; Joins between entities:
        (check-equal?
         (source->foreign-keys
          (sql (inner (inner pet
                             person
                             (and (= pet.name "Garfield") (= pet.owner person.guid)))
                      other
                      (= person.guid other.owner))))
         (build-hash (sql-list pet.owner   person.guid
                               other.owner person.guid)))
        ; Joins between entities and sub-selects:
        (check-equal?
         (source->foreign-keys
          (sql (inner pet
                      (select #:from  (inner person other (= person.guid other.owner))
                              #:limit 5)
                      (and (= pet.name "Garfield")
                           (= pet.owner person.guid)))))
         (build-hash (sql-list pet.owner   person.guid
                               other.owner person.guid)))))))

; Helpers --------------------------------------

; list -> hasheq
(define (build-hash args)
  (let ([ans (make-hash)])
    (let loop ([args args])
      (match args
        [(list-rest key val rest)
         (hash-set! ans key val)
         (loop rest)]
        [(list) ans]))))

; Provide statements -----------------------------

(provide sql-util-tests)
