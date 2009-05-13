#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/26
         (unlib-in hash)
         "../test-data.ss"
         "era.ss")

; Helpers --------------------------------------

(define test-person #f)
(define test-pet    #f)

; Tests ------------------------------------------

(define cached-struct-tests
  (test-suite "cahed-struct.ss"
    
    #:before
    (lambda ()
      (set! test-person (make-snooze-struct person
                                            (entity-make-guid person #f)
                                            #f
                                            "Jon"))
      (set! test-pet    (make-snooze-struct pet
                                            (entity-make-guid pet #f)
                                            #f
                                            (struct-guid test-person)
                                            "Garfield")))
    
    (test-case "struct-entity"
      (check-eq? (struct-entity test-person) person))
    
    (test-case "struct-guid"
      (check-false (guid-id (struct-guid test-person)))
      (check-eq? (guid-entity (struct-guid test-person)) person)
      (check-not-equal? (struct-guid test-person) (struct-guid (make-person "Dave"))))
    
    (test-case "struct-saved?"
      (check-false (struct-saved? test-person))
      (check-true  (struct-saved? (person-set test-person #:guid (entity-make-guid person 123)))))
    
    (test-case "struct-revision and set-struct-revision!"
      (check-equal? (struct-revision test-person) #f)
      (check-equal? (struct-revision (person-set test-person #:revision 1000)) 1000))
    
    (test-case "field accessors and mutators"
      (check-eq? (pet-owner test-pet) test-person)
      (check-equal? (pet-name test-pet) "Garfield")
      (check-equal? (pet-owner (pet-set test-pet #:owner #f)) #f)
      (check-equal? (pet-name  (pet-set test-pet #:name "Odie")) "Odie"))
    
    (test-case "snooze-struct-ref"
      (check-equal? (snooze-struct-ref test-pet 'guid) test-pet)
      (check-equal? (snooze-struct-ref test-pet (attr pet guid)) test-pet)
      (check-equal? (snooze-struct-ref test-pet 'revision) #f)
      (check-equal? (snooze-struct-ref test-pet (attr pet revision)) #f)
      (check-equal? (snooze-struct-ref test-pet 'owner) test-person)
      (check-equal? (snooze-struct-ref test-pet (attr pet owner)) test-person)
      (check-equal? (snooze-struct-ref test-pet 'name) "Garfield")
      (check-equal? (snooze-struct-ref test-pet (attr pet name)) "Garfield"))
    
    (test-case "snooze-struct-ref*"
      (check-equal? (snooze-struct-ref* test-pet)
                    (list test-pet #f test-person "Garfield")))
    
    (test-case "snooze-struct-set"
      (let* ([test-person2 (snooze-struct-set test-person)])
        
        ; Struct equality:
        (check-true  (equal? test-person2 test-person))
        (check-false (eq? test-person2 test-person))
        
        ; Guids:
        (check-pred  guid? (struct-guid test-person))
        (check-pred  guid? (struct-guid test-person2))
        (check-equal? (struct-guid test-person)      (struct-guid test-person2))
        (check-false  (eq? (struct-guid test-person) (struct-guid test-person2)))
        
        ; Attributes:
        (check-equal? (struct-id test-person2)       (struct-id test-person))
        (check-equal? (struct-revision test-person2) (struct-revision test-person))
        (check-equal? (person-name test-person2)     (person-name test-person)))
      
      (let ([test-person2 (snooze-struct-set test-person (attr person name) "Dave")])
        (check-not-equal? test-person                    test-person2)
        (check-not-eq?    test-person                    test-person2)
        (check-equal?     (struct-id test-person2)       (struct-id test-person))
        (check-equal?     (struct-revision test-person2) (struct-revision test-person))
        (check-not-equal? (person-name test-person2)     (person-name test-person))))
    
    (test-case "make-snooze-struct/defaults"
      (check-equal? (make-snooze-struct/defaults person)
                    (make-person #f))
      (check-equal? (make-snooze-struct/defaults person (attr person name) "Dave")
                    (make-person "Dave"))
      (check-not-equal? (make-snooze-struct/defaults person)
                        (make-person "Dave"))
      
      ; Guid (in)equality:
      (let ([test-person2 (make-snooze-struct/defaults person (attr person guid) (entity-make-guid person 123))]
            [test-person3 (make-snooze-struct/defaults person (attr person guid) (entity-make-guid person 123))])
        (check-equal? (struct-id   test-person2) 123)
        (check-equal? (struct-guid test-person2) (struct-guid test-person3))
        (check-false  (eq? (struct-guid test-person2) (struct-guid test-person3))))
      
      ; Bad attribute/value arguments:
      (check-exn exn:fail?
        (cut make-snooze-struct/defaults person (attr person guid) 123))
      (check-exn exn:fail:contract?
        (cut make-snooze-struct/defaults person (attr person name)))
      (check-exn exn:fail:contract?
        (cut make-snooze-struct/defaults person (attr person name) (attr person guid)))
      (check-exn exn:fail:contract?
        (cut make-snooze-struct/defaults person (attr person name) "Dave" (attr person name) "Dave"))
      (check-exn exn:fail:contract?
        (cut make-snooze-struct/defaults person (attr pet name) 123)))
    
    (test-case "copy-snooze-struct"
      (check-equal? test-person (copy-snooze-struct test-person)))
    
    (test-case "snooze-struct-format"
      (check-equal? (snooze-struct-format test-person)
                    "Jon")
      (check-equal? (snooze-struct-format test-pet)
                    "Garfield")
      (check-equal? (snooze-struct-format test-pet #t)
                    "Jon's pet Garfield")
      (check-equal? (snooze-struct-format (make-pet #f "Topcat") #t)
                    "Stray animal Topcat"))))

; Provide statements -----------------------------

(provide cached-struct-tests)
