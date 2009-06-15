#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/26
         (unlib-in hash)
         "core.ss")

; Helpers --------------------------------------

(define test-person #f)
(define test-pet    #f)

; Tests ------------------------------------------

(define cached-struct-tests
  (test-suite "cached-struct.ss"
    
    #:before
    (lambda ()
      (set! test-person (make-person "Jon"))
      (set! test-pet    (make-pet test-person "Garfield")))
    
    (test-case "snooze-struct-entity"
      (check-eq? (snooze-struct-entity test-person) person))
    
    (test-case "snooze-struct-saved?"
      (check-false (snooze-struct-saved? test-person)))
    
    (test-case "snooze-struct-revision and set-struct-revision!"
      (check-equal? (snooze-struct-revision test-person) #f))
    
    (test-case "field accessors and mutators"
      (check-not-eq? (pet-owner test-pet) test-person)
      (check snooze-struct-eq? (pet-owner test-pet) test-person)
      (check-equal? (pet-name  test-pet) "Garfield")
      (check-equal? (pet-owner (pet-set test-pet #:owner #f)) #f)
      (check-equal? (pet-name  (pet-set test-pet #:name "Odie")) "Odie"))
    
    (test-case "snooze-struct-ref"
      (check-pred local-guid? (snooze-struct-ref test-pet 'guid))
      (check-pred local-guid? (snooze-struct-ref test-pet (attr pet guid)))
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
        (check-false (guid=? test-person2 test-person))
        (check-equal?  test-person2 test-person)
        (check-not-eq? test-person2 test-person)
        
        ; Guids:
        (check-equal?  test-person test-person2)
        (check-not-eq? test-person test-person2)
        (check-false (guid=? test-person test-person2))
        
        ; Attributes:
        (check-equal? (snooze-struct-id test-person2)       (snooze-struct-id test-person))
        (check-equal? (snooze-struct-revision test-person2) (snooze-struct-revision test-person))
        (check-equal? (person-name test-person2)            (person-name test-person)))
      
      (let ([test-person2 (snooze-struct-set test-person (attr person name) "Dave")])
        (check-not-equal? test-person                           test-person2)
        (check-not-eq?    test-person                           test-person2)
        (check-equal?     (snooze-struct-id test-person2)       (snooze-struct-id test-person))
        (check-equal?     (snooze-struct-revision test-person2) (snooze-struct-revision test-person))
        (check-not-equal? (person-name test-person2)            (person-name test-person))))
    
    (test-case "make-snooze-struct/defaults"
      (check-equal? (make-snooze-struct/defaults person)
                    (make-person #f))
      (check-equal? (make-snooze-struct/defaults person (attr person name) "Dave")
                    (make-person "Dave"))
      (check-not-equal? (make-snooze-struct/defaults person)
                        (make-person "Dave"))
      
      ; Guid (in)equality:
      (let ([test-person2 (make-snooze-struct/defaults person (attr person guid) (entity-make-vanilla-guid person 123))]
            [test-person3 (make-snooze-struct/defaults person (attr person guid) (entity-make-vanilla-guid person 123))])
        (check-equal?  (snooze-struct-id   test-person2) 123)
        (check-equal?  test-person2 test-person3)
        (check-false (guid=? test-person2 test-person3)))
      
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
    
    (test-case "format-snooze-struct"
      (check-equal? (format-snooze-struct test-person) "Jon")
      (check-equal? (format-snooze-struct test-pet) "Garfield")
      (check-equal? (format-snooze-struct test-pet #t) "Jon's pet Garfield")
      (check-equal? (format-snooze-struct (make-pet #f "Topcat") #t) "Stray animal Topcat"))))

; Provide statements -----------------------------

(provide cached-struct-tests)
