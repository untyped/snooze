#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/26
         (unlib-in hash)
         "../test-data.ss"
         "core.ss"
         "define-entity.ss"
         "snooze-struct.ss")

; Helpers --------------------------------------

(define-struct normal (a b c) #:transparent)

(define test-normal (make-normal 1 2 3))
(define test-person #f)
(define test-pet    #f)

; Tests ------------------------------------------

(define snooze-struct-tests
  (test-suite "snooze-struct.ss"
    
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
      (check-eq? (struct-entity test-person) person)
      (check-exn exn:fail? (cut struct-entity test-normal)))
    
    (test-case "struct-guid"
      (check-pred guid? (struct-guid test-person))
      (check-equal? (guid-entity (struct-guid test-person)) person))
    
    (test-case "struct-saved?"
      (check-false (struct-saved? test-person))
      (check-true (struct-saved? (snooze-struct-set
                                  test-person
                                  (attr person guid)
                                  (entity-make-guid person 1)))))
    
    (test-case "struct-id"
      (check-equal? (struct-id test-person) #f)
      (check-equal? (struct-id (snooze-struct-set
                                test-person
                                (attr person guid)
                                (entity-make-guid person 1))) 1))
    
    (test-case "struct-revision"
      (check-equal? (struct-revision test-person) #f)
      (check-equal? (struct-revision (snooze-struct-set test-person (attr person revision) 1)) 1))
    
    (test-case "set-struct-revision!"
      (let ([struct (make-person* "Dave")])
        (check-equal? (struct-revision struct) #f)
        (set-struct-revision! struct 1000)
        (check-equal? (struct-revision struct) 1000)))
    
    (test-case "snooze-struct-ref"
      (check-equal? (guid-entity (snooze-struct-ref test-pet 'guid)) pet)
      (check-equal? (guid-entity (snooze-struct-ref test-pet (attr pet guid))) pet)
      (check-equal? (snooze-struct-ref test-pet 'revision) #f)
      (check-equal? (snooze-struct-ref test-pet (attr pet revision)) #f)
      (check-equal? (guid-entity (snooze-struct-ref test-pet 'owner)) person)
      (check-equal? (guid-entity (snooze-struct-ref test-pet (attr pet owner))) person)
      (check-equal? (snooze-struct-ref test-pet 'name) "Garfield")
      (check-equal? (snooze-struct-ref test-pet (attr pet name)) "Garfield")
      (check-exn exn:fail? (cut snooze-struct-ref test-normal 'guid)))
    
    (test-case "snooze-struct-ref*"
      (parameterize ([in-cache-code? #t])
        (map (lambda (x y)
               (if (or (guid? x) (guid? y))
                   (check guid=? x y)
                   (check-equal? x y)))
             (snooze-struct-ref* test-pet)
             (list (struct-guid test-pet)
                   #f
                   (struct-guid test-person)
                   "Garfield"))
        (check-exn exn:fail? (cut snooze-struct-ref* test-normal))))
    
    (test-case "snooze-struct-set"
      (let ([test-person2 (snooze-struct-set test-person)]
            [test-person3 (snooze-struct-set
                           test-person
                           (attr person guid)
                           (entity-make-guid person 123))])
        (check-equal?     test-person test-person2)
        (check-not-eq?    test-person test-person2)
        (check-not-equal? test-person test-person3)
        (check-equal?     (cdr (snooze-struct-ref* test-person))
                          (cdr (snooze-struct-ref* test-person3)))))
    
    (test-case "make-snooze-struct/defaults"
      (parameterize ([in-cache-code? #t])
        (check-equal? (struct-entity (make-snooze-struct/defaults person)) person)
        (let ([test-person2 (make-snooze-struct/defaults person)]
              [test-person3 (make-snooze-struct/defaults
                             person
                             (attr person guid)
                             (entity-make-guid person 123))])
          (check-false  (struct-id test-person))
          (check-false  (struct-id test-person2))
          (check-equal? (struct-id test-person3) 123)))
      
      ; Bad attribute/value arguments:
      (check-exn exn:fail:contract?
        (cut make-snooze-struct/defaults person (attr person name)))
      (check-exn exn:fail:contract?
        (cut make-snooze-struct/defaults person (attr person name) (attr person guid)))
      (check-exn exn:fail:contract?
        (cut make-snooze-struct/defaults person (attr person name) "Dave" (attr person name) "Dave"))
      (check-exn exn:fail:contract?
        (cut make-snooze-struct/defaults person (attr pet name) 123)))
    
    (test-case "snooze-struct-set"
      (let ([test-person2 (copy-snooze-struct test-person)])
        (check-equal?     test-person test-person2)
        (check-not-eq?    test-person test-person2)))))

; Provide statements -----------------------------

(provide snooze-struct-tests)
