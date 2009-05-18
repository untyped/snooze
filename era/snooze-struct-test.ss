#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/26
         (unlib-in hash)
         "core.ss"
         "define-entity.ss"
         "snooze-struct.ss")

; Helpers --------------------------------------

(define-struct normal (a b c) #:transparent)

(define test-normal      (make-normal 1 2 3))
(define test-person-guid #f)
(define test-person      #f)
(define test-pet         #f)

; Tests ------------------------------------------

(define snooze-struct-tests
  (test-suite "snooze-struct.ss"
    
    #:before
    (lambda ()
      (set! test-person-guid (entity-make-vanilla-guid person 123))
      (set! test-person      (make-snooze-struct person test-person-guid #f "Jon"))
      (set! test-pet         (make-snooze-struct pet #f #f test-person-guid "Garfield")))
    
    (test-case "equal?"
      (check-equal?
       (make-snooze-struct person #f #f "Jon")
       (make-snooze-struct person #f #f "Jon"))
      (check-equal?
       (make-snooze-struct person test-person-guid #f "Jon")
       (make-snooze-struct person test-person-guid #f "Jon"))
      (check-not-equal?
       (make-snooze-struct person #f #f "Jon")
       (make-snooze-struct person test-person-guid #f "Jon")))
    
    (test-case "struct-entity"
      (check-eq? (struct-entity test-person) person)
      (check-exn exn:fail? (cut struct-entity test-normal)))
    
    (test-case "struct-guid"
      (check-pred guid? (struct-guid test-person))
      (check guid=? (struct-guid test-person) test-person-guid))
    
    (test-case "struct-saved?"
      (check-true (struct-saved? test-person))
      (check-false (struct-saved? (snooze-struct-set test-person (attr person guid) #f))))
    
    (test-case "struct-id"
      (check-equal? (struct-id test-person) 123)
      (check-equal? (struct-id (snooze-struct-set
                                test-person
                                (attr person guid)
                                (entity-make-vanilla-guid person 1))) 1))
    
    (test-case "struct-revision"
      (check-equal? (struct-revision test-person) #f)
      (check-equal? (struct-revision (snooze-struct-set test-person (attr person revision) 1)) 1))
    
    (test-case "snooze-struct-ref"
      (check guid=? (snooze-struct-ref test-person 'guid) test-person-guid)
      (check guid=? (snooze-struct-ref test-person (attr person guid)) test-person-guid)
      (check-equal? (snooze-struct-ref test-person 'revision) #f)
      (check-equal? (snooze-struct-ref test-person (attr person revision)) #f)
      (check guid=? (snooze-struct-ref test-pet 'owner) test-person-guid)
      (check guid=? (snooze-struct-ref test-pet (attr pet owner)) test-person-guid)
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
      (let* ([test-person2      (snooze-struct-set test-person)]
             [test-person-guid3 (entity-make-vanilla-guid person 321)]
             [test-person3      (snooze-struct-set test-person
                                                   (attr person guid)
                                                   test-person-guid3)])
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
                             (entity-make-vanilla-guid person 321))])
          (check-equal? (struct-id test-person)  123)
          (check-equal? (struct-id test-person2) #f)
          (check-equal? (struct-id test-person3) 321)))
      
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
