#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/26
         (unlib-in hash)
         "struct.ss"
         "define-entity.ss"
         "snooze-struct.ss")

; Helpers --------------------------------------

(define-struct normal (a b c) #:transparent)

(define test-normal      (make-normal 1 2 3))
(define test-person      (make-person "Jon"))
(define test-person-guid (snooze-struct-guid test-person))
(define test-pet         (make-pet test-person "Garfield"))

; Tests ------------------------------------------

(define/provide-test-suite snooze-struct-tests
    
  (test-case "equal?"
    (check-equal? (make-person "Jon") (make-person "Jon"))
    (check-not-equal? (make-person "Jon") (make-person "Fred")))
  
  (test-case "snooze-struct-entity"
    (check-eq? (snooze-struct-entity test-person) person)
    (check-exn exn:fail? (cut snooze-struct-entity test-normal)))
  
  (test-case "snooze-struct-guid"
    (check-pred guid? (snooze-struct-guid test-person))
    (check-equal? (snooze-struct-guid test-person) test-person-guid))
  
  (test-case "snooze-struct-saved?"
    (check-true (snooze-struct-saved? test-person))
    (check-false (snooze-struct-saved? (snooze-struct-set test-person (attr person guid) #f))))
  
  (test-case "snooze-struct-id"
    (check-equal? (snooze-struct-id test-person) 123)
    (check-equal? (snooze-struct-id (snooze-struct-set
                                     test-person
                                     (attr person guid)
                                     (entity-make-guid person 1))) 1))
  
  (test-case "snooze-struct-revision"
    (check-equal? (snooze-struct-revision test-person) #f)
    (check-equal? (snooze-struct-revision (snooze-struct-set test-person (attr person revision) 1)) 1))
  
  (test-case "snooze-struct-ref"
    (check-equal? (snooze-struct-ref test-person 'guid) test-person-guid)
    (check-equal? (snooze-struct-ref test-person (attr person guid)) test-person-guid)
    (check-equal? (snooze-struct-ref test-person 'revision) #f)
    (check-equal? (snooze-struct-ref test-person (attr person revision)) #f)
    (check-equal? (snooze-struct-ref test-pet 'owner) test-person-guid)
    (check-equal? (snooze-struct-ref test-pet (attr pet owner)) test-person-guid)
    (check-equal? (snooze-struct-ref test-pet 'name) "Garfield")
    (check-equal? (snooze-struct-ref test-pet (attr pet name)) "Garfield")
    (check-exn exn:fail? (cut snooze-struct-ref test-normal 'guid)))
  
  (test-case "snooze-struct-ref*"
    (for ([actual   (in-list (snooze-struct-ref* test-pet))]
          [expected (in-list (list (snooze-struct-guid test-pet)
                                   #f
                                   (snooze-struct-guid test-person)
                                   "Garfield"))])
      (check-equal? actual expected))
    (check-exn exn:fail? (cut snooze-struct-ref* test-normal)))
  
  (test-case "snooze-struct-set"
    (let* ([test-person2      (snooze-struct-set test-person)]
           [test-person-guid3 (entity-make-guid person 321)]
           [test-person3      (snooze-struct-set test-person
                                                 (attr person guid)
                                                 test-person-guid3)])
      (check-equal?     test-person test-person2)
      (check-not-eq?    test-person test-person2)
      (check-not-equal? test-person test-person3)
      (check-equal?     (cdr (snooze-struct-ref* test-person))
                        (cdr (snooze-struct-ref* test-person3)))))
  
  (test-case "make-snooze-struct/defaults"
    (check-equal? (snooze-struct-entity (make-snooze-struct/defaults person)) person)
    (let ([test-person2 (make-snooze-struct/defaults person)]
          [test-person3 (make-snooze-struct/defaults
                         person
                         (attr person guid)
                         (entity-make-guid person 321))])
      (check-equal? (snooze-struct-id test-person)  123)
      (check-equal? (snooze-struct-id test-person2) #f)
      (check-equal? (snooze-struct-id test-person3) 321))
    
    ; Bad attribute/value arguments:
    (check-exn exn:fail:contract?
      (cut make-snooze-struct/defaults person (attr person name)))
    (check-exn exn:fail:contract?
      (cut make-snooze-struct/defaults person (attr person name) (attr person guid)))
    (check-exn exn:fail:contract?
      (cut make-snooze-struct/defaults person (attr person name) "Dave" (attr person name) "Dave"))
    (check-exn exn:fail:contract?
      (cut make-snooze-struct/defaults person (attr pet name) 123)))
  
  (test-case "snooze-struct-copy"
    (let ([copy-person (snooze-struct-copy test-person)])
      (check-not-eq? copy-person test-person)
      (check-false      (snooze-struct-id       copy-person))
      (check-false      (snooze-struct-revision copy-person)))))

; Provide statements -----------------------------

(provide snooze-struct-tests)
