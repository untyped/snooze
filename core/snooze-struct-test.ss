#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/26
         (unlib-in hash)
         "../snooze.ss")

; Helpers --------------------------------------

(define-struct normal (a b c) #:transparent)

(define test-normal      (make-normal 1 2 3))
(define test-person      (make-person "Jon"))
(define test-person-guid (snooze-struct-guid test-person))
(define test-pet         (make-pet test-person "Garfield"))

; Tests ------------------------------------------

(define/provide-test-suite snooze-struct-tests
  
  (test-case "snooze-struct-entity"
    (check-eq? (snooze-struct-entity test-person) person)
    (check-exn exn:fail? (cut snooze-struct-entity test-normal)))
  
  (test-case "snooze-struct-guid"
    (check-pred guid? (snooze-struct-guid test-person))
    (check-equal? (snooze-struct-guid test-person) test-person-guid))
  
  (test-case "snooze-struct-saved?"
    (let ([p (make-person/defaults #:name "P")])
      (check-false (snooze-struct-saved? p))
      (check-true (snooze-struct-saved? (save! p)))
      (check-true (snooze-struct-saved? p))
      (check-false (snooze-struct-saved? (delete! p)))
      (check-false (snooze-struct-saved? p))))
  
  (test-case "snooze-struct-id"
    (let ([p (make-person/defaults #:name "P")])
      (check-pred symbol? (snooze-struct-id p))
      (check-pred number? (snooze-struct-id (save! p)))
      (check-pred number? (snooze-struct-id p))
      (check-pred symbol? (snooze-struct-id (delete! p)))
      (check-pred symbol? (snooze-struct-id p))))
  
  (test-case "snooze-struct-revision"
    (let ([p (make-person/defaults #:name "P")])
      (check-false (snooze-struct-revision p))
      (check-pred number? (snooze-struct-revision (save! p)))
      (check-pred number? (snooze-struct-revision p))
      (check-false (snooze-struct-revision (delete! p)))
      (check-false (snooze-struct-revision p))))
  
  (test-case "snooze-struct-ref"
    (let ([p (make-person/defaults #:name "P")])
      (check-pred temporary-guid? (snooze-struct-ref p 'guid))
      (check-pred temporary-guid? (snooze-struct-ref p (attr person guid)))
      (check-false (snooze-struct-ref p 'revision))
      (check-false (snooze-struct-ref p (attr person revision)))
      (check-equal? (snooze-struct-ref p 'name) "P")
      (check-equal? (snooze-struct-ref p (attr person name)) "P")))
  
  (test-case "snooze-struct-ref*"
    (let ([p (make-person/defaults #:name "P")])
      (check-equal? (snooze-struct-ref* p)
                    (map (cut snooze-struct-ref p <>)
                         (attr-list person guid revision name)))))
  
  (test-case "snooze-struct-set"
    (let* ([p  (make-person/defaults #:name "P")]
           [q1 (snooze-struct-set p 'name "Q")]
           [q2 (snooze-struct-set p (attr person name) "Q")])
      (check-not-eq?  p q1)
      (check-not-eq?  p q2)
      (check-not-eq? q1 q2)
      (check-equal?  q1 q2)
      (check-equal? (snooze-struct-ref q1 'name) "Q")
      (check-equal? (snooze-struct-ref q1 (attr person name)) "Q")))
  
  (test-case "make-snooze-struct/defaults"
    (let ([p (make-snooze-struct/defaults person)])
      (check-pred temporary-guid? (snooze-struct-guid p))
      (check-false (snooze-struct-revision p))
      (check-false (person-name p)))
    (let ([p (make-snooze-struct/defaults person 'name "P")])
      (check-pred temporary-guid? (snooze-struct-guid p))
      (check-false (snooze-struct-revision p))
      (check-equal? (person-name p) "P"))
    (let ([p (make-snooze-struct/defaults person (attr person name) "P")])
      (check-pred temporary-guid? (snooze-struct-guid p))
      (check-false (snooze-struct-revision p))
      (check-equal? (person-name p) "P")))
  
  (test-case "snooze-struct-copy"
    (let* ([p (make-snooze-struct/defaults person)]
           [q (snooze-struct-copy p)])
      (check-not-eq? p q)
      (check-not-equal? p q)
      (check-equal? (cdr (snooze-struct-ref* p))
                    (cdr (snooze-struct-ref* q))))))

; Provide statements -----------------------------

(provide snooze-struct-tests)
