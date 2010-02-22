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
    (check-eq? (snooze-struct-entity (make-person/defaults)) person)
    (check-exn exn:fail? 
      (cut snooze-struct-entity (make-normal 1 2 3))))
  
  (test-case "snooze-struct-guid"
    (let ([p (make-person/defaults #:name "P")])
      (check-pred guid? (snooze-struct-guid p))
      (let ([p2 (save! p)])
        (check-eq? (snooze-struct-guid p)
                   (snooze-struct-guid p2))
        (let ([p3 (delete! p2)])
          (check-eq? (snooze-struct-guid p)
                     (snooze-struct-guid p3))))))
  
  (test-case "snooze-struct-saved?, snooze-struct-has-revision?"
    (let ([p (make-person/defaults #:name "P")])
      (check-false (snooze-struct-saved? p))
      (check-false (snooze-struct-has-revision? p))
      (let ([p2 (save! p)])
        (check-true (snooze-struct-saved? p))
        (check-true (snooze-struct-saved? p2))
        (check-false (snooze-struct-has-revision? p))
        (check-true  (snooze-struct-has-revision? p2))
        (let ([p3 (delete! p2)])
          (check-false (snooze-struct-saved? p))
          (check-false (snooze-struct-saved? p2))
          (check-false (snooze-struct-saved? p3))
          (check-false (snooze-struct-has-revision? p))
          (check-true  (snooze-struct-has-revision? p2))
          (check-false (snooze-struct-has-revision? p3))))))
  
  (test-case "snooze-struct-id"
    (let ([p (make-person/defaults #:name "P")])
      (check-pred symbol? (snooze-struct-id p))
      (let ([p2 (save! p)])
        (check-pred number? (snooze-struct-id p2))
        (check-eq? (snooze-struct-id p) (snooze-struct-id p2))
        (let ([p3 (delete! p2)])
          (check-pred symbol? (snooze-struct-id p3))
          (check-eq? (snooze-struct-id p) (snooze-struct-id p2))
          (check-eq? (snooze-struct-id p) (snooze-struct-id p3))))))
  
  (test-case "snooze-struct-revision"
    (let ([p (make-person/defaults #:name "P")])
      (check-false (snooze-struct-revision p))
      (let ([p2 (save! p)])
        (check-false (snooze-struct-revision p))
        (check-pred integer? (snooze-struct-revision p2))
        (let ([p3 (delete! p2)])
          (check-false (snooze-struct-revision p))
          (check-pred integer? (snooze-struct-revision p2))
          (check-false (snooze-struct-revision p3))))))
  
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
      (check-equal? (snooze-struct-ref q1 (attr person name)) "Q")
      (check-eq? (snooze-struct-guid p) (snooze-struct-guid q1))
      (check-eq? (snooze-struct-guid p) (snooze-struct-guid q2))))
  
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
      (check-equal? (cdr (snooze-struct-raw-ref* p))
                    (cdr (snooze-struct-raw-ref* q)))))
  
  (test-case "snooze-struct-{guid,data,}-equal?"
    (let ([p  (make-person/defaults #:name "P")])
      (let ([q (person-set p #:name "P2")])
        (check-true  (snooze-struct-guid-equal? p q))
        (check-false (snooze-struct-data-equal? p q))
        (check-false (snooze-struct-equal?      p q)))
      (let ([q (make-person/defaults #:name "P")])
        (check-false (snooze-struct-guid-equal? p q))
        (check-true  (snooze-struct-data-equal? p q))
        (check-false (snooze-struct-equal?      p q)))
      (let ([q (make-person/defaults #:name "Q")])
        (check-false (snooze-struct-guid-equal? p q))
        (check-false (snooze-struct-data-equal? p q))
        (check-false (snooze-struct-equal?      p q)))
      (let ([q (save! p)])
        (check-true (snooze-struct-guid-equal? p q))
        (check-true (snooze-struct-data-equal? p q))
        (check-true (snooze-struct-equal?      p q)))
      (let ([q (find-by-guid (snooze-struct-guid p))])
        (check-true (snooze-struct-guid-equal? p q))
        (check-true (snooze-struct-data-equal? p q))
        (check-true (snooze-struct-equal?      p q))))))

; Provide statements -----------------------------

(provide snooze-struct-tests)
