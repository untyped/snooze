#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/19
         srfi/26
         (unlib-in hash)
         "core.ss")

(require/expose "entity.ss"
  (name->database-name))

; Tests ----------------------------------------

(define define-entity-tests
  (test-suite "define-entity.ss"
    
    (test-case "entity"
      (check-pred entity? person))
    
    (test-case "struct-type"
      (check-pred struct-type? struct:person))
    
    (test-case "constructor and predicate"
      (check-pred procedure? make-person)
      (check-pred procedure? person?)
      (check-true  (person? (make-person "Jon")))
      (check-false (person? (make-pet #f "Odie")))
      (check-false (person? #f)))
    
    (test-case "struct equality"
      (let ([a1 (make-person "A")]
            [a2 (make-person "A")])
        (check-not-equal? a1 a2)
        (check-equal? (cddr (snooze-struct-ref* a1))
                      (cddr (snooze-struct-ref* a2)))
        (check-not-eq? a1 a2)))
    
    (test-equal? "keyword constructor"
      (cdr (snooze-struct-ref* (make-person/defaults #:name "Dave")))
      (cdr (snooze-struct-ref* (make-person "Dave"))))
    
    (test-case "copy constructor"
      (let ([original (make-person "Dave")])
        (check-equal? (cdr (snooze-struct-ref* (person-set original)))
                      (cdr (snooze-struct-ref* original)))
        (check-eq? (snooze-struct-guid (person-set original))
                   (snooze-struct-guid original))))
    
    (test-case "name->table-name"
      (check-equal? (name->database-name 'person)        'person)
      (check-equal? (name->database-name 'person-record) 'personrecord)
      (check-equal? (name->database-name 'PersonRecord)  'personrecord))
    
    (test-case "attr"
      (let ([a (attr person guid)]
            [b (attr person name)])
        (check-true (andmap attribute? (list a b)))
        (check-not-eq? a b)
        (check-equal? (map attribute-name (list a b)) '(guid name))
        (check-equal? (map attribute-entity (list a b)) (list person person))))
    
    (test-case "attr-list"
      (let ([attrs (attr-list person guid name)])
        (check-equal? (attr-list person guid name)
                      (list (attr person guid)
                            (attr person name)))))
        
    (test-case "contracts"
      ; Constracts don't check constraints like NOT NULL and string length.
      ; They only check that fields are of the right type or null type.
      (check-not-exn (cut make-course/defaults #:code #f))
      (check-exn exn:fail:contract? (cut make-course/defaults #:code "not a symbol"))
      (check-not-exn (cut make-course/defaults #:code 'bittoolong))
      (check-not-exn (cut make-course/defaults #:name (make-string 129 #\a)))
      (check-not-exn (cut make-course/defaults #:value 0))
      (check-not-exn (cut make-course/defaults #:value 5))
      (check-not-exn (cut make-course/defaults #:value -1))
      (check-not-exn (cut make-course/defaults #:value 6))
      (check-not-exn (cut make-course/defaults #:rating 0.0))
      (check-not-exn (cut make-course/defaults #:rating 1.0))
      (check-not-exn (cut make-course/defaults #:rating -0.001))
      (check-not-exn (cut make-course/defaults #:rating 1.001))
      (check-exn exn:fail:contract? (cut make-course/defaults #:start (current-time time-utc)))
      (check-not-exn (cut make-course/defaults #:notes '(a b c)))
      (check-exn exn:fail:contract? (cut make-course/defaults #:notes (lambda (x) (add1 x))))
      (check-not-exn (cut make-tree-node/defaults #:color 'red))
      (check-not-exn (cut make-tree-node/defaults #:color (color red)))
      (check-exn exn:fail:contract? (cut make-tree-node/defaults #:color 'white)))
    
    (test-case "attr/c"
      (check-pred flat-contract? (attr/c person name))
      (let ([contract (attr/c person name)])
        (check-false ((flat-contract-predicate contract) (attr person name)))
        (check-true  ((flat-contract-predicate contract) "Name"))
        (check-true  ((flat-contract-predicate contract) #f)))
      (let ([contract (attr/c person guid)])
        (check-false ((flat-contract-predicate contract) (attr pet owner)))
        (check-false ((flat-contract-predicate contract) "Name"))
        ; Doesn't like guids:
        (check-false ((flat-contract-predicate contract) (entity-make-temporary-guid person)))
        ; Does like snooze-structs:
        (check-true  ((flat-contract-predicate contract) (make-person "Dave")))
        (check-true  ((flat-contract-predicate contract) #f))))))

; Provide statements -----------------------------

(provide define-entity-tests)
