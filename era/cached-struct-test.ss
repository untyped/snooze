#lang scheme/base

(require "../test-base.ss")

(require (only-in srfi/1 take)
         srfi/26
         (unlib-in hash)
         "../test-data.ss"
         "era.ss")

; Helpers --------------------------------------

(define-struct normal (a b c) #:transparent)

; Tests ------------------------------------------

(define cached-struct-tests
  (test-suite "cahed-struct.ss"
    
    (test-case "struct-entity"
      (check-eq? (struct-entity (make-person "Dave")) person)
      (check-exn exn:fail? (cut struct-entity (make-normal 1 2 3))))
    
    (test-case "struct-guid"
      (let ([struct (make-person "Dave")])
        (check-equal? (guid-id (struct-guid struct))
                      (guid-id (entity-make-guid person #f)))
        (check-equal? (guid-id (struct-guid struct))
                      (guid-id (struct-guid (make-person "Dave"))))
        (check-not-equal? (struct-guid struct)
                          (struct-guid (make-person "Dave2")))))
    
    (test-case "struct-saved?"
      (let ([struct (make-person "Dave")])
        (check-false (struct-saved? struct))
        (set-guid-id! (struct-guid struct) 1)
        (check-true (struct-saved? struct))))
    
    (test-case "struct-revision and set-struct-revision!"
      (let ([struct (make-person "Dave")])
        (check-equal? (struct-revision struct) #f)
        (set-struct-revision! struct 1000)
        (check-equal? (struct-revision struct) 1000)))
    
    (test-case "field accessors and mutators"
      (let ([struct (make-pet 3 "Garfield")])
        (set-struct-id! struct 1)
        (set-struct-revision! struct 2)
        
        (check-equal? (pet-owner-id struct) 3)
        (check-equal? (pet-name struct) "Garfield")
        
        (set-pet-owner-id! struct 4)
        (set-pet-name! struct "Odie")
        
        (check-equal? (pet-owner-id struct) 4)
        (check-equal? (pet-name struct) "Odie")))
    
    (test-case "snooze-struct-ref"
      (let* ([struct (make-pet 3 "Garfield")]
             [guid1  (copy-snooze-struct struct
                                         (attr pet guid)     (entity-make-guid pet 1)
                                         (attr pet revision) 2)])
        (set-struct-id! struct 1)
        (set-struct-revision! struct 2)
        
        (check-equal? (snooze-struct-ref struct 'guid) guid1)
        (check-equal? (snooze-struct-ref struct (attr pet guid)) guid1)
        (check-equal? (snooze-struct-ref struct 'revision) 2)
        (check-equal? (snooze-struct-ref struct (attr pet revision)) 2)
        (check-equal? (snooze-struct-ref struct 'owner-id) 3)
        (check-equal? (snooze-struct-ref struct (attr pet owner-id)) 3)
        (check-equal? (snooze-struct-ref struct 'name) "Garfield")
        (check-equal? (snooze-struct-ref struct (attr pet name)) "Garfield")
        
        (check-exn exn:fail? (cut snooze-struct-ref (make-normal 1 2 3) 'guid))))
    
    (test-case "snooze-struct-ref*"
      (let ([struct (make-pet 3 "Garfield")])
        (set-struct-id! struct 1)
        (set-struct-revision! struct 2)
        
        (check-equal? (snooze-struct-ref* struct)
                      (list struct 2 3 "Garfield"))
        
        (check-exn exn:fail? (cut snooze-struct-ref* (make-normal 1 2 3)))))
    
    (test-case "snooze-struct-set!"
      
      (define-check (check-attribute struct attr+name expected)
        (check-false (equal? (snooze-struct-ref struct attr+name) expected))
        (snooze-struct-set! struct attr+name expected)
        (check-equal? (snooze-struct-ref struct attr+name) expected))
      
      ; Guids:
      (let ([struct (make-pet 3 "Garfield")]
            [guid   (entity-make-guid pet 100)])
        (check-not-equal? (struct-id struct) 100)
        (snooze-struct-set! struct 'guid guid)
        (check-equal? (struct-id struct) 100)
        (check-not-eq? (struct-guid struct) guid)
        (check-exn exn:fail:contract?
          (cut snooze-struct-set! struct 'guid 100))
        (check-exn exn:fail:contract?
          (cut snooze-struct-set! struct (attr pet guid) 100)))
      
      ; Other attributes:
      (let ([struct (make-pet 3 "Garfield")])
        (check-attribute struct 'revision 100)
        (check-attribute struct (attr pet revision) 300)
        (check-attribute struct 'owner-id 100)
        (check-attribute struct (attr pet owner-id) 200)
        (check-attribute struct 'name "Odie")
        (check-attribute struct (attr pet name) "Nermal")))
    
    (test-case "make-snooze-struct/defaults"
      (check-equal? (make-snooze-struct/defaults person)
                    (make-person #f))
      (check-equal? (make-snooze-struct/defaults
                     person
                     (attr person name)
                     "Dave")
                    (make-person "Dave"))
      
      ; Guid (in)equality:
      (let ([struct1 (make-snooze-struct/defaults
                      person
                      (attr person guid)
                      (entity-make-guid person 123))]
            [struct2 (make-snooze-struct/defaults
                      person
                      (attr person guid)
                      (entity-make-guid person 123))])
        (check-equal? (struct-id   struct1) 123)
        (check-equal? (struct-guid struct1) (struct-guid struct2))
        (check-false  (eq? (struct-guid struct1) (struct-guid struct2))))
      
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
      (let* ([struct1 (make-person/defaults #:name "Dave")])
        (let ([struct2 (copy-snooze-struct struct1)])
          
          ; Struct equality:
          (check-true  (equal? struct2 struct1))
          (check-false (eq? struct2 struct1))
          
          ; Guids:
          (check-pred  guid? (struct-guid struct1))
          (check-pred  guid? (struct-guid struct2))
          (check-equal? (struct-guid struct1)      (struct-guid struct2))
          (check-false  (eq? (struct-guid struct1) (struct-guid struct2)))
          
          ; Attributes:
          (check-equal? (struct-id struct2)        (struct-id struct1))
          (check-equal? (struct-revision struct2)  (struct-revision struct1))
          (check-equal? (person-name struct2)      (person-name struct1)))
        
        (let ([struct2 (copy-snooze-struct struct1 (attr person name) "Noel")])
          (check-not-equal? struct1                   struct2)
          (check-not-eq?    struct1                   struct2)
          (check-equal?     (struct-id struct2)       (struct-id struct1))
          (check-equal?     (struct-revision struct2) (struct-revision struct1))
          (check-not-equal? (person-name struct2)     (person-name struct1)))))
    
    (test-case "update-snooze-struct-from-copy!"
      (let* ([guid    (entity-make-guid pet 20)]
             [struct1 (make-snooze-struct/defaults
                       pet
                       (attr pet owner-id) 1000
                       (attr pet name)     "Garfield")]
             [struct2 (make-snooze-struct/defaults
                       pet
                       (attr pet guid)     guid
                       (attr pet revision) 200
                       (attr pet owner-id) 2000
                       (attr pet name)     "Heathcliff")])
        
        (update-snooze-struct-from-copy! struct1 struct2)
        
        (check-not-eq? struct2 struct1)
        (check-eq?     struct2 guid)
        
        (check-equal? (struct-id struct2)       20)
        (check-equal? (struct-revision struct1) 200)
        (check-equal? (snooze-struct-ref struct1 'owner-id) 2000)
        (check-equal? (snooze-struct-ref struct1 'name) "Heathcliff")
        (check-equal? struct1 struct2)
        (check-false (eq? struct1 struct2))))))

; Provide statements -----------------------------

(provide cached-struct-tests)
