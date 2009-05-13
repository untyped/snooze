#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "test-data.ss"
         "snooze-cache-test-util.ss"
         "era/era.ss")

; Tests -------------------------------------------

; test-suite
(define snooze-concurrency-tests
  (test-suite "snooze-concurrency-tests"
    
    #:before
    (lambda ()
      (recreate-test-tables))
    
    #:after
    drop-all-tables
    
    #;(test-suite "modifying structs"
        
        (test-case "load, modify"
          (recreate-test-tables)
          (cache-clear!)
          (check-cache-size (list 0))
          (save! (make-person "Dave"))
          (check-cache-size (list 1))
          (cache-clear!)
          (check-cache-size (list 0))
          (let* ([person1 (select-one #:from person)]
                 [person2 (person-set person1 #:name "David")])
            (check-cache-size (list 2))
            (check-not-eq?    person1
                              person2)
            (check-not-equal? person1
                              person2)
            (check-equal?     (guid-id person1)
                              (guid-id person2))
            (check-not-equal? (guid-serial person1)
                              (guid-serial person2))
            (check-equal? (person-name person1) "Dave")
            (check-equal? (person-name person2) "David")))
        
        (test-case "load, modify, reload"
          (recreate-test-tables)
          (cache-clear!)
          (check-cache-size (list 0))
          (save! (make-person "Dave"))
          (check-cache-size (list 1))
          (cache-clear!)
          (check-cache-size (list 0))
          (debug-location)
          (let*/debug ([person1  (person-set (select-one #:from person))]
                       [person2a (select-one #:from person)]
                       [person2b (person-set person2a #:name "David")]
                       [person2  (save! person2b)])
                      (check-cache-size (list 2))
                      (debug-location)
                      (let ([person3 (select-one #:from person)])
                        (debug-location)
                        (check-cache-size (list 2))
                        ;(check-not-eq? person1 person2a)
                        ;(check-not-eq? person2a person2b)
                        ;(check-not-eq? person2b person2)
                        ;(check-equal? person1 person2a)
                        ;(check-not-equal? person2a person2b)
                        ;(check-equal? person2b person2)
                        ;(check-equal? person2 person3)
                        (check-equal? (person-name person1)  "Dave")
                        (check-equal? (person-name person2a) "David")
                        (check-equal? (person-name person2b) "David")
                        (check-equal? (person-name person2)  "David")
                        (check-equal? (person-name person3)  "David"))))
        
        (test-case "load, modify, save"
          (recreate-test-tables)
          (cache-clear!)
          (check-cache-size (list 0))
          (save! (make-person "Dave"))
          (check-cache-size (list 2))
          (cache-clear!)
          (check-cache-size (list 0))
          (let* ([person1 (select-one #:from person)]
                 [person2 (save! (person-set person1 #:name "David"))])
            (check-cache-size (list 2))
            (check-equal? (person-name person1) "David")
            (check-equal? (person-name person2) "David"))))
    
    #;(test-suite "pushing and popping frames"
        
        (test-case "create, push cache"
          (cache-clear!)
          (check-cache-size (list 0))
          (let ([person1 (make-person "Dave")])
            (check-equal? (person-name person1) "Dave")
            (check-cache-size (list 1))
            (with-cache
             (check-cache-size (list 0 1))
             (check-equal? (person-name person1) "Dave")
             (check-cache-size (list 1 1)))))
        
        (test-case "push cache, create"
          (cache-clear!)
          (check-cache-size (list 0))
          (let ([person1 #f])
            (with-cache
             (check-cache-size (list 0 0))
             (set! person1 (make-person "Dave"))
             (check-cache-size (list 1 0))
             (check-equal? (person-name person1) "Dave"))
            (check-cache-size (list 0))
            (check-exn exn:fail:snooze:cache? (cut person-name person1))))
        
        (test-case "push cache, create, save"
          (cache-clear!)
          (check-cache-size (list 0))
          (let ([person1 #f]
                [person2 #f])
            (with-cache
             (check-cache-size (list 0 0))
             (set! person1 (make-person "Dave"))
             (check-cache-size (list 1 0))
             (set! person2 (save! person1))
             (check-cache-size (list 2 1))
             (check-equal? (guid-id person1) (guid-id person2))
             (check-equal? (person-name person1) "Dave")
             (check-equal? (person-name person2) "Dave"))
            (check-cache-size (list 1))
            (pretty-print (cache-alist))
            (check-exn exn:fail:snooze:cache? (cut person-name person1))
            (check-not-exn (cut person-name person2))))
        
        (test-case "push frame, load"
          (cache-clear!)
          (check-cache-size (list 0))
          (let ([guid1   #f]
                [struct1 #f])
            (with-cache
             (check-cache-size (list 0 0))
             (set! guid1   (select-one #:from person))
             (set! struct1 (send (current-snooze) cache-ref guid1))
             (check-cache-size (list 1 1))
             (check-equal? (person-name guid1) "Dave"))
            (check-cache-size (list 1))
            (check-not-eq? (send (current-snooze) cache-ref guid1) struct1))))
    
    #;(test-case "inter-struct reference: make, clear, load, traverse ref"
        (cache-clear!)
        (check-cache-size (list 0))
        (let* ([person1    (make-person "Jon")]
               [person1-id (guid-id person1)]
               [pet1       (make-pet person1 "Garfield")]
               [pet1-id    (guid-id pet1)])
          (check-cache-size (list 2))
          (debug "a" (cache-alist))
          (let ([person2 (save! person1)]
                [pet2    (save! pet1)])
            (debug "b" (cache-alist))
            (cache-clear!)
            (check-cache-size (list 0))
            (let ([pet3 (select-one #:from pet)])
              (debug "c" (cache-alist))
              (check-cache-size (list 1))
              (check-equal? (person-name (pet-owner pet3)) "Jon")
              (debug "d" (cache-alist))
              (check-cache-size (list 2))))))
    
    ))

; Provide statements -----------------------------

(provide snooze-concurrency-tests)
