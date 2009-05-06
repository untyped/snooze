#lang scheme/base

(require "test-base.ss")

(require (unlib-in hash)
         "snooze-api.ss"
         "test-data.ss")

(define-syntax-rule (with-cache expr ...)
  (call-with-cache (lambda () expr ...)))

(define (cache-hash)
  (send ((get-field current-cache (current-snooze))) get-structs))

(define (cache-clear!)
  (let ([hash (cache-hash)])
    (for ([key (in-list (hash-keys hash))])
      (hash-remove! hash key))))

(define (cache-alist)
  (for/list ([item (in-hash-pairs (cache-hash))])
    item))

(define-check (check-cache-size expected)
  (let ([alist (cache-alist)])
    (with-check-info (['cached alist])
      (check-equal? (length alist) expected))))

; Tests -------------------------------------------

; test-suite
(define snooze-concurrency-tests
  (test-suite "snooze-concurrency-tests"
    
    #:before
    recreate-test-tables
    
    #:after
    drop-all-tables
    
    (test-case "create"
      (cache-clear!)
      (check-cache-size 0)
      (let ([p1 (make-person "Dave")]
            [p2 (make-person "Dave")])
        (debug "param" (get-field current-cache (current-snooze)))
        (debug "cache" ((get-field current-cache (current-snooze))))
        (debug "hash"  (get-field structs ((get-field current-cache (current-snooze)))))
        (debug "stored" (cache-alist))
        (check-cache-size 2)
        (check-equal? (person-name p1) "Dave")
        (check-equal? (person-name p2) "Dave")
        (check-equal?  p1 p2)
        (check-not-eq? p1 p2)))
    
    (test-case "create, push"
      (cache-clear!)
      (check-cache-size 0)
      (let ([p1 (make-person "Dave")])
        (check-equal? (person-name p1) "Dave")
        (check-cache-size 1)
        (with-cache
         (check-cache-size 0)
         (check-equal? (person-name p1) "Dave")
         (check-cache-size 1))))
    
    (test-case "push, create"
      (cache-clear!)
      (check-cache-size 0)
      (let ([p1 #f])
        (with-cache
         (check-pred null? (cache-alist))
         (set! p1 (make-person "Dave"))
         (check-cache-size 1)
         (check-equal? (person-name p1) "Dave"))
        (check-cache-size 0)
        (check-exn exn:fail:snooze:cache? (cut person-name p1))))
    
    (test-case "push, create, save"
      (cache-clear!)
      (check-cache-size 0)
      (let ([p1 #f])
        (with-cache
         (set! p1 (save! (make-person "Dave")))
         (check-equal? (person-name p1) "Dave"))
        (check-not-exn (cut person-name p1))))
    
    (test-case "load"
      (cache-clear!)
      (check-cache-size 0)
      (let ([p1 (select-one #:from person)]
            [p2 (select-one #:from person)])
        (check-cache-size 1)
        (check-equal? (person-name p1) "Dave")
        (check-eq? p1 p2)))
    
    (test-case "push, load"
      (cache-clear!)
      (check-cache-size 0)
      (let ([p1 #f]
            [s1 #f])
        (with-cache
         (set! p1 (select-one #:from person))
         (set! s1 (send (current-snooze) cache-ref p1))
         (check-cache-size 1)
         (check-equal? (person-name p1) "Dave"))
        (check-cache-size 1)
        (check-not-eq? (send (current-snooze) cache-ref p1) s1)))))

; Provide statements -----------------------------

(provide snooze-concurrency-tests)
