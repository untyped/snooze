#lang scheme/base

(require "test-base.ss")

(require (unlib-in hash)
         "snooze-api.ss"
         "test-data.ss"
         (only-in "era/core.ss" guid-id))

;(thread (lambda ()
;          (let ([receiver (make-log-receiver (current-logger) 'info)])
;            (let loop ()
;              (let ([msg (sync receiver)])
;                (printf "~a~n" msg)
;                (loop))))))

(define-syntax-rule (with-cache expr ...)
  (call-with-cache (lambda () expr ...)))

(define (cache)
  ((get-field current-cache (current-snooze))))

(define (cache-hash [cache (cache)])
  (send cache get-structs))

(define (cache-parent [cache (cache)])
  (send cache get-parent))

(define (cache-clear! [cache (cache)])
  (let ([guid-hash (send (send (current-snooze) get-guid-cache) get-guids)])
    (for ([key (in-list (hash-keys guid-hash))])
      (hash-remove! guid-hash key)))
  (when (cache-parent cache)
    (cache-clear! (cache-parent cache)))
  (let ([hash (cache-hash cache)])
    (for ([key (in-list (hash-keys hash))])
      (hash-remove! hash key)))
  (check-equal? (cache-size cache) 0 "post-clear!"))

(define (cache-alist [cache (cache)])
  (list (cons 'CONTENT (for/list ([item (in-hash-pairs (cache-hash))])
                         item))
        (cons 'PARENT  (if (cache-parent cache)
                           (cache-alist (cache-parent cache))
                           #f))))

(define (cache-size [cache (cache)])
  (length (hash-keys (cache-hash cache))))

(define-check (check-cache-size expected)
  (with-check-info (['actual (cache-alist (cache))])
    (let loop ([cache (cache)] [expected expected] [level 0])
      (if (null? expected)
          (check-false cache "too many levels of caching")
          (begin (check-not-false cache "too few levels of caching")
                 (check-equal? 
                  (cache-size cache)
                  (car expected)
                  (format "wrong number of cached structs ~a levels deep" level))
                 (loop (cache-parent cache) (cdr expected) (add1 level)))))))

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
      (check-cache-size (list 0))
      (let ([guid1 (make-person "Dave")]
            [guid2 (make-person "Dave")])
        (debug "param" (get-field current-cache (current-snooze)))
        (debug "cache" ((get-field current-cache (current-snooze))))
        (debug "hash"  (get-field structs ((get-field current-cache (current-snooze)))))
        (debug "stored" (cache-alist))
        (check-cache-size (list 2))
        (check-equal? (person-name guid1) "Dave")
        (check-equal? (person-name guid2) "Dave")
        (check-equal?  guid1 guid2)
        (check-not-eq? guid1 guid2)))
    
    (test-case "create, push"
      (cache-clear!)
      (check-cache-size (list 0))
      (let ([guid1 (make-person "Dave")])
        (check-equal? (person-name guid1) "Dave")
        (check-cache-size (list 1))
        (with-cache
         (check-cache-size (list 0 1))
         (check-equal? (person-name guid1) "Dave")
         (check-cache-size (list 1 1)))))
    
    (test-case "push, create"
      (cache-clear!)
      (check-cache-size (list 0))
      (let ([guid1 #f])
        (with-cache
         (check-cache-size (list 0 0))
         (set! guid1 (make-person "Dave"))
         (check-cache-size (list 1 0))
         (check-equal? (person-name guid1) "Dave"))
        (check-cache-size (list 0))
        (check-exn exn:fail:snooze:cache? (cut person-name guid1))))
    
    (test-case "push, create, save"
      (cache-clear!)
      (check-cache-size (list 0))
      (let ([guid1 #f])
        (with-cache
         (check-cache-size (list 0 0))
         (set! guid1 (make-person "Dave"))
         (check-cache-size (list 1 0))
         (save! guid1)
         (check-cache-size (list 1 1))
         (check-equal? (person-name guid1) "Dave"))
        (check-cache-size (list 1))
        (check-not-exn (cut person-name guid1))))
    
    (test-case "load"
      (cache-clear!)
      (check-cache-size (list 0))
      (let ([guid1 (select-one #:from person)]
            [guid2 (select-one #:from person)])
        (check-cache-size (list 1))
        (check-equal? (person-name guid1) "Dave")
        (check-eq? guid1 guid2)))
    
    (test-case "push, load"
      (cache-clear!)
      (check-cache-size (list 0))
      (let ([guid1 #f]
            [struct1 #f])
        (with-cache
         (check-cache-size (list 0 0))
         (set! guid1   (select-one #:from person))
         (set! struct1 (send (current-snooze) cache-ref guid1))
         (check-cache-size (list 1 1))
         (check-equal? (person-name guid1) "Dave"))
        (check-cache-size (list 1))
        (check-not-eq? (send (current-snooze) cache-ref guid1) struct1)))
    
    (test-case "inter-struct reference"
      (cache-clear!)
      (check-cache-size (list 0))
      (let* ([person1    (make-person "Jon")]
             [person1-id (guid-id person1)]
             [pet1       (make-pet person1 "Garfield")]
             [pet1-id    (guid-id pet1)])
        
        (check-cache-size (list 2))
        (save! person1)
        (save! pet1)
        (cache-clear!)
        (check-cache-size (list 0))
        (set! pet1 (select-one #:from pet))
        (check-cache-size (list 1))
        (check-equal? (person-name (pet-owner pet1)) "Jon")
        (check-cache-size (list 2))))))

; Provide statements -----------------------------

(provide snooze-concurrency-tests)
