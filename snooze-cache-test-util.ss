#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "test-data.ss"
         "era/era.ss")

(define-syntax-rule (with-cache expr ...)
  (call-with-cache (lambda () expr ...)))

(define (cache)
  ((get-field current-cache (current-snooze))))

(define (cache-hash [cache (cache)])
  (send cache get-structs))

(define (cache-parent [cache (cache)])
  (send cache get-parent))

(define (cache-clear! [cache (cache)])
  (when (cache-parent cache)
    (cache-clear! (cache-parent cache)))
  (let ([hash (cache-hash cache)])
    (for ([key (in-list (dict-map hash (lambda (k v) k)))])
      #;(pretty-print (list* 'REMOVING
                             key
                             (guid=?-hash-code key)
                             (for/list ([item (in-dict-pairs (cache-hash cache))])
                               (list (car item) (guid=? key (car item)) (guid=?-hash-code (car item))))))
      (dict-remove! hash key)
      #;(pretty-print (list* 'REMOVED
                             key
                             (guid=?-hash-code key)
                             (for/list ([item (in-dict-pairs (cache-hash cache))])
                               (list (car item) (guid=? key (car item)) (guid=?-hash-code (car item))))))))
  (check-equal? (cache-size cache) 0 "post-clear!"))

(define (cache-alist [cache (cache)])
  (list (cons 'CONTENT (for/list ([item (in-dict-pairs (cache-hash cache))])
                         item))
        (cons 'PARENT  (if (cache-parent cache)
                           (cache-alist (cache-parent cache))
                           #f))))

(define (cache-size [cache (cache)])
  (dict-count (cache-hash cache)))

(define-check (check-cache-size expected)
  (with-check-info (['actual (cache-alist (cache))])
    (let loop ([cache (cache)] [expected expected] [level 0])
      (if (null? expected)
          (check-false cache "too many levels of caching")
          (begin (check-not-false cache "too few levels of caching")
                 (check-equal? 
                  (cache-size cache)
                  (car expected)
                  (format "wrong number of cached structs at depth ~a" level))
                 (loop (cache-parent cache) (cdr expected) (add1 level)))))))

; Provide statements -----------------------------

(provide (all-defined-out))
