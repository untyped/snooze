#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "test-data.ss"
         "era/era.ss")

; (_ expr ...)
;
; Shorthand syntax for pushing a cache.
(define-syntax-rule (with-cache expr ...)
  (call-with-cache (lambda () expr ...)))

; -> cache%
;
; Returns the current cache.
(define (cache)
  ((get-field current-cache (current-snooze))))

; [cache%] -> (dictof guid struct)
;
; Returns the struct dictionary from the supplied/current cache.
(define (cache-data [cache (cache)])
  (send cache get-data))

; [cache%] -> (U cache% #f)
;
; Returns the parent of the supplied/current cache.
(define (cache-parent [cache (cache)])
  (send cache get-parent))

; [cache%] -> void
;
; Clears the supplied/current cache and its ancestors.
(define (cache-clear! [cache (cache)])
  (when (cache-parent cache)
    (cache-clear! (cache-parent cache)))
  (let ([hash (cache-data cache)])
    (for ([key (in-list (dict-map hash (lambda (k v) k)))])
      (dict-remove! hash key)))
  (check-equal? (cache-size cache) 0 "post-clear!"))

; -> void
(define (recreate-test-tables/cache)
  (recreate-test-tables)
  (cache-clear!))

; [cache%] -> (listof (alistof guid cached-data))
;
; Returns printable debugging information about the supplied/current cache.
(define (cache-alist [cache (cache)])
  (for/list ([item (in-dict-pairs (cache-data cache))]) item))

; [cache%] -> (listof (alistof guid cached-data))
;
; Returns printable debugging information about the supplied/current cache.
(define (cache-alists [cache (cache)])
  (if cache
      (cons (cache-alist cache)
            (cache-alist (cache-parent cache)))
      null))

; [cache%] -> alist
;
; Returns the number of guids in the supplied/current cache (ignores ancestor caches).
(define (cache-size [cache (cache)])
  (dict-count (cache-data cache)))

; (_ (listof natural))
;
; Checks that each level the current cache stack (from the current cache upwards)
; contains a certain number of guids.
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

; (_ guid guid boolean boolean boolean)
;
; Shorthand syntax for checking eq?, guid=? and equal? equality on guids/structs.
(define-syntax-rule (check-equality a b eq guid= equal)
  (begin
    (with-check-info (['actual a] ['expected b] ['comparison 'eq?])
      (if eq
          (check-true (eq? a b))
          (check-false (eq? a b))))
    
    (with-check-info (['actual a] ['expected b] ['comparison 'guid=?])
      (if guid=
          (check-true (guid=? a b))
          (check-false (guid=? a b))))
    
    (with-check-info (['actual a] ['expected b] ['comparison 'equal?])
      (if equal
          (check-true (equal? a b))
          (check-false (equal? a b))))))

; Provide statements -----------------------------

(provide (all-defined-out))
