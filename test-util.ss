#lang scheme/base

(require "base.ss")

(require scheme/dict
         (schemeunit-in test)
         (unlib-in hash)
         "snooze-api.ss"
         "test-data.ss"
         "era/era.ss")

; (parameter (string -> (listof (listof any))))
(define direct-query-proc
  (make-parameter (lambda (sql) (error "direct queries not initialised"))))

; string -> (listof (listof any))
(define (direct-query sql)
  ((direct-query-proc) sql))

; (_ expr ...)
;
; Shorthand syntax for pushing a cache.
(define-syntax-rule (with-cache expr ...)
  (call-with-cache (lambda () expr ...)))

; -> cache%
;
; Returns the current cache.
(define (current-cache)
  (or (send (current-snooze) get-current-cache)
      (error "no current cache")))

; [cache%] -> (dictof guid struct)
;
; Returns the struct dictionary from the supplied/current cache.
(define (cache-data [cache (current-cache)])
  (send cache get-data))

; [cache%] -> (U cache% #f)
;
; Returns the parent of the supplied/current cache.
(define (cache-parent [cache (current-cache)])
  (send cache get-parent))

; [cache%] -> void
;
; Clears the supplied/current cache and its ancestors.
(define (cache-clear! [cache (current-cache)])
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
(define (cache-list [cache (current-cache)])
  (for/list ([item (in-dict-pairs (cache-data cache))])
    (list (car item)
          (cadr item)
          (cddr item))))

; [cache%] -> (listof (alistof guid cached-data))
;
; Returns printable debugging information about the supplied/current cache.
(define (cache-lists [cache (current-cache)])
  (if cache 
      (cons (cache-list cache)
            (cache-lists (cache-parent cache)))
      null))

; [cache%] -> alist
;
; Returns the number of guids in the supplied/current cache (ignores ancestor caches).
(define (cache-size [cache (current-cache)])
  (dict-count (cache-data cache)))

; (_ (listof natural))
;
; Checks that each level the current cache stack (from the current cache upwards)
; contains a certain number of guids.
(define-check (check-cache-size expected)
  (let ([actual (cache-lists)])
    (with-handlers ([exn? (lambda (exn)
                            (pretty-print actual)
                            (raise exn))])
      (check-equal? (length actual) (length expected) "wrong number of caches")
      (for ([actual   (in-list actual)]
            [expected (in-list expected)]
            [depth    (in-naturals)])
        (check-equal? (length actual) expected (format "wrong number of entries at depth ~a" depth))))))

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
