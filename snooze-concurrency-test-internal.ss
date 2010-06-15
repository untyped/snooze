#lang scheme/base

(require "test-base.ss")

(require (for-syntax scheme/base
                     (planet untyped/unlib:3/for)
                     (planet untyped/unlib:3/syntax))
         "snooze.ss")

; (_ [natural expr ...] ...)
; Each clause is a thread number (natural), followed by a block of expressions.
; The blocks are executed sequentialy in clause order, but in separate threads as indicated by the thread numbers.
(define-syntax (threadify stx)
  (syntax-case stx ()
    [(_ [id expr ...] ...)
     (let ([min-thread-id (apply min (map syntax->datum (syntax->list #'(id ...))))]
           [max-thread-id (apply max (map syntax->datum (syntax->list #'(id ...))))])
       
       (with-syntax ([(((block-id block-expr ...) ...) ...)
                      (for/list ([thread-num (in-range min-thread-id (add1 max-thread-id))])
                        (for/fold/reverse
                         ([accum null])
                         ([block-id  (in-naturals)]
                          [block-stx (in-list (syntax->list #'([id expr ...] ...)))])
                         (syntax-case block-stx ()
                           [(id expr ...)
                            (if (equal? (syntax->datum #'id) thread-num)
                                (cons #`(#,block-id expr ...) accum)
                                accum)])))]
                     [final-block-id (length (syntax->list #'(id ...)))]
                     [(sem ...)
                      (for/list ([block-id  (in-naturals)]
                                 [block-stx (in-list (cons #f (syntax->list #'(id ...))))])
                        (if (zero? block-id)
                            #'(make-semaphore 1)
                            #'(make-semaphore 0)))])
         
         #'(let ([final-exn  #f]
                 [semaphores (list sem ...)])
             (sync (thread
                    (lambda ()
                      (with-connection
                        (with-handlers ([exn? (lambda (exn)
                                                (set! final-exn exn)
                                                (raise exn))])
                          (begin
                            (semaphore-wait (list-ref semaphores block-id))
                            block-expr ...
                            (semaphore-post (list-ref semaphores (add1 block-id))))
                          ...
                          (semaphore-wait (list-ref semaphores final-block-id))))))
                   ...)
             (when final-exn
               (raise final-exn)))))]))

; Provides ---------------------------------------

(provide threadify)