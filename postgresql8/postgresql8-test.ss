#lang scheme/base

(require "../test-base.ss")

(require "../snooze-api.ss")

; Tests -------------------------------------------

; test-suite
(define/provide-test-suite postgresql8-tests
  
  (test-case "parallel connections"
    (let ([thread-exn #f])
      (apply
       sync
       (for/list ([i (in-range 100)])
         (thread (lambda ()
                   (with-handlers ([(lambda _ #t) (lambda (exn) (set! thread-exn exn))])
                     (or thread-exn (call-with-connection (cut sleep 1))))))))
      (sleep 1)
      (check-false thread-exn))))