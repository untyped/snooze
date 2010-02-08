#lang scheme/base

(require "../test-base.ss")

(require scheme/class)

; Tests -------------------------------------------

; test-suite
(define (make-postgresql8-tests snooze)
  (test-suite "postgresql8 tests"
    
    (test-case "parallel connections"
      (let ([thread-exn #f])
        (apply
         sync
         (for/list ([i (in-range 100)])
           (thread (lambda ()
                     (with-handlers ([(lambda _ #t) (lambda (exn) (set! thread-exn exn))])
                       (or thread-exn (send snooze call-with-connection (cut sleep 1) #f)))))))
        (sleep 1)
        (check-pred exn:fail:snooze:connection-count? thread-exn)))))

; Provides ---------------------------------------

(provide make-postgresql8-tests)
