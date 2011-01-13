#lang scheme/base

(require "../test-base.ss")

(require scheme/dict
         "../snooze-api.ss"
         "connection-pool.ss")

; Helpers ----------------------------------------

; (-> any) -> (listof natural) (listof natural)
(define (sample-connections thunk)
  (let ([accum1  null]
        [accum2  null]
        [finish? #f])
    (thread (lambda ()
              (let loop ()
                (unless finish?
                  (set! accum1 (cons (claimed-count) accum1))
                  (set! accum2 (cons (unclaimed-count) accum2))
                  (quick-sleep (quotient (current-keepalive) 10))
                  (loop)))))
    (thunk)
    (quick-sleep)
    (set! finish? #t)
    (values (simplify accum1) 
            (simplify accum2))))

; (list natural natural natural natural) thunk -> void
(define-check (check-counts counts thunk)
  (match counts
    [(and counts (list claimed-min
                         claimed-max
                         unclaimed-min
                         unclaimed-max))
       (let-values ([(claimed-profile unclaimed-profile)
                     (sample-connections thunk)])
         (with-check-info (['claimed-profile   claimed-profile]
                           ['unclaimed-profile unclaimed-profile])
           (check-equal? (list (apply min claimed-profile)
                               (apply max claimed-profile)
                               (apply min unclaimed-profile)
                               (apply max unclaimed-profile))
                         counts)))]))

; Tests ------------------------------------------

(define-test-suite connection-pool-tests
  
  #:before
  (lambda ()
    (printf "Connection pool tests starting.~n")
    (send (current-snooze) disconnect)
    (quick-sleep (current-keepalive)))
  
  #:after
  (lambda ()
    (send (current-snooze) connect)
    (printf "Connection pool tests complete.~n"))
  
  (test-case "sanity check"
    ; Mustn't have a connection already claimed when we run these tests:
    (check-counts (list 0 0 10 10) void))
  
  (test-case "one connection"
    (check-counts
     (list 0 1 9 10) 
     (lambda ()
       (call-with-connection
        (lambda ()
          (quick-sleep (* (current-keepalive) 2))))
       (quick-sleep (* (current-keepalive) 2)))))
  
  (test-case "parallel connections"
    (check-counts
     (list 0 4 6 10) 
     (lambda ()
       (apply
        sync
        (for/list ([i (in-range 4)])
          (thread (lambda ()
                    (call-with-connection
                     (lambda ()
                       (quick-sleep (* (current-keepalive) 2))))))))
       (quick-sleep (* (current-keepalive) 2)))))
  
  (test-case "sequential connections (connection reuse)"
    (let ([conns null])
      (for/list ([i (in-range 20)])
        (call-with-connection
         (lambda () (set! conns (cons (current-connection) conns)))))
      (call-with-connection
       (lambda () (check-not-false (member (current-connection) conns))))))
  
  (test-case "threads killed"
    (for/list ([i (in-range 3)])
      (thread
       (lambda ()
         (call-with-connection
          (lambda ()
            (kill-thread (current-thread)))))))
    (quick-sleep)
    (check-counts
     (list 0 0 10 10) 
     (lambda ()
       (quick-sleep (current-keepalive)))))
  
  (test-case "disconnections without connections"
    (for ([i (in-range 1 1000)])
      (send (current-snooze) disconnect)))

  (test-case "connections created in response to load"
      (check-counts
       (list 0 20 0 18)
       (lambda ()
         (apply sync
                (for/list ([i (in-range 40)])
                          (thread (lambda ()
                                    (send (current-snooze) call-with-connection
                                          (lambda () (sleep 1)))))))))))

(define (make-connection-pool-tests snooze)
  (if (is-a? (send snooze get-database) connection-pooled-database<%>)
      connection-pool-tests
      (test-suite "connection-pool-tests"
        (test-case "connection-pool-tests disabled"
          (fail "test database doesn't have a connection pool mixin (this failure is expected)")))))

; Helpers ----------------------------------------

; -> natural
(define (claimed-count)
  (get-field acquired-count (send (current-snooze) get-database)))

; -> natural
(define (unclaimed-count)
  (get-field available-count (send (current-snooze) get-database)))

; -> natural
(define (current-keepalive) 500)

; (listof numbers) -> (listof numbers)
(define (simplify numbers)
  (for/fold ([accum null])
            ([num (in-list numbers)])
            (if (or (null? accum) (not (= num (car accum))))
                (cons num accum)
                accum)))

; [natural] -> void
(define (quick-sleep [millis (quotient (current-keepalive) 2)])
  (sync (alarm-evt (+ (current-inexact-milliseconds) millis)))
  (void))

; Provides ---------------------------------------

(provide make-connection-pool-tests)
