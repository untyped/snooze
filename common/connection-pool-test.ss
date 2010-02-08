#lang scheme/base

(require "../test-base.ss")

(require scheme/dict
         "../snooze-api.ss")

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
    [(list claimed-min
           claimed-max
           unclaimed-min
           unclaimed-max)
     (let-values ([(claimed-profile unclaimed-profile)
                   (sample-connections thunk)])
       (with-check-info (['claimed-profile   claimed-profile]
                         ['unclaimed-profile unclaimed-profile])
         (check-equal? (apply min claimed-profile) claimed-min)
         (check-equal? (apply max claimed-profile) claimed-max)
         (check-equal? (apply min unclaimed-profile) unclaimed-min)
         (check-equal? (apply max unclaimed-profile) unclaimed-max)))]))

; Tests ------------------------------------------

(define/provide-test-suite connection-pool-tests
  
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
    ; Mustn't have a connection already open when we run these tests:
    (check-counts (list 0 0 0 0) void))
  
  (test-case "one connection"
    (check-counts
     (list 0 1 0 1) 
     (lambda ()
       (call-with-connection
        (lambda ()
          (quick-sleep (* (current-keepalive) 2))))
       (quick-sleep (* (current-keepalive) 2)))))
  
  (test-case "parallel connections"
    (check-counts
     (list 0 5 0 5) 
     (lambda ()
       (apply
        sync
        (for/list ([i (in-range 5)])
          (thread (lambda ()
                    (call-with-connection
                     (lambda ()
                       (quick-sleep (* (current-keepalive) 2))))))))
       (quick-sleep (* (current-keepalive) 2)))))
  
  (test-case "sequential connections (connection reuse)"
    (let ([conn #f])
      (check-counts
       (list 0 1 0 1) 
       (lambda ()
         (for/list ([i (in-range 3)])
           (call-with-connection
            (lambda ()
              (if conn
                  (check-eq? conn (current-connection))
                  (set! conn (current-connection)))
              (quick-sleep (* (current-keepalive) 2)))))
         (quick-sleep (* (current-keepalive) 2))))))
  
  (test-case "threads killed"
    (for/list ([i (in-range 3)])
      (thread
       (lambda ()
         (call-with-connection
          (lambda ()
            (kill-thread (current-thread)))))))
    (quick-sleep)
    (check-counts
     (list 0 0 0 3) 
     (lambda ()
       (quick-sleep (current-keepalive))))))

; Helpers ----------------------------------------

; -> natural
(define (claimed-count)
  (dict-count (get-field claimed-connections (send (current-snooze) get-database))))

; -> natural
(define (unclaimed-count)
  (dict-count (get-field unclaimed-connections (send (current-snooze) get-database))))

; -> natural
(define (current-keepalive)
  (get-field keepalive-milliseconds (send (current-snooze) get-database)))

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
