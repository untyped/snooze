#lang scheme/base

(require "../test-base.ss")

(require scheme/class
         scheme/dict
         scheme/match
         "../snooze.ss")

; Tests ------------------------------------------

(define (make-connection-pool-tests snooze)
  
  ; -> natural
  (define (claimed-count)
    (dict-count (get-field claimed-connections (send snooze get-database))))
  
  ; -> natural
  (define (unclaimed-count)
    (dict-count (get-field unclaimed-connections (send snooze get-database))))
  
  ; -> natural
  (define (current-keepalive)
    (get-field keepalive-milliseconds (send snooze get-database)))
  
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
  
  ; (-> any) -> (listof natural) (listof natural)
  (define (sample-connections thunk)
    (let ([accum1  null]
          [accum2  null]
          [finish? #f])
      (thread (lambda ()
                (let loop ()
                  (unless finish?
                    (set! accum1 (cons (claimed-count)   accum1))
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
           (check-equal? (apply min claimed-profile)   claimed-min)
           (check-equal? (apply max claimed-profile)   claimed-max)
           (check-equal? (apply min unclaimed-profile) unclaimed-min)
           (check-equal? (apply max unclaimed-profile) unclaimed-max)))]))
  
  (test-suite "connection-pool.ss"
    
    #:before
    (lambda ()
      (send snooze disconnect)
      (quick-sleep (current-keepalive)))
    
    #:after
    (lambda ()
      (send snooze connect))
    
    (test-case "sanity check"
      ; Mustn't have a connection already open when we run these tests:
      (check-counts (list 0 0 0 0) void))
    
    (test-case "one connection"
      (check-counts
       (list 0 1 0 1) 
       (lambda ()
         (send snooze
               call-with-connection
               (lambda ()
                 (quick-sleep (* (current-keepalive) 2)))
               #f)
         (quick-sleep (* (current-keepalive) 2)))))
    
    (test-case "parallel connections"
      (check-counts
       (list 0 5 0 5) 
       (lambda ()
         (apply
          sync
          (for/list ([i (in-range 5)])
            (thread (lambda ()
                      (send snooze
                            call-with-connection
                            (lambda ()
                              (quick-sleep (* (current-keepalive) 2)))
                            #f)))))
         (quick-sleep (* (current-keepalive) 2)))))
    
    (test-case "sequential connections (connection reuse)"
      (let ([conn #f])
        (check-counts
         (list 0 1 0 1) 
         (lambda ()
           (for/list ([i (in-range 3)])
             (send snooze
                   call-with-connection
                   (lambda ()
                     (if conn
                         (check-eq? conn (send snooze current-connection))
                         (set! conn (send snooze current-connection)))
                     (quick-sleep (* (current-keepalive) 2)))
                   #f))
           (quick-sleep (* (current-keepalive) 2))))))
    
    (test-case "threads killed"
      (for/list ([i (in-range 3)])
        (thread
         (lambda ()
           (send snooze
                 call-with-connection
                 (lambda ()
                   (kill-thread (current-thread)))
                 #f))))
      (quick-sleep)
      (check-counts
       (list 0 0 0 3) 
       (lambda ()
         (quick-sleep (current-keepalive)))))))

; Provides ---------------------------------------

(provide make-connection-pool-tests)
