#lang scheme

(require (planet untyped/unlib:3/log)
         "snooze.ss"
         "postgresql8/postgresql8.ss")

; Database connection ----------------------------

(define (test-postgresql8-database
         #:server   [server   "localhost"]
         #:port     [port     5432]
         #:database [database "snoozetest"]
         #:username [username "snooze"]
         #:password [password #f])
  (make-postgresql8-database
   #:server   server
   #:port     port
   #:database database
   #:username username
   #:password password
   #:pool-connections? #t))

(current-snooze (make-snooze (test-postgresql8-database)))

; Test data --------------------------------------

(define job-size 20)

(define-entity test
  ([str string]
   [num integer]))

(define next-id
  (let ([id 0])
    (lambda ()
      (begin0 id (set! id (add1 id))))))

(define-struct worker (id starter thread state)
  #:mutable
  #:transparent
  #:property prop:custom-write
  (lambda (self out write?)
    ((if write? write display)
     (vector 'worker (worker-id self) (worker-state self))
     out)))

(define (start-worker starter)
  (restart-worker (make-worker (next-id) starter #f #f)))

(define (restart-worker worker [kill? #f])
  (if (and (worker-thread worker) (thread-running? (worker-thread worker)))
      (if kill?
          (begin
            (kill-thread (worker-thread worker))
            (worker-log worker 'killed)
            (restart-worker worker))
          worker)
      (begin
        (set-worker-state! worker #f)
        (worker-log worker 'about-to-start)
        (set-worker-thread! worker (thread (lambda () ((worker-starter worker) worker))))
        worker)))

(define (worker-log worker state)
  (set-worker-state! worker state)
  (log-info* (worker-id worker)
             (worker-state worker)))

(define (start-save-worker)
  (start-worker
   (lambda (self)
     (worker-log self 'started)
     (with-connection
      (worker-log self 'acquired)
      (with-transaction #:metadata "save-worker"
        (for ([i (in-range 0 job-size)])
          ;(worker-log self i)
          (save! (make-test (format "test~a" i) i))))
      (worker-log self 'finished))
     (worker-log self 'released))))

(define (start-delete-worker)
  (start-worker
   (lambda (self)
     (worker-log self 'started)
     (with-connection
      (let ([num (random job-size)])
        (worker-log self 'acquired)
        (with-transaction #:metadata (format "Delete ~a" num)
          (for-each delete! (find-tests #:num num)))
        (worker-log self 'finished))
      (worker-log self 'released)))))

(define (start-select-worker)
  (start-worker
   (lambda (self)
     (worker-log self 'started)
     (with-connection
      (worker-log self 'acquired)
      (let ([num (random job-size)])
        (let-alias ([a test]
                    [b test])
                   (select-all #:what (a b)
                               #:from (outer a b)
                               #:where (or (= a.num ,num)
                                           (= b.num ,num))))
        (worker-log self 'finished))
      (worker-log self 'released)))))

(define (start-any-worker)
  (match (random 3)
    [0 (start-save-worker)]
    [1 (start-delete-worker)]
    [2 (start-select-worker)]))

(define (worker-counts workers)
  (let ([counts (make-hasheq)])
    (for ([worker (in-vector workers)])
      (let* ([state (worker-state worker)]
             [count (dict-ref counts state 0)])
        (dict-set! counts state (add1 count))))
    counts))

; Threads ----------------------------------------

; Sleeps for the specified number of milliseconds.
;
; integer -> void
(define (sleep/ms ms)
  (sync (alarm-evt (+ (current-inexact-milliseconds) ms))))

(start-log-output 'info)

(with-connection
 (drop-table test)
 (create-table test))

(let* ([num-workers 10]
       [workers (apply vector (for/list ([i (in-range 0 num-workers)])
                                (start-any-worker)))])
  (for ([i (in-naturals)])
    (let ([j (random num-workers)])
      (log-info* "counts" (worker-counts workers))
      (sleep/ms 50)
      (restart-worker (vector-ref workers j)))))
