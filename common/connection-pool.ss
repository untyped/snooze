#lang scheme/base

(require "../base.ss")

(require scheme/async-channel
         scheme/dict
         (planet untyped/unlib:3/match)
         (planet untyped/unlib:3/log)
         "interface.ss")

; Terminology:
;
; A connection in use by an application thread is *acquired*
; A connection returned by an applicaiton thread is *released*
; Connections that are not in use by an application thread are *available*
; A connection that is permanently removed from the pool (i.e. disconnected) is *freed*

; Operations:
;
;   - connect
;     - Application thread: Block till a connection becomes available
;     - Manager thread: Mark the connection as acquired by the new thread;
;
;   - disconnect
;     - Manager thread: Marks the connection as released by a thread;
;     - Manager thread: Return the connection to the pool
;
;   - Acquired connection: thread death
;     - Manager thread: Marks relevant connection as released;

; Invariants:
;
; Only the manager thread should:
;   - Place connections in the pool
;   - Update the acquired-count and available-count counters
;
; Only application threads should:
;   - Block retrieving connections from the pool
;
; If the manager thread blocks attempting to retrieve connections it will deadlock itself.

; Helpers ----------------------------------------

(define-syntax-rule (add1! id)
  (set! id (add1 id)))

(define-syntax-rule (sub1! id)
  (set! id (sub1 id)))

;; Holds information related to an acquired connection
;;  id         : Symbol  A symbol that uniqely identifies this acquisition of a connection
;;  dead-evt   : Event   The thread-dead-evt of the thread that acquired the connection
;;  start-time : Real    The time, in ms, that the thread acquired the connection
(define-struct acquisition (id dead-evt start-time))


; Mixins -----------------------------------------

(define connection-pooled-database<%>
  (interface ()))

(define connection-pool-mixin
  (mixin (database<%>) (connection-pooled-database<%>)
    
    (inherit reset-connection)
    
    ; natural
    (init-field min-connections)
    (init-field max-connections)
    
    ; A channel to send requests from the application thread to the manager thread.
    ; Only one request can be processed at a time.
    ; 
    ; async-channel
    (field [manager-channel (make-async-channel 1)])
    
    ; Connections that are not in use by an application thread.
    ; 
    ; (async-channel-of connection)
    (field [available-connections (make-async-channel max-connections)])
    
    ; The number of acquired connections
    ;
    ; natural
    (field [acquired-count 0])
    
    ; The number of available connections
    ;
    ; natural
    (field [available-count 0])
    
    (super-new)
    
    ; thread
    (field [manager-thread (thread (cut manage-connections))])
    
    ; Manager thread -----------------------------
    
    ; To avoid needlessly hogging connections, we wait until 
    ; the first call to connect before we initialize the connection pool.
    ;
    ; boolean
    (define initialized? #f)
    
    ; Method that performs the following steps in a continuous loop:
    ; 1. Resizes the connection pool if required (using PE goodness);
    ; 2. Responds to a message from the application or a thread death.
    ; -> void
    (define/private (manage-connections)
      (define gain 0.5)
      ; Accepts number of connections that the manager wishes to create/free.
      ; Returns that number, bounded by min- and max-connections.
      ; integer -> integer
      (define (clip x)
        (let* ([current (+ available-count acquired-count)]
               [goal    (+ x current)])
          (cond
            ;; (- min-connections current) gives a -ve number
            ;; which is consistent with the value of x when we
            ;; are freeing connections
            [(< goal min-connections) (- min-connections current)]
            [(< max-connections goal) (- max-connections current)]
            [else x])))
      (define (create-connection)
        (async-channel-put available-connections (super connect))
        (add1! available-count))
      (define (free-connection)
        (let ([conn (async-channel-try-get available-connections)])
          (if conn
              (begin (super disconnect conn) (sub1! available-count))
              (log-debug* "Snooze connection pool attempted to free connection but none were available"
                          available-count acquired-count))))
      (define (initialise)
        (with-handlers
            ([exn? (lambda (exn)
                     (log-error*
                      "Snooze connection pool manager caused exception on initialisation" exn)
                     (raise exn))])
          (unless initialized?
            (for ([index (in-range min-connections)])
              (create-connection))
            (set! initialized? #t)
            (log-debug* "Snooze connection pool initialised"))))
      (define (free-connections n)
        (for ([i (in-range n)])
          (free-connection)))
      (define (create-connections n)
        (for ([i (in-range n)])
          (create-connection)))
      
      ; claimed-connections : (alistof connection acquisition)
      ; waiting             : Natural  The number of application threads waiting for a connection
      (let loop ([acquired-connections null]
                 [waiting              0])
        
        ;; Make a decision on allocating or freeing
        ;; connections based on the current demand for
        ;; connections.
        ;;
        ;; This is a proportional error (PE)
        ;; controller. We calculate the demand for
        ;; connections (connections already acquired and
        ;; number of application threads waiting for a
        ;; connection) and compare it to the number of
        ;; connections available. The difference gives us
        ;; our error term.
        (let* ([demand (+ waiting acquired-count)]
               [total  (+ available-count acquired-count)]
               [error  (- demand total)]
               [change (clip (round (* error gain)))])
          (log-debug* "Snooze connection pool decision variables"
                      demand total waiting available-count acquired-count)
          (cond
            [(zero? change)
             ; Do nothing:
             #t]
            [(< change 0)
             ;; change is -ve, so we negate to get the number we should free
             (let ([n-to-free (- change)])
               (log-debug* "Snooze connection pool decided to free connections" n-to-free)
               (free-connections n-to-free))]
            [(> change 0)
             (let ([n-to-create change])
               (log-debug* "Snooze connection pool decided to create connections" n-to-create)
               (create-connections n-to-create))]))
        
        (match (sync manager-channel
                     (wrap-evt
                      (apply choice-evt (dict-map acquired-connections
                                                  (lambda (conn acq)
                                                    (acquisition-dead-evt acq))))
                      (lambda (evt)
                        (list 'unclaim evt))))
          
          [(list 'connect time thd)
           (unless initialized?
             (initialise))
           ;; Make an unique id for this request
           (let ([id (gensym)])
             (with-handlers ([exn? (lambda (exn) (loop acquired-connections waiting))])
               (thread-send thd id)
               (log-debug* "Snooze connection pool connection attempt started"
                           id time (add1 waiting) available-count acquired-count))
             ; This is outside the with-handlers block (for tail-recursive non-stack-exploding goodness):
             (loop acquired-connections (add1 waiting)))]
          
          [(list 'connected time thd id conn)
           (sub1! available-count)
           (add1! acquired-count)
           (log-debug* "Snooze connection pool connection attempt succeeded"
                       id time (sub1 waiting) available-count acquired-count)
           (loop (dict-set acquired-connections conn
                           (make-acquisition id (thread-dead-evt thd) time))
                 (sub1 waiting))]
          
          [(list 'disconnect conn time)
           (let ([acq (dict-ref acquired-connections conn #f)])
             (if acq
                 (begin
                   (async-channel-put available-connections conn)
                   (add1! available-count)
                   (sub1! acquired-count)
                   (log-debug* "Snooze connection pool accepted disconnect"
                               (acquisition-id acq) (current-inexact-milliseconds)
                               waiting available-count acquired-count)
                   (loop (dict-remove acquired-connections conn)
                         waiting))
                 (begin
                   (log-warning* "Snooze connection pool refused disconnect"
                                 waiting available-count acquired-count)
                   (loop acquired-connections
                         waiting))))]
          
          [(list 'unclaim evt)
           (let-values ([(conn acq)
                         (for/fold ([conn #f] [acq #f])
                                   ([(the-conn the-acq) (in-dict acquired-connections)])
                                   (if (equal? evt (acquisition-dead-evt the-acq))
                                       (values the-conn the-acq)
                                       (values conn acq)))])
             (if conn
                 (begin
                   (reset-connection conn)
                   (async-channel-put available-connections conn)
                   (add1! available-count)
                   (sub1! acquired-count)
                   (log-debug* "Snooze connection pool retrieved connection on thread death"
                               (acquisition-id acq) (current-inexact-milliseconds)
                               waiting available-count acquired-count)
                   (loop (dict-remove acquired-connections conn)
                         waiting))
                 (begin
                   (log-warning* "Snooze connection pool had thread death event for thread we aren't monitoring"
                                 waiting available-count acquired-count)
                   (loop acquired-connections
                         waiting))))])))
    
    ; Application thread -------------------------
    
    ; -> connection
    (define/override (connect)
      ; The application thread must be the one that blocks waiting for a free connection:
      (let* ([me    (current-thread)]
             [start (current-inexact-milliseconds)]
             [id    (begin
                      (async-channel-put
                       manager-channel
                       (list 'connect start me))
                      (thread-receive))]
             [conn  (async-channel-get available-connections)]
             [stop  (current-inexact-milliseconds)])
        (async-channel-put
         manager-channel
         (list 'connected stop me id conn))
        (log-info* "PROFILE" "Connection pool connect" (- stop start))
        conn))
    
    ; connection -> void
    (define/override (disconnect conn)
      (let ([now (current-inexact-milliseconds)])
        (async-channel-put manager-channel (list 'disconnect conn now))
        (log-info* "PROFILE" "Connection pool disconnect" (- (current-inexact-milliseconds) now))))))

; Provides ---------------------------------------

(provide connection-pooled-database<%>
         connection-pool-mixin)