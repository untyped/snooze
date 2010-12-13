#lang scheme/base

(require "../base.ss")

(require scheme/async-channel
         scheme/dict
         (planet untyped/unlib:3/match)
         (planet untyped/unlib:3/log)
         "interface.ss")

; Operations:
;
;   - connect
;     - Application thread: Block till a connection becomes free
;     - Manager thread: Mark the connection as claimed by the new thread;
;
;   - disconnect
;     - Manager thread: Marks the connection as unclaimed by a thread;
;     - Manager thread: Return the connection to the pool
;
;   - Claimed connection: thread death
;     - Manager thread: Marks relevant connection as unclaimed;

; Invariants
;
; Only the manager thread should:
;   - Place connections in the unclaimed-connections queue
;   - Update the claimed-count and unclaimed-count counters
;
; Only application threads should:
;   - Block retrieving connections from the unclaimed-connection queue
;
; If the manager thread attempts to retrieve connections it will deadlock itself.

; Helpers ----------------------------------------

(define-syntax-rule (add1! id)
  (set! id (add1 id)))

(define-syntax-rule (sub1! id)
  (set! id (sub1 id)))


; Set API ----------------------------------------
; We represent a set as a list

(define (set-member? set v)
  (if (member v set)
      #t
      #f))

(define (set-add set v)
  (if (set-member? set v)
      set
      (cons v set)))

(define (set-remove set v)
  (remove v set))


; Mixins -----------------------------------------

(define connection-pooled-database<%>
  (interface ()))

(define connection-pool-mixin
  (mixin (database<%>) (connection-pooled-database<%>)
    
    ; natural
    (init-field [max-connections 20])
    
    ; A channel to send requests from the application thread to the manager thread.
    ; Only one request can be processed at a time.
    ; 
    ; async-channel
    (field [tx-channel (make-async-channel 1)])
    

    (field [claimed-connections null])
    
    ; Connections that are not claimed by a thread.
    ; 
    ; (async-channel-of connection)
    (field [unclaimed-connections (make-async-channel max-connections)])
    
    ; The number of claimed connections
    ;
    ; natural
    (field [claimed-count 0])
    
    ; The number of unclaimed connections
    ;
    ; natural
    (field [unclaimed-count 0])
    
    (super-new)
    
    ; thread
    (field [manager-thread (thread (cut manage-connections))])
    
    ; Manager thread -----------------------------

    ; To avoid needlessly hogging connections, we wait until 
    ; the first call to connect before we initialize the connection pool.
    ;
    ; boolean
    (define initialized? #f)
    
    ; -> void
    (define/private (manage-connections)
      ; claimed-connections : (alistof connection thread-dead-evt)
      (let loop ([claimed-connections null])
        (match (sync tx-channel
                     (wrap-evt
                      (apply choice-evt (map cdr claimed-connections))
                      (lambda (evt)
                        (list 'unclaim evt))))
          
          [(list 'init rx-channel)
           ; Connect method is sent #t if the connection pool was initialized correctly, #f otherwise:
           (if initialized?
               (async-channel-put rx-channel #t)
               (let ([ans (with-handlers ([exn? (lambda (exn)
                                               ((error-display-handler) (exn-message exn) exn)
                                               #f)])
                         (for ([index (in-range max-connections)])
                              (async-channel-put unclaimed-connections (super connect))
                              (add1! unclaimed-count))
                         #t)])
                 (async-channel-put rx-channel ans)
                 (log-info* "Snooze connection pool initialised")
                 (set! initialized? #t)
                 (loop claimed-connections)))]
          
          [(list 'connect conn evt)
           (sub1! unclaimed-count)
           (add1! claimed-count)
           (log-info* "Snooze connection pool accepted connect" unclaimed-count claimed-count)
           (loop (dict-set claimed-connections conn evt))]
          
          [(list 'disconnect conn evt)
           (let ([evt (dict-ref claimed-connections conn #f)])
             (if evt
                 (begin
                   (async-channel-put unclaimed-connections conn)
                   (add1! unclaimed-count)
                   (sub1! claimed-count)
                   (log-info* "Snooze connection pool accepted disconnect" unclaimed-count claimed-count)
                   (loop (dict-remove claimed-connections conn)))
                 (begin
                   (log-info* "Snooze connection pool refused disconnect" unclaimed-count claimed-count)
                   (loop claimed-connections))))]
          
          [(list 'unclaim evt)
           (let ([conn (for/fold ([conn #f])
                           ([(the-conn the-evt) (in-dict claimed-connections)])
                         (if (equal? evt the-evt)
                             the-conn
                             conn))])
             (if conn
                 (begin
                   (async-channel-put unclaimed-connections conn)
                   (add1! unclaimed-count)
                   (sub1! claimed-count)
                   (log-info* "Snooze connection pool retrieved connection on thread death" unclaimed-count claimed-count)
                   (loop (dict-remove claimed-connections conn)))
                 (begin
                   (log-info* "Snooze connection pool read thread death event for thread we aren't monitoring" unclaimed-count claimed-count)
                   (loop claimed-connections))))])))

    ; Application thread -------------------------
        
    ; -> connection
    (define/override (connect)
      (unless initialized?
        (let ([rx-channel (make-async-channel)])
          (async-channel-put tx-channel (list 'init rx-channel))
          (unless (async-channel-get rx-channel)
            (raise-exn exn:fail:snooze "Could not initialise connection pool"))))
    
      ; The application thread must be the one that blocks waiting for a free connection:
      (let ([now (current-inexact-milliseconds)]
            [conn (async-channel-get unclaimed-connections)])
        (async-channel-put tx-channel (list 'connect conn (thread-dead-evt (current-thread))))
        (log-info* "PROFILE" "Connection pool connect" (- (current-inexact-milliseconds) now))
        conn))
    
    ; connection -> void
    (define/override (disconnect conn)
      (let ([now (current-inexact-milliseconds)]
            [evt (thread-dead-evt (current-thread))])
        (async-channel-put tx-channel (list 'disconnect conn evt))
        (log-info* "PROFILE" "Connection pool disconnect" (- (current-inexact-milliseconds) now))))))

; Provides ---------------------------------------

(provide connection-pooled-database<%>
         connection-pool-mixin)