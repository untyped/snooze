#lang scheme/base

(require "../base.ss")

(require scheme/async-channel
         scheme/dict
         (planet untyped/unlib:3/match)
         "interface.ss")

; Operations:
;
;   - connect - manager:
;     - looks for an unclaimed connection and recycles it if possible;
;     - otherwise, opens a new connection;
;     - marks the connection as claimed by the new thread;
;
;   - disconnect
;     - marks the connection as unclaimed by a thread;
;     - starts a death timer (N seconds);
;
;   - claimed connection: thread death
;     - marks relevant connection as unclaimed;
;
;   - unclaimed connection: release alarm;
;     - closes connection.

; Helpers ----------------------------------------

(define-syntax-rule (add1! id)
  (set! id (add1 id)))

(define-syntax-rule (sub1! id)
  (set! id (sub1 id)))

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
    
    ; Connections that are claimed by a thread.
    ; The connections are "hashed" against thread-dead events for their threads.
    ; This (a) doesn't prevent the threads getting GC'd and (b) provides an easy
    ; way to respond to the deaths of the threads.
    ;
    ; (alistof thread-dead-evt connection)
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
    
    ; -> void
    (define/private (manage-connections)
      
      ; boolean
      (define initialized? #f)
      
      ; -> void
      (define (initialize!)
        (for ([index (in-range max-connections)])
          (async-channel-put unclaimed-connections (super connect))
          (add1! unclaimed-count)))
      
      (let loop ([claimed-connections null])
        (match (sync tx-channel
                     (wrap-evt
                      (apply choice-evt (map car claimed-connections))
                      (lambda (evt)
                        (list 'unclaim evt))))
          
          [(list 'connect evt conn)
           (unless initialized? (initialize!))
           (sub1! unclaimed-count)
           (add1! claimed-count)
           (loop (dict-set claimed-connections evt conn))]
          
          [(list 'disconnect evt conn)
           (let ([conn (dict-ref claimed-connections evt #f)])
             (if conn
                 (begin
                   (async-channel-put unclaimed-connections conn)
                   (add1! unclaimed-count)
                   (sub1! claimed-count)
                   (loop (dict-remove claimed-connections evt)))
                 (loop claimed-connections)))]
          
          [(list 'unclaim evt)
           (let ([conn (dict-ref claimed-connections evt)])
             (async-channel-put unclaimed-connections conn)
             (add1! unclaimed-count)
             (sub1! claimed-count)
             (loop (dict-remove claimed-connections evt)))])))
    
    
    ; thread-dead-evt -> void
    (define (unclaim-connection! evt)
      (let ([conn (dict-ref claimed-connections evt)])
        (set! claimed-connections (dict-remove claimed-connections evt))
        (async-channel-put unclaimed-connections conn)
        (set! unclaimed-count (add1 unclaimed-count))
        (void)))
    
    ; Application thread -------------------------
    
    ; -> connection
    (define/override (connect)
      ; The application thread must be the one that blocks waiting for a free connection.
      (let ([conn (async-channel-get unclaimed-connections)])
        (async-channel-put tx-channel (list 'connect (thread-dead-evt (current-thread)) conn))
        conn))
    
    ; connection -> void
    (define/override (disconnect conn)
      (let ([evt (thread-dead-evt (current-thread))])
        (async-channel-put tx-channel (list 'disconnect evt conn))))))

; Provides ---------------------------------------

(provide connection-pooled-database<%>
         connection-pool-mixin)