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

; Send a value to tx-channel and wait for a response on a newly created rx-channel.
; (_ async-channel any) -> any
(define-syntax-rule (async-send tx-channel msg arg ...)
  (let ([rx-channel (make-async-channel)])
    (async-channel-put tx-channel (list msg rx-channel arg ...))
    (async-channel-get rx-channel)))

; Mixins -----------------------------------------

(define connection-pooled-database<%>
  (interface ()))

(define connection-pool-mixin
  (mixin (database<%>) (connection-pooled-database<%>)
    
    ; natural
    (init-field [max-connections 100])
    
    ; natural
    (init-field [keepalive-milliseconds 5000])
    
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
    ; The connections are "hashed" against alarm-evts that will trigger when they
    ; are to be cleaned up.
    ; 
    ; (alistof alarm-evt connection)
    (field [unclaimed-connections null])
    
    (super-new)
    
    ; thread
    (field [manager-thread (thread (cut manage-connections))])
    
    ; Manager thread -----------------------------
    
    ; -> void
    (define/private (manage-connections)
      (let loop ()
        
        (match (sync tx-channel
                     (wrap-evt (apply choice-evt (map car claimed-connections))
                               (lambda (evt)
                                 (list 'unclaim evt)))
                     (wrap-evt (apply choice-evt (map car unclaimed-connections))
                               (lambda (evt)
                                 (list 'release evt))))
          
          ; Application connect request:
          [(list 'connect rx-channel thread)
           (async-channel-put
            rx-channel
            (with-handlers ([exn? (lambda (exn) exn)])
              (new-connection! (thread-dead-evt thread))))]
          
          ; Application disconnect request:
          [(list 'disconnect rx-channel thread conn)
           (async-channel-put
            rx-channel
            (with-handlers ([exn? (lambda (exn) exn)])
              (unclaim-connection! (thread-dead-evt thread))))]
          
          ; Thread death:
          [(list 'unclaim evt)
           (unclaim-connection! evt)]
          
          ; Unclaimed connection timeout:
          [(list 'release evt)
           (release-connection! evt)])
        
        (loop)))
    
    ; thread-dead-evt -> connection
    (define (new-connection! evt)
      
      ; -> (U connection #f)
      (define (reclaim-connection!)
        (let* ([pos  (dict-iterate-first unclaimed-connections)]
               [evt  (and pos (dict-iterate-key   unclaimed-connections pos))]
               [conn (and pos (dict-iterate-value unclaimed-connections pos))])
          (and pos
               (set! unclaimed-connections (dict-remove unclaimed-connections evt))
               conn)))
      
      (let ([conn (or (reclaim-connection!)
                      (super connect))])
        (set! claimed-connections (dict-set claimed-connections evt conn))
        conn))
    
    ; thead-dead-evt -> boolean
    (define (claimed-connection? evt)
      (and (dict-ref claimed-connections evt #f) #t))
    
    ; thread-dead-evt -> void
    (define (unclaim-connection! evt)
      (let ([conn (dict-ref claimed-connections evt)])
        (set! claimed-connections   (dict-remove claimed-connections evt))
        (set! unclaimed-connections (dict-set unclaimed-connections (release-evt) conn))
        (void)))
    
    ; alarm-evt -> void
    (define (release-connection! evt)
      (let ([conn (dict-ref unclaimed-connections evt)])
        (set! unclaimed-connections (dict-remove unclaimed-connections evt))
        (super disconnect conn)))
    
    ; -> alarm-evt
    (define (release-evt)
      (alarm-evt (+ (current-inexact-milliseconds)
                    keepalive-milliseconds)))
        
    ; Application thread -------------------------
    
    ; -> connection
    (define/override (connect)
      (let ([ans (async-send tx-channel 'connect (current-thread))])
        (if (exn? ans)
            (raise ans)
            ans)))
    
    ; connection -> void
    (define/override (disconnect conn)
      (when (claimed-connection? (thread-dead-evt (current-thread)))
        (let ([ans (async-send tx-channel 'disconnect (current-thread) conn)])
          (if (exn? ans)
              (raise ans)
              ans))))))

; Provides ---------------------------------------

(provide connection-pooled-database<%>
         connection-pool-mixin)