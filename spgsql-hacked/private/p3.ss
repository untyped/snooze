;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module p3 mzscheme
  (require "p3-msg.ss")
  
  (provide protocol3:new
           protocol3:new-exchange
           protocol3:end-exchange
           protocol3:close
           protocol3:encode
           protocol3:flush)

  (provide stream:cons
           stream:current
           stream:next
           stream:current+next
           stream:done?
           stream->list)

  (define-struct protocol3 (inport outport stream ownerlock iolock))

  ;; protocol3:new : input-port output-port -> protocol
  (define (protocol3:new inport outport)
    (make-protocol3 inport outport #f (make-semaphore 1) (make-semaphore 1)))

  ;; protocol3:new-exchange : protocol3 -> stream
  (define (protocol3:new-exchange protocol)
    (semaphore-wait (protocol3-ownerlock protocol))
    (stream:force-to-end (protocol3-stream protocol))
    (let ([new-mg (stream:new protocol)])
      (set-protocol3-stream! protocol new-mg)
      new-mg))

  ;; protocol3:end-exchange : protocol3 -> void
  (define (protocol3:end-exchange protocol)
    (semaphore-post (protocol3-ownerlock protocol)))

  ;; protocol3:close : protocol3 -> void
  (define (protocol3:close protocol)
    (close-output-port (protocol3-outport protocol))
    ;; Don't lose messages from previous exchanges!
    (close-input-port (protocol3-inport protocol)))

  ;; protocol3:encode : protocol3 msg -> void
  (define (protocol3:encode protocol message)
    (write-message message (protocol3-outport protocol)))

  ;; protocol3:flush : protocol3 -> void
  (define (protocol3:flush protocol)
    (flush-output (protocol3-outport protocol)))

  ;; parse-message : protocol3 -> Response/eof
  (define (parse-message protocol)
    (parse-server-message (protocol3-inport protocol)))

  ;; STREAMS (Message Generators)

  (define-struct stream (p3 promise done?))

  (define (stream:current+next mg)
    (let ([iolock (protocol3-iolock (stream-p3 mg))])
      (dynamic-wind
          (lambda () (semaphore-wait iolock))
          (lambda () (stream:current+next/nosync mg))
          (lambda () (semaphore-post iolock)))))

  (define (stream:current+next/nosync mg)
    (force (stream-promise mg)))

  (define (stream:current mg)
    (let-values [((current next) (stream:current+next mg))]
      current))

  (define (stream:next mg)
    (let-values [((current next) (stream:current+next mg))]
      next))

  (define (stream:done? mg)
    (or (not mg)
        (stream-done? mg)))

  (define (stream:new protocol)
    (make-stream
     protocol
     (delay
       (let [(next-message (parse-message protocol))]
         (values next-message
                 (if (or (eof-object? next-message)
                         (end-of-exchange-message? next-message))
                     (make-stream protocol #f #t)
                     (stream:new protocol)))))
     #f))

  ;; Disabled for now:
  ;; Lazy message fetching seems to interact badly with fatal errors
  (define (stream:new/start-after protocol old-mg)
    (if (stream:done? old-mg)
        (stream:new protocol)
        (make-stream protocol
                     (delay
                       (let-values ([(_msg next-old-mg)
                                     (stream:current+next/nosync old-mg)])
                         (stream:current+next/nosync
                          (stream:new/start-after protocol next-old-mg))))
                     #f)))

  (define (stream:cons r mg)
    (make-stream
     (stream-p3 mg)
     (delay (values r mg))
     #f))

  (define (stream:force-to-end mg)
    (unless (stream:done? mg)
      (stream:force-to-end (stream:next mg))))
  
  (define (stream->list mg)
    (if (stream:done? mg)
        null
        (let-values ([(r mg) (stream:current+next mg)])
          (cons r (stream->list mg)))))

  (define (end-of-exchange-message? msg)
    (ReadyForQuery? msg))

  )
