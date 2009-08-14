#lang scheme/base

(require scheme/async-channel)

;  ((any -> any) -> thunk)
;  ((any -> any) -> thunk)
;  ((any -> any) -> thunk) ...
; ->
;  void
;
; Accepts N>=2 arguments, where each argument is a procedure that:
;   - accepts an argument called "transfer";
;   - runs for a while;
;   - calls "transfer" to pass control to the next procedure.
;
; Interleave runs each procedure in a separate thread, but uses
; async channels to make sure only one thread is running. The first
; argument is the first to be run.
;
; The transfer arguments can be called with 0 or 1 arguments:
;
;   - 0 arguments: passes control to the next procedure argument
;     (the last procedure passes control back to the first);
;   - 1 argument: the argument is the index of the procedure that
;     is to be invoked next.
(define (interleave first-body second-body . other-bodies)
  
  (define bodies
    (list* first-body second-body other-bodies))
  
  (define channels
    (for/list ([body     (in-list bodies)])
      (make-async-channel)))
  
  (define transfer-procs
    (for/list ([body          (in-list bodies)]
               [my-index      (in-naturals)]
               [my-channel    (in-list channels)])
      (let ([next-index (remainder (add1 my-index) (length channels))])
        (lambda ([to-index next-index])
          (async-channel-put (list-ref channels to-index) (void))
          (async-channel-get my-channel)))))
  
  (define wrapped-procs
    (for/list ([body          (in-list bodies)]
               [my-channel    (in-list channels)]
               [transfer-proc (in-list transfer-procs)])
      (lambda ()
        (async-channel-get my-channel)
        (body transfer-proc))))
  
  (define threads
    (map thread wrapped-procs))
  
  (async-channel-put (car channels) (void))
  
  (apply sync threads)
  (for/list ([channel (in-list channels)])
    (async-channel-put channel 'done))
  
  (void))

(provide interleave)