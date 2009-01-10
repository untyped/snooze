#lang scheme/base

(require scheme/contract
         "../base.ss"
         "annotation.ss"
         "result-internal.ss")

; Constructors -----------------------------------

; [string] -> (list check-success)
(define (create-check-success [message "Okay"] #:annotations [annotations #hasheq()])
  (make-check-success message annotations))

; string -> (list check-warning)
(define (create-check-warning message #:annotations [annotations #hasheq()])
  (make-check-warning message annotations))

; string -> (list check-failure)
(define (create-check-failure message #:annotations [annotations #hasheq()])
  (make-check-failure message annotations))

; string exn -> check-fatal
(define (create-check-fatal message exn #:annotations [annotations #hasheq()])
  (make-check-fatal message annotations exn))

; Accessors and mutators -----------------------

; check-result -> (U exn #f)
(define (check-result-exn result)
  (if (check-fatal? result)
      (check-fatal-exn result)
      #f))

; Provide statements ---------------------------

(provide (except-out (all-from-out "result-internal.ss")
                     make-check-success
                     make-check-warning
                     make-check-failure
                     make-check-fatal))

(provide/contract
 [check-result-exn                               (-> check-result? (or/c exn? false/c))]
 [rename create-check-success make-check-success (->* ()             (string? #:annotations annotations/c) check-success?)]
 [rename create-check-warning make-check-warning (->* (string?)      (#:annotations annotations/c)         check-warning?)]
 [rename create-check-failure make-check-failure (->* (string?)      (#:annotations annotations/c)         check-failure?)]
 [rename create-check-fatal     make-check-fatal     (->* (string? exn?) (#:annotations annotations/c)         check-fatal?)])
