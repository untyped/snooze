#lang scheme/base

(require "../base.ss")

(require "check-annotation.ss"
         "struct.ss")

; Primitive constructors -------------------------

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

; Accessors and functional update ----------------

; check-result -> (U exn #f)
(define (check-result-exn result)
  (and (check-fatal? result)
       (check-fatal-exn result)))

; Provide statements -----------------------------

; contract
(define annotations/c
  (and/c hash? hash-eq?))

(provide/contract
 [rename create-check-success make-check-success (->* ()             (string? #:annotations annotations/c) check-success?)]
 [rename create-check-warning make-check-warning (->* (string?)      (#:annotations annotations/c)         check-warning?)]
 [rename create-check-failure make-check-failure (->* (string?)      (#:annotations annotations/c)         check-failure?)]
 [rename create-check-fatal   make-check-fatal   (->* (string? exn?) (#:annotations annotations/c)         check-fatal?)]
 [check-result-exn                               (-> check-result? (or/c exn? #f))])
