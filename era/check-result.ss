#lang scheme/base

(require "../base.ss")

(require "check-annotation.ss"
         "core.ss")

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

; check-result annotation -> any
(define (check-result-annotation result annote)
  (hash-ref (check-result-annotations result)
            annote
            (cut annotation-default annote result)))

; check-result annotation -> boolean
(define (check-result-has-annotation? result annote)
  (with-handlers ([exn? (lambda _ #f)])
    (hash-ref (check-result-annotations result) annote)
    #t))

; check-result annotation any -> check-result
(define (check-result-annotation-set result annote val)
  (let* ([message (check-result-message result)]
         [old     (check-result-annotation result annote)]
         [new     (annotation-compose annote result old val)]
         [annotes (hash-set (check-result-annotations result) annote new)])
    ; check-result
    (cond [(check-success? result) (make-check-success message annotes)]
          [(check-warning? result) (make-check-warning message annotes)]
          [(check-failure? result) (make-check-failure message annotes)]
          [(check-fatal? result)   (make-check-fatal   message annotes (check-fatal-exn result))])))

; Provide statements -----------------------------

; contract
(define annotations/c
  (and/c hash? hash-eq?))

(provide/contract
 [rename create-check-success make-check-success (->* ()             (string? #:annotations annotations/c) check-success?)]
 [rename create-check-warning make-check-warning (->* (string?)      (#:annotations annotations/c)         check-warning?)]
 [rename create-check-failure make-check-failure (->* (string?)      (#:annotations annotations/c)         check-failure?)]
 [rename create-check-fatal   make-check-fatal   (->* (string? exn?) (#:annotations annotations/c)         check-fatal?)]
 [check-result-exn                               (-> check-result? (or/c exn? false/c))]
 [check-result-annotation                        (-> check-result? annotation? any)]
 [check-result-has-annotation?                   (-> check-result? annotation? boolean?)]
 [check-result-annotation-set                    (-> check-result? annotation? any/c check-result?)])