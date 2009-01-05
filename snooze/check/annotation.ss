#lang scheme/base

(require "../annotation.ss"
         "../base.ss"
         "result-internal.ss")

; Procedures -------------------------------------

; check-result annotation -> any
(define (check-result-annotation result annote)
  (hash-ref (check-result-annotations result)
            annote
            (cut (annotation-default annote) result)))

; check-result annotation -> boolean
(define (check-result-has-annotation? result annote)
  (with-handlers ([exn? (lambda _ #f)])
    (hash-ref (check-result-annotations result) annote)
    #t))

; check-result annotation any -> check-result
(define (check-result-annotation-set result annote val)
  ; string
  (define message
    (check-result-message result))
  ; (hasheqof annotation any)
  (define annotations
    (hash-set (check-result-annotations result) annote
              ((annotation-combinator annote)
               result (check-result-annotation result annote) val)))
  ; check-result
  (cond [(check-success? result) (make-check-success message annotations)]
        [(check-warning? result) (make-check-warning message annotations)]
        [(check-failure? result) (make-check-failure message annotations)]
        [(check-fatal? result)   (make-check-fatal   message annotations (check-fatal-exn result))]))

; Provide statements -----------------------------

(provide (all-from-out "../annotation.ss"))

(provide/contract
 [check-result-annotation      (-> check-result? annotation? any)]
 [check-result-has-annotation? (-> check-result? annotation? boolean?)]
 [check-result-annotation-set  (-> check-result? annotation? any/c check-result?)])