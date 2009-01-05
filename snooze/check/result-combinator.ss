#lang scheme/base

(require scheme/contract
         scheme/match
         "result.ss")

; (listof check-result) -> boolean
(define (check-problems? results)
  (ormap check-problem? results))

; (listof check-result) -> boolean
(define (check-errors? results)
  (ormap check-error? results))

; (listof check-result) -> boolean
(define (check-fatals? results)
  (ormap check-fatal? results))

; Provide statements ---------------------------

(provide/contract
 [check-problems? (-> (listof check-result?) boolean?)]
 [check-errors?   (-> (listof check-result?) boolean?)]
 [check-fatals?   (-> (listof check-result?) boolean?)])
