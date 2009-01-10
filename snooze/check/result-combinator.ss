#lang scheme/base

(require scheme/contract
         scheme/match
         "check-combinator.ss"
         "result.ss")

; (listof check-result) ... -> boolean
(define (check-successes? . results)
  (ormap check-success? (apply check-all results)))

; (listof check-result) ... -> boolean
(define (check-problems? . results)
  (ormap check-problem? (apply check-all results)))

; (listof check-result) ... -> boolean
(define (check-warnings? . results)
  (ormap check-warning? (apply check-all results)))

; (listof check-result) ... -> boolean
(define (check-errors? . results)
  (ormap check-error? (apply check-all results)))

; (listof check-result) ... -> boolean
(define (check-failures? . results)
  (ormap check-failure? (apply check-all results)))

; (listof check-result) ... -> boolean
(define (check-fatals? . results)
  (ormap check-fatal? (apply check-all results)))

; Provide statements ---------------------------

(provide/contract
 [check-successes? (->* () () #:rest (listof (listof check-result?)) boolean?)]
 [check-problems?  (->* () () #:rest (listof (listof check-result?)) boolean?)]
 [check-warnings?  (->* () () #:rest (listof (listof check-result?)) boolean?)]
 [check-errors?    (->* () () #:rest (listof (listof check-result?)) boolean?)]
 [check-failures?  (->* () () #:rest (listof (listof check-result?)) boolean?)]
 [check-fatals?    (->* () () #:rest (listof (listof check-result?)) boolean?)])
