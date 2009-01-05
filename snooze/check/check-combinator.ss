#lang scheme/base

(require scheme/contract
         scheme/match
         "../base.ss"
         "annotation.ss"
         "result.ss"
         (only-in "result-internal.ss" annotations/c))

; Atomic check constructors ----------------------

; [string] -> (list check-success)
(define (check-pass [message "Okay"])
  (list (make-check-success message)))

; string -> (list check-warning)
(define (check-warn message)
  (list (make-check-warning message)))

; string -> (list check-failure)
(define (check-fail message)
  (list (make-check-failure message)))

; Combinators ------------------------------------

; (listof check-result) ... -> (listof check-result)
(define check-all append)

; (listof check-result) ... -> (listof check-result)
(define (check-problems . args)
  (filter check-problem? (apply check-all args)))

; (listof check-result) ... -> (listof check-result)
(define (check-errors . args)
  (filter check-error? (apply check-all args)))

;  (listof check-result)
;  ...
; ->
;  (listof check-warning)
;  (listof check-failure)
;  (listof check-fatal)
(define (check-warnings+failures+fatals . args)
  (let loop ([results (apply check-all args)] [warnings null] [failures null] [fatals null])
    (match results
      [(list)
       (values (reverse warnings)
               (reverse failures)
               (reverse fatals))]
      [(list-rest (? check-result? result) other)
       (cond [(check-success? result) (loop other warnings failures fatals)]
             [(check-warning? result) (loop other (cons result warnings) failures fatals)]
             [(check-failure? result) (loop other warnings (cons result failures) fatals)]
             [(check-fatal?   result) (loop other warnings failures (cons result fatals))])])))

; (-> (listof check-result)) -> (listof check-result)
(define (check-with-handlers thunk)
  (with-handlers ([exn? (lambda (exn)
                          (list (make-check-fatal "Exception raised" exn)))])
    (thunk)))

; (listof annotation any) (-> (listof check-result)) -> (listof check-result)
(define (check-with-annotations annotations+values thunk)
  (map (lambda (result)
         (foldl (match-lambda*
                  [(list (list-rest ann val) result)
                   (check-result-annotation-set result ann val)])
                result
                annotations+values))
       (check-with-handlers thunk)))

; (-> (listof check-result)) ... -> (listof check-results)
(define check-until-problems
  (match-lambda*
    [(list) null]
    [(list-rest head tail)
     ; (listof check-result)
     (define results
       (head))
     ; boolean
     (define problems? 
       (and (ormap check-problem? results) #t))
     ; (listof check-result)
     (if problems?
         results
         (apply check-until-problems tail))]))

; Provide statements ---------------------------

(provide/contract
 [check-pass                     (->* () (string?) (list/c check-success?))]
 [check-warn                     (-> string?       (list/c check-warning?))]
 [check-fail                     (-> string?       (list/c check-failure?))]
 [check-all                      (->* () () #:rest (listof (listof check-result?)) (listof check-result?))]
 [check-problems                 (->* () () #:rest (listof (listof check-result?)) (listof check-result?))]
 [check-errors                   (->* () () #:rest (listof (listof check-result?)) (listof check-result?))]
 [check-warnings+failures+fatals   (->* () () #:rest (listof (listof check-result?))
                                        (values (listof check-warning?)
                                                (listof check-failure?)
                                                (listof check-fatal?)))]
 [check-with-handlers            (-> (-> (listof check-result?)) (listof check-result?))]
 [check-with-annotations         (-> (listof (cons/c annotation? any/c)) (-> (listof check-result?)) (listof check-result?))]
 [check-until-problems           (->* () () #:rest (listof procedure?) (listof check-problem?))])
