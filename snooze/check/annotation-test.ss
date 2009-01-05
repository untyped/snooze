#lang scheme/base

(require "../test-base.ss"
         "annotation.ss"
         "check-combinator.ss"
         "check-combinator-syntax.ss"
         "result.ss")

; annotation
(define-annotation ann:num 
  (lambda (result) 0)
  (lambda (result old new) (+ old new)))

; annotation
(define-annotation ann:str
  (lambda (result) "")
  (lambda (result old new) (string-append old new)))

; Tests ------------------------------------------

(define annotation-tests
  (test-suite "annotation.ss"
    
    (test-case "check-result-annotation"
      (let ([result (car (check/annotate ([ann:num 123]) (check-pass)))])
        (check-equal? (check-result-annotation result ann:num) 123)
        (check-equal? (check-result-annotation result ann:str) "")))
    
    (test-case "check-result-has-annotation?"
      (let ([result (car (check/annotate ([ann:num 123]) (check-pass)))])
        (check-true (check-result-has-annotation? result ann:num))
        (check-false (check-result-has-annotation? result ann:str))))
    
    (test-case "check-result-annotation-set"
      (let* ([result0 (make-check-success)]
             [result1 (check-result-annotation-set result0 ann:num 1)]
             [result2 (check-result-annotation-set result1 ann:num 2)]
             [result3 (check-result-annotation-set result2 ann:num 3)])
        (check-equal? (check-result-annotation result1 ann:num) 1)
        (check-equal? (check-result-annotation result2 ann:num) 3)
        (check-equal? (check-result-annotation result3 ann:num) 6)))))

; Provide statements -----------------------------

(provide annotation-tests)
