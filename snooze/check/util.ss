#lang scheme/base

(require scheme/contract
         "../era/era.ss"
         "annotation.ss"
         "result.ss")

; (annotation (listof attribute))
(define-annotation ann:attrs
  ; check-result -> (listof attribute)
  (lambda (result)
    null)
  ; check-result (listof attribute) (listof attribute) -> (listof attribute)
  (lambda (result old new) 
    (append old new)))

; check-result -> (listof symbol)
(define (check-result-attributes result)
  (hash-ref (check-result-annotations result) ann:attrs null))

; check-result attribute -> boolean
(define (check-result-has-attribute? result attr)
  (and (memq attr (check-result-attributes result)) #t))

; Provide statements -----------------------------

(provide/contract
 [ann:attrs                   annotation?]
 [check-result-attributes     (-> check-result? (listof attribute?))]
 [check-result-has-attribute? (-> check-result? attribute? boolean?)])
