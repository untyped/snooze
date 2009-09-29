#lang scheme/base

(require "base.ss")

(require scheme/dict
         (schemeunit-in test)
         (unlib-in hash)
         "snooze-api.ss"
         "test-data.ss"
         "core/core.ss")

; (parameter (string -> (listof (listof any))))
(define direct-query-proc
  (make-parameter (lambda (sql) (error "direct queries not initialised"))))

; string -> (listof (listof any))
(define (direct-query sql)
  ((direct-query-proc) sql))

; (_ guid guid boolean boolean boolean)
;
; Shorthand syntax for checking eq?, guid=? and equal? equality on guids/structs.
(define-syntax-rule (check-equality a b eq guid= equal)
  (begin
    (with-check-info (['actual a] ['expected b] ['comparison 'eq?])
      (if eq
          (check-true (eq? a b))
          (check-false (eq? a b))))
    
    (with-check-info (['actual a] ['expected b] ['comparison 'guid=?])
      (if guid=
          (check-true (guid=? a b))
          (check-false (guid=? a b))))
    
    (with-check-info (['actual a] ['expected b] ['comparison 'equal?])
      (if equal
          (check-true (equal? a b))
          (check-false (equal? a b))))))

; Provide statements -----------------------------

(provide (all-defined-out))
