#lang scheme/base

(require scheme/contract
         "core/era.ss"
         (prefix-in sql: (only-in "sql/sql-lang.ss"
                                  attr or in null? =))
         (prefix-in sql: (only-in "sql/sql-struct.ss"
                                  expression?
                                  attribute-alias?
                                  attribute-alias-attribute)))

; Procedures -------------------------------------

; attribute any -> sql-expr
(define (quick-find-expression alias value)
  (cond [(void? value)      #t]
        [(list? value)      (sql:in alias value)]
        [(procedure? value) (value alias)]
        [(not value)        (if (boolean-type? (attribute-type (sql:attribute-alias-attribute alias)))
                                (sql:or (sql:null? alias)
                                        (sql:= alias #f))
                                (sql:null? alias))]
        [else               (sql:= alias value)]))

; Provide statements -----------------------------

(provide/contract
 [quick-find-expression (-> sql:attribute-alias? any/c sql:expression?)])
