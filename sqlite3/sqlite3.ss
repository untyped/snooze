#lang scheme/base
  
(require scheme/class
         scheme/contract)

(require "../generic/connection.ss"
         "../generic/database.ss"
         "database.ss")

; (U path ':memory: ':temp:) -> database%
(define (make-database path)
  (new database% [path path]))

; Provide statements -----------------------------

(provide (struct-out connection)
         database%)

(provide/contract
 [make-database (-> (or/c path? ':memory: ':temp:) (is-a?/c database<%>))])
