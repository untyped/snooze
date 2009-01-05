#lang scheme/base
  
(require scheme/class
         scheme/contract)

(require (file "../generic/connection.ss")
         (file "../generic/database.ss")
         (file "database.ss"))

; path -> database%
(define (make-database path)
  (new database% [path path]))

; Provide statements -----------------------------

(provide (struct-out connection)
         database%)

(provide/contract
 [make-database (-> (or/c path? string?) (is-a?/c database<%>))])
