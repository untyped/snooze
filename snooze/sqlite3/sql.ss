#lang scheme/base
  
(require scheme/unit)
  
(require (file "../base.ss")
         (file "../generic/sql-data-sig.ss")
         (file "../generic/sql-name-sig.ss")
         (file "../generic/sql-query-sig.ss")
         (file "../generic/sql-query-unit.ss")
         (file "../generic/sql-update-sig.ss")
         (file "sql-data-unit.ss")
         (file "sql-name-unit.ss")
         (file "sql-update-unit.ss"))

; Unit invocations -----------------------------

(define-compound-unit/infer sql@
  (import)
  (export sql-name^ sql-data^ sql-query^ sql-update^)
  (link sql-name@ sql-data@ sql-query@ sql-update@))

(define-values/invoke-unit/infer sql@)

; Provide statements ---------------------------

(provide-signature-elements sql-name^ sql-data^ sql-query^ sql-update^)
