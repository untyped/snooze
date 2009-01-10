#lang scheme/base
  
(require scheme/unit)
  
(require "../base.ss"
         "../generic/sql-data-sig.ss"
         "../generic/sql-name-sig.ss"
         "../generic/sql-query-sig.ss"
         "../generic/sql-update-sig.ss"
         "sql-data-unit.ss"
         "sql-name-unit.ss"
         "sql-query-unit.ss"
         "sql-update-unit.ss")

; Unit invocations -----------------------------

(define-compound-unit/infer sql@
  (import)
  (export sql-name^ sql-data^ sql-query^ sql-update^)
  (link sql-name@ sql-data@ sql-query@ sql-update@))

(define-values/invoke-unit/infer sql@)

; Provide statements ---------------------------

(provide-signature-elements sql-name^ sql-data^ sql-query^ sql-update^)
