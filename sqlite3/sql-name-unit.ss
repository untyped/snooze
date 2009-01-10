#lang scheme/unit
  
(require "../base.ss"
         "../generic/sql-name-sig.ss")

(import)

(export sql-name^)
  
; symbol -> string
(define (escape-name name)
  (string-append "[" (symbol->string name) "]"))
