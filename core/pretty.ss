#lang scheme/base

(require "../base.ss")

(require srfi/13)

; symbol -> string
(define (name->pretty-name name)
  (let* ([str   (symbol->string name)]
         [words (regexp-split #px"-" str)])
    (string-join words " ")))

; string -> string
(define (pluralize-pretty-name pretty)
  (format "~as" pretty))

(provide/contract
 [name->pretty-name     (-> symbol? string?)]
 [pluralize-pretty-name (-> string? string?)])
