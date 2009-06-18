#lang scheme/base

(require "../base.ss")

(require srfi/13)

; symbol -> string
(define (name->pretty-name name)
  (let* ([str   (symbol->string name)]
         [words (regexp-split #px"-" str)])
    (string-join words " ")))

; symbol -> symbol
(define (name->plural-name name)
  (string->symbol (format "~as" name)))

; string -> string
(define (pretty-name->pretty-name-plural name)
  (format "~as" name))

(provide/contract
 [name->pretty-name               (-> symbol? string?)]
 [name->plural-name               (-> symbol? symbol?)]
 [pretty-name->pretty-name-plural (-> string? string?)])
