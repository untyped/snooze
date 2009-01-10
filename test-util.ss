#lang scheme/base
  
(require srfi/19/time)
  
; Procedures -----------------------------------

; string -> time-tai
(define (string->time-tai str)
  (date->time-tai (string->date (string-append str "+0000") "~Y-~m-~d ~H:~M:~S~z")))

; string -> time-utc
(define (string->time-utc str)
  (date->time-utc (string->date (string-append str "+0000") "~Y-~m-~d ~H:~M:~S~z")))

; Provide statements ---------------------------

(provide string->time-tai
         string->time-utc)
