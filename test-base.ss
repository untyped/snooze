#lang scheme/base

(require "base.ss")

(require srfi/19
         (schemeunit-in test text-ui)
         "era/cache.ss"
         "era/era.ss"
         #;(prefix-in postgresql8: "postgresql8/postgresql8.ss")
         #;(prefix-in sqlite3: "sqlite3/sqlite3.ss"))

; test-suite -> any
(define (run-tests/no-database tests)
  (parameterize ([current-snooze (new (snooze-cache-mixin object%))])
    (run-tests tests)))

;  [#:server   string]
;  [#:port     natural]
;  [#:database string]
;  [#:username string]
;  [#:password (U string #f)]
;  test-suite
; ->
;  any
#;(define (run-tests/postgresql8 #:server   [server   "localhost"]
                               #:port     [port     5432]
                               #:database [database "snoozetest"]
                               #:username [username "dave"]
                               #:password [password #f]
                               tests)
  (parameterize ([current-snooze (make-snooze (postgresql8:make-database
                                               #:server   server
                                               #:port     port
                                               #:database database
                                               #:username username
                                               #:password password))])
    (run-tests tests)))

; (U string path ':memory: ':temp:) test-suite -> any
#;(define (run-tests/sqlite3 location tests)
  (parameterize ([current-snooze (make-snooze (sqlite3:make-database location))])
    (run-tests tests)))

; string -> time-tai
(define (string->time-tai str)
  (date->time-tai (string->date (string-append str "+0000") "~Y-~m-~d ~H:~M:~S~z")))

; string -> time-utc
(define (string->time-utc str)
  (date->time-utc (string->date (string-append str "+0000") "~Y-~m-~d ~H:~M:~S~z")))

; Provide statements --------------------------- 

(provide (all-from-out "base.ss")
         (schemeunit-out test text-ui))

(provide/contract
 [run-tests/no-database (-> test-suite? any)]
 #;[run-tests/postgresql8 (-> (test-suite?)
                            (#:server string?
                                      #:port     natural-number/c
                                      #:database string?
                                      #:username string?
                                      #:password (or/c string? #f))
                            any)]
 #;[run-tests/sqlite3     (-> (or/c string? path? ':memory: ':temp:) test-suite? any)]
 [string->time-tai      (-> string? time-tai?)]
 [string->time-utc      (-> string? time-utc?)])