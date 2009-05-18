#lang scheme/base

(require "base.ss")

(require srfi/19
         (schemeunit-in test text-ui)
         (prefix-in sqlite: (sqlite-in sqlite))
         "cache.ss"
         "snooze-class.ss"
         "test-data.ss"
         "test-util.ss"
         "era/era.ss"
         "postgresql8/postgresql8.ss"
         "sqlite3/sqlite3.ss")

;  [#:server   string]
;  [#:port     natural]
;  [#:database string]
;  [#:username string]
;  [#:password (U string #f)]
;  test-suite
; ->
;  any
(define (run-tests/postgresql8 #:server   [server   "localhost"]
                               #:port     [port     5432]
                               #:database [database "snoozetest"]
                               #:username [username "snooze"]
                               #:password [password #f]
                               tests)
  (parameterize ([current-snooze    (make-snooze (make-postgresql8-database
                                                  #:server   server
                                                  #:port     port
                                                  #:database database
                                                  #:username username
                                                  #:password password))]
                 [direct-query-proc (lambda (sql)
                                      (let ([conn (send (current-snooze) current-connection)])
                                        (send (connection-back-end conn) map sql list)))])
    (send (current-snooze) call-with-connection (cut run-tests tests))))

; (U string path ':memory: ':temp:) test-suite -> any
(define (run-tests/sqlite3 location tests)
  (let* ([file?     (or (path? location) (string? location))]
         [existing? (and file? (file-exists? location))])
    (when (and file? existing?)
      (delete-file location))
    (parameterize ([current-snooze    (make-snooze (make-sqlite3-database location))]
                   [direct-query-proc (lambda (sql)
                                        (let* ([conn (send (current-snooze) current-connection)]
                                               [ans  (sqlite:select (connection-back-end conn) sql)])
                                          (if (null? ans) null (cdr ans))))])
      (run-tests tests))
    (when (and file? (not existing?))
      (delete-file location))))

; string -> time-tai
(define (string->time-tai str)
  (date->time-tai (string->date (string-append str "+0000") "~Y-~m-~d ~H:~M:~S~z")))

; string -> time-utc
(define (string->time-utc str)
  (date->time-utc (string->date (string-append str "+0000") "~Y-~m-~d ~H:~M:~S~z")))

; Provide statements --------------------------- 

(provide (all-from-out "base.ss"
                       "test-data.ss"
                       "test-util.ss")
         (except-out (schemeunit-out test text-ui)
                     run-tests))

(provide/contract
 [run-tests/postgresql8 (->* (test-suite?)
                             (#:server string?
                                       #:port     natural-number/c
                                       #:database string?
                                       #:username string?
                                       #:password (or/c string? #f))
                             any)]
 [run-tests/sqlite3     (-> (or/c string? path? ':memory: ':temp:) test-suite? any)]
 [string->time-tai      (-> string? time-tai?)]
 [string->time-utc      (-> string? time-utc?)])
