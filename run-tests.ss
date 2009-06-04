#lang scheme/base

(require mzlib/etc
         scheme/class
         scheme/match
         srfi/13/string
         srfi/26/cut
         "sqlite3/all-sqlite3-tests.ss"
         "sqlite3/sqlite3.ss"
         "postgresql8/all-postgresql8-tests.ss"
         "postgresql8/postgresql8.ss"
         "all-snooze-tests.ss"
         "snooze.ss"
         "snooze-class.ss"
         "test-base.ss")

(print-struct #t)
(print-hash-table #t)
(error-print-width 1024)

; [(U exn #f)] -> void
(define (print-usage [exn #f])
  (when exn 
    (printf "Oops! Exception raised!~n~n"))
  (printf #<<ENDOUTPUT
Usage:
mzscheme <options> run-tests.ss sqlite3 [<filename>]
mzscheme <options> run-tests.ss postgresql8 <server> <port> <database> <username> <password>

ENDOUTPUT
          )
  (when exn
    (raise exn)))

; Main program body ------------------------------

(with-handlers ([exn? print-usage]) 
  (match (vector->list (current-command-line-arguments))
    [(list) (print-usage)]
    [(list-rest "sqlite3" rest)
     (let ([tests (make-snooze-tests all-sqlite3-tests)])
       (match rest
         [(list)
          (run-tests/sqlite3 ':memory: tests)]
         [(list location)
          (run-tests/sqlite3 location tests)]
         [_ (error "bad sqlite3 options")]))]
    [(list-rest "postgresql8" rest)
     (let ([tests (make-snooze-tests all-postgresql8-tests)])
       (match rest
         [(list)
          (run-tests/postgresql8 tests)]
         [(list server (app string->number (? integer? port)) database username)
          (run-tests/postgresql8 #:server server #:port port #:database database #:username username tests)]
         [(list server (app string->number (? integer? port)) database username password)
          (run-tests/postgresql8 #:server server #:port port #:database database #:username username #:password password tests)]
         [_ (error "bad postgresql8 options")]))]
    [(list-rest dbms _)
     (error "bad dbms" dbms)]))
