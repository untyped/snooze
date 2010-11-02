#lang scheme/base

(require mzlib/etc
         scheme/class
         scheme/match
         srfi/13/string
         srfi/26/cut
         (prefix-in sqlite3: "sqlite3/all-sqlite3-tests.ss")
         (prefix-in sqlite3: "sqlite3/sqlite3.ss")
         (prefix-in postgresql8: "postgresql8/all-postgresql8-tests.ss")
         (prefix-in postgresql8: "postgresql8/postgresql8.ss")
         "all-snooze-tests.ss"
         "snooze.ss"
         "snooze-class.ss"
         "test-base.ss")

(print-struct #t)
(print-hash-table #t)
(error-print-width 1024)

; snooze<%> test-suite -> void
(define (run-snooze-tests snooze back-end-tests)
  (run-tests (make-snooze-tests snooze back-end-tests))
  (void))

; string -> void
(define (run-sqlite3-tests filename)
  (define pre-existing-file?
    (file-exists? filename))
  (when pre-existing-file?
    (delete-file filename))
  (run-snooze-tests
   (make-snooze (sqlite3:make-database (string->path filename)))
   sqlite3:all-sqlite3-tests)
  (unless pre-existing-file?
    (delete-file filename)))

; string integer string string [string] -> void
(define (run-postgresql8-tests server port database username [password #f])
  (let ([snooze (make-snooze (postgresql8:make-database #:server                 server
                                                        #:port                   port
                                                        #:database               database
                                                        #:username               username
                                                        #:password               password
                                                        #:pool-connections?      #t))])
    (run-snooze-tests
     snooze
     (postgresql8:make-all-postgresql8-tests snooze))))

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
    [(list "sqlite3")
     (run-sqlite3-tests "snooze-sqlite3-test.db")]
    [(list "sqlite3" filename)
     (run-sqlite3-tests filename)]
    [(list-rest "sqlite3" _)
     (raise-exn exn:fail:snooze "Bad options for sqlite3.")]
    [(list "postgresql8")
     (run-postgresql8-tests "localhost" 5432 "snoozetest" "dave" #f)]
    [(list "postgresql8" host (app string->number (? integer? port)) database username)
     (run-postgresql8-tests host port database username #f)]
    [(list "postgresql8" host (app string->number (? integer? port)) database username password)
     (run-postgresql8-tests host port database username password)]
    [(list-rest "postgresql8" _)
     (raise-exn exn:fail:snooze "Bad options for postgresql8.")]
    [(list-rest back-end _)
     (raise-exn exn:fail:snooze (format "Unrecognised back-end: ~a" back-end))]))
