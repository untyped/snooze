(module run-tests mzscheme
  
  (require (lib "etc.ss")
           (lib "cut.ss" "srfi" "26"))
  
  (require (prefix sqlite3: (file "sqlite3/all-sqlite3-tests.ss"))
           (prefix sqlite3: (file "sqlite3/sqlite3.ss"))
           (prefix postgresql8: (file "postgresql8/all-postgresql8-tests.ss"))
           (prefix postgresql8: (file "postgresql8/postgresql8.ss"))
           (file "all-snooze-tests.ss")
           (file "snooze.ss")
           (file "test-base.ss"))
  
  (print-struct #t)
  (print-hash-table #t)
  (error-print-width 1024)
  
  ;; run-tests : db^ config -> void
  (define (run-tests db@ config backend-tests)
    (define-snooze-interface db@)
    (call-with-database config
                        (lambda ()
                          (test/text-ui (make-all-snooze-tests db@ backend-tests)))))
  
  (define (handle-command-line argv)
    (with-handlers
        ([exn? 
          (lambda (exn)
            (printf #<<ENDOUTPUT
Oops! Exception raised!

Usage:
  mzscheme <options> run-tests.ss sqlite3 [<filename>]
  mzscheme <options> run-tests.ss postgresql8 <server> <port> <database> <username> <password>

Exception was:

ENDOUTPUT
                    )
            (raise exn))])
      (if (> (vector-length argv) 0)
          (cond [(equal?  (vector-ref argv 0) "sqlite3")
                 (let ([filename (if (> (vector-length argv) 1)
                                     (vector-ref argv 1)
                                     "snooze-sqlite3-test.db")])
                   (when (file-exists? filename)
                     (delete-file filename))
                   (run-tests sqlite3:db@
                              (sqlite3:make-config (string->path filename))
                              sqlite3:all-sqlite3-tests)
                   (when (file-exists? filename)
                     (delete-file filename)))]
                [(equal?  (vector-ref argv 0) "postgresql8")
                 (let ([host (vector-ref argv 1)]
                       [port (string->number (vector-ref argv 2))]
                       [database (vector-ref argv 3)]
                       [username (vector-ref argv 4)]
                       [password (vector-ref argv 5)])
                   (run-tests postgresql8:db@
                              (postgresql8:make-config host port database username password #:ssl 'optional)
                              postgresql8:all-postgresql8-tests))]
                [else (raise-exn exn:fail:snooze
                        (format "Invalid backend specified: ~a" (vector-ref argv 0)))])
          (raise-exn exn:fail:snooze "No backend specified!"))))
  
  (handle-command-line (current-command-line-arguments))
  
  )