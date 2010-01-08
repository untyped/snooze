;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module concurrent mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 7))
           (lib "list.ss")
           (lib "etc.ss")
           (lib "class.ss")
           "config.ss"
           "../spgsql.ss")
  (provide concurrent-test)
  
  (define (make-slow-output-port out pause? limit)
    (make-output-port 'slow-port
                      out
                      (lambda (buf start end ok-buffer? ok-break?)
                        (when pause? (sleep 0.01))
                        (let ([end (min end (+ start limit))])
                          (write-bytes-avail buf out start end)))
                      (lambda () (close-output-port out))))
  
  (define (((mk-worker c iterations) tid))
    (define insert
      (send c prepare-exec "insert into play_numbers (n) values ($1)"))
    (define (add-to-max n)
      (insert (+ n (send c query-value "select max(n) from play_numbers"))))
    (for-each insert (build-list iterations add1))
    (for-each add-to-max (build-list iterations add1))
    (printf "~s: ~s\n"
            tid
            (send c query-value "select max(n) from play_numbers"))
    (send c query "select * from pg_type"))
  
  (define concurrent-test
    (test-suite "Concurrency"
      (test-case "lots of threads"
        (call-with-connection
         (lambda (c)
           (send c exec "create temporary table play_numbers (n integer)")
           (for-each thread-wait
                     (map thread
                          (map (mk-worker c 100) (build-list 20 add1)))))))
      (test-case "threads with pausing ports"
        (parameterize ((testing-connection-mixin
                        (lambda (%)
                          (class %
                            (define/override (attach-to-ports in out)
                              (super attach-to-ports
                                     in 
                                     (make-slow-output-port out #t 256)))
                            (super-new)))))
          (call-with-connection
           (lambda (c)
             (send c exec "create temporary table play_numbers (n integer)")
             (for-each thread-wait
                       (map thread
                            (map (mk-worker c 5) (build-list 4 add1))))))))
      (test-case "threads with small-chunk ports"
        (parameterize ((testing-connection-mixin
                        (lambda (%)
                          (class %
                            (define/override (attach-to-ports in out)
                              (super attach-to-ports
                                     in 
                                     (make-slow-output-port out #f 1)))
                            (super-new)))))
          (call-with-connection
           (lambda (c)
             (send c exec "create temporary table play_numbers (n integer)")
             (for-each thread-wait
                       (map thread
                            (map (mk-worker c 5) (build-list 4 add1)))))))))))
