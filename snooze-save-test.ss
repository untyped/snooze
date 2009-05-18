#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "era/era.ss"
         (prefix-in real: "era/snooze-struct.ss"))

; Tests -------------------------------------------

; test-suite
(define snooze-save-tests
  (test-suite "snooze-save-tests"
    
    (test-case "save! : returns a local guid"
      (recreate-test-tables/cache)
      (let ([per (save! (make-person "Dave"))])
        (check-cache-size (list 3))
        (check-pred guid-local?   per)
        (check-pred struct-saved? per)))
    
    (test-case "save! : remaps old guid"
      (recreate-test-tables/cache)
      (let* ([per1 (make-person "Dave")]
             [per2 (save! per1)])
        (check struct-eq? per1 per2)))
    
    (test-case "save! : actually saves data"
      (recreate-test-tables/cache)
      (pretty-print (cache-alists))
      (with-cache
       (let* ([per1 (begin0 (save! (make-person "Dave"))
                            (check-equal? (direct-query "select count(id) from people;")
                                          (list (list 1)))
                            (pretty-print (cache-alists)))]
              [per2 (begin0 (save! (make-person "Dave"))
                            (check-equal? (direct-query "select count(id) from people;")
                                          (list (list 2)))
                            (pretty-print (cache-alists)))]
              [per3 (begin0 (save! per1)
                            (check-equal? (direct-query "select count(id) from people;")
                                          (list (list 2)))
                            (pretty-print (cache-alists)))]
              [per4 (begin0 (save! (make-person "Noel"))
                            (check-equal? (direct-query "select count(id) from people;")
                                          (list (list 3)))
                            (pretty-print (cache-alists)))])
         (check-cache-size (list 10 3))
         (check-equal? (direct-query "select name from people order by id asc;")
                       (list (list "Dave")
                             (list "Dave")
                             (list "Noel"))))))))

; Provide statements -----------------------------

(provide snooze-save-tests)