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
    
    (test-case "save! : stores information correctly in the cache"
      (recreate-test-tables/cache)
      (pretty-print (cache-lists))
      (with-cache
       (let*/debug ([per1 (begin0 (save! (make-person "Dave"))
                                  (check-equal? (direct-query "select count(id) from people;")
                                                (list (list 1)))
                                  (collect-garbage)
                                  (debug* "per1 cache" cache-lists)
                                  (check-cache-size (list 2 1)))]
                    [per2 (begin0 (save! (make-person "Dave"))
                                  (check-equal? (direct-query "select count(id) from people;")
                                                (list (list 2)))
                                  (collect-garbage)
                                  (debug* "per2 cache" cache-lists)
                                  (check-cache-size (list 4 2)))]
                    [per3 (begin0 (save! per1)
                                  (debug* "per3 cache1" cache-lists)
                                  (check-equal? (direct-query "select count(id) from people;")
                                                (list (list 2)))
                                  (collect-garbage)
                                  (collect-garbage)
                                  (debug* "per3 cache2" cache-lists)
                                  (debug "per3 per1" (list per1 (guid-ref per1)))
                                  (check-cache-size (list 5 2)))]
                    [per4 (begin0 (save! (make-person "Noel"))
                                  (check-equal? (direct-query "select count(id) from people;")
                                                (list (list 3)))
                                  (collect-garbage)
                                  (debug* "per4 cache" cache-lists)
                                  (check-cache-size (list 7 3)))])
                   (void))))
    
    (test-case "save! : stores information correctly in the database"
      (recreate-test-tables/cache)
      (pretty-print (cache-lists))
      (with-cache
       (let*/debug ([per1 (save! (make-person "Dave"))]
                    [per2 (save! (make-person "Dave"))]
                    [per3 (save! per1)]
                    [per4 (save! (make-person "Noel"))])
                   (check-equal? (direct-query "select id,name from people order by id asc;")
                                 (list (list (struct-id per1) "Dave")
                                       (list (struct-id per2) "Dave")
                                       (list (struct-id per4) "Noel"))))))))

; Provide statements -----------------------------

(provide snooze-save-tests)