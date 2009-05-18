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
                                  (debug* "per1 cache1" cache-lists)
                                  (collect-garbage)
                                  (collect-garbage)
                                  (collect-garbage)
                                  (collect-garbage)
                                  (debug* "per1 cache2" cache-lists)
                                  (check-cache-size (list 2 1)))]
                    [per2 (begin0 (save! (make-person "Dave"))
                                  (debug* "per2 cache1" cache-lists)
                                  (collect-garbage)
                                  (collect-garbage)
                                  (collect-garbage)
                                  (collect-garbage)
                                  (debug* "per2 cache2" cache-lists)
                                  (check-cache-size (list 4 2)))]
                    [per3 (begin0 (save! per1)
                                  (debug* "per3 cache1" cache-lists)
                                  (collect-garbage)
                                  (collect-garbage)
                                  (collect-garbage)
                                  (collect-garbage)
                                  (debug* "per3 cache2" cache-lists)
                                  (debug "per3 people"
                                         (list (list per1)
                                               (list per2)))
                                  (check-cache-size (list 5 2)))]
                    [per4 (begin0 (save! (make-person "Noel"))
                                  (debug* "per4 cache1" cache-lists)
                                  (collect-garbage)
                                  (collect-garbage)
                                  (collect-garbage)
                                  (collect-garbage)
                                  (debug* "per4 cache2" cache-lists)
                                  (debug "per3 people"
                                         (list (list per1)
                                               (list per2)
                                               (list per3)))
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