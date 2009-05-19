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
      (with-cache
       (check-cache-size (list 0 0))
       (let* ([per1 (begin0 (make-person "Dave")
                            (collect-garbage)
                            (check-cache-size (list 1 0)))]
              [per2 (save! per1)])
         (collect-garbage)
         (check-cache-size (list 3 1))
         (check-pred guid-local?   per1)
         (check-pred struct-saved? per1)
         (check-pred guid-local?   per2)
         (check-pred struct-saved? per2))))
    
    (test-case "save! : remaps old guid"
      (recreate-test-tables/cache)
      (let* ([per1    (make-person "Dave")]
             [struct1 (guid-ref per1)]
             [per2    (save! per1)])
        (check struct-eq? per1 per2)
        (check-false (eq? (guid-ref per1) struct1))))
    
    (test-case "save! : stores information correctly in the cache"
      (recreate-test-tables/cache)
      (with-cache
       (let* ([per1 (begin0 (save! (make-person "Dave"))
                            (collect-garbage)
                            (collect-garbage)
                            (collect-garbage)
                            (collect-garbage)
                            (check-cache-size (list 2 1)))]
              [per2 (begin0 (save! (make-person "Dave"))
                            (collect-garbage)
                            (collect-garbage)
                            (collect-garbage)
                            (collect-garbage)
                            (check-cache-size (list 4 2)))]
              [per3 (begin0 (save! per1)
                            (collect-garbage)
                            (collect-garbage)
                            (collect-garbage)
                            (collect-garbage)
                            (check-cache-size (list 5 2)))]
              [per4 (begin0 (save! (make-person "Noel"))
                            (collect-garbage)
                            (collect-garbage)
                            (collect-garbage)
                            (collect-garbage)
                            (check-cache-size (list 7 3)))])
         (void))))
    
    (test-case "save! : stores information correctly in the database"
      (recreate-test-tables/cache)
      (pretty-print (cache-lists))
      (with-cache
       (let* ([per1 (save! (make-person "Dave"))]
              [per2 (save! (make-person "Dave"))]
              [per3 (save! per1)]
              [per4 (save! (make-person "Noel"))])
         (check-equal? (direct-query "select id,name from people order by id asc;")
                       (list (list (struct-id per1) "Dave")
                             (list (struct-id per2) "Dave")
                             (list (struct-id per4) "Noel"))))))
    
    (test-case "save! : creates a vanilla GUID and a local GUID, distinct from first."
      (recreate-test-tables/cache)
      (let* ([per      (make-person "Per")]
             [per2     (save! per)]
             [struct   (send (current-cache) cache-ref/local per)]
             [vanilla  (send (current-cache) get-vanilla-guid per)]
             [struct2  (send (current-cache) cache-ref/local per2)]
             [vanilla2 (send (current-cache) get-vanilla-guid per2)])
        ; per, having been remapped following save!
        (check-pred guid-local? per)                           ; still a local guid
        (check-not-false vanilla)                              ; but it now has a vanilla-guid
        (check-pred (entity-private-predicate person) struct)  ; still refers to a person, but now it's saved:
        (check-not-false (real:struct-guid struct))            ; ... it has a valid guid,
        (check-not-false (real:struct-id struct))              ; ... id,
        (check-not-false (real:struct-revision struct))        ; ... and revision.
        ; per2
        (check-pred guid-local? per2)                          ; per2 is a local guid...
        (check-pred (entity-private-predicate person) struct2) ; it also points to a saved person...
        (check-true (struct-eq? per per2))                     ; ... exactly the same one as per.
        (check-true (eq? struct struct2))                      ; (just to be sure)
        (check-false (eq? per per2))                           ; However, guids themselves are not eq?
        (check-not-false vanilla2)                             ; per2 also points to a vanilla guid ...
        (check-true (eq? vanilla vanilla2))))))                ; ... the same one as per    

; Provide statements -----------------------------

(provide snooze-save-tests)