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
         (check-pred guid-local?          per1)
         (check-pred snooze-struct-saved? per1)
         (check-pred guid-local?          per2)
         (check-pred snooze-struct-saved? per2))))
    
    (test-case "save!, person-set : remaps guids appropriately"
      (recreate-test-tables/cache)
      (let* ([per1a   (make-person "Dave")]
             [per1b   (person-set per1a #:name "Noel")]
             [per2a   (save! per1b)]
             [per2b   (person-set per2a #:name "Matt")])
        (check-false (eq? per1a per1b))
        (check-false (eq? per1b per2a))
        (check-false (eq? per2a per2b))
        (check-false (snooze-struct-eq? per1a per1b))
        (check-true  (snooze-struct-eq? per1b per2a))
        (check-false (snooze-struct-eq? per2a per2b))
        (check-equal? (person-name per1a) "Dave")
        (check-equal? (person-name per1b) "Noel")
        (check-equal? (person-name per2a) "Noel")
        (check-equal? (person-name per2b) "Matt")))
    
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
      (with-cache
       (let* ([per1 (save! (make-person "Dave"))]
              [per2 (save! (make-person "Dave"))]
              [per3 (save! per1)]
              [per4 (save! (make-person "Noel"))])
         (check-equal? (direct-query "select id,name from people order by id asc;")
                       (list (list (snooze-struct-id per1) "Dave")
                             (list (snooze-struct-id per2) "Dave")
                             (list (snooze-struct-id per4) "Noel"))))))
    
    (test-case "save! : consecutive saves"
      (recreate-test-tables/cache)
      (let* ([per1 (save! (make-person "Jon"))]
             [per2 (save! (person-set per1 #:name "Lyman"))]
             [per3 (save! (make-person "Liz"))]
             [per4 (save! (make-person "Liz"))])
        (check-equal? (direct-query "select count(id) from people where name = 'Jon';") (list (list 0)))
        (check-equal? (direct-query "select count(id) from people where name = 'Lyman';") (list (list 1)))
        (check-equal? (direct-query "select count(id) from people where name = 'Liz';") (list (list 2)))))
    
    (test-case "save! : bad data types"
      (recreate-test-tables/cache)
      (check-exn exn:fail:snooze? (cut save! (make-person 'R2D2)))
      (let ([pet1 (save! (make-pet #f "Garfield"))])
        (check-exn exn:fail:snooze? (cut save! (make-pet pet1 "Odie")))))
    
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
        (check-not-false (real:snooze-struct-guid struct))     ; ... it has a valid guid,
        (check-not-false (real:snooze-struct-id struct))       ; ... id,
        (check-not-false (real:snooze-struct-revision struct)) ; ... and revision.
        ; per2
        (check-pred guid-local? per2)                          ; per2 is a local guid...
        (check-pred (entity-private-predicate person) struct2) ; it also points to a saved person...
        (check-true (snooze-struct-eq? per per2))              ; ... exactly the same one as per.
        (check-true (eq? struct struct2))                      ; (just to be sure)
        (check-false (eq? per per2))                           ; However, guids themselves are not eq?
        (check-not-false vanilla2)                             ; per2 also points to a vanilla guid ...
        (check-true (eq? vanilla vanilla2))))                  ; ... the same one as per
    
    (test-case "save! : revision incremented on save"
      (recreate-test-tables/cache)
      (let ([per0 (make-person "Dave")])
        (check-false (snooze-struct-revision per0))
        (let ([per1 (save! per0)])
          (check-equal? (snooze-struct-revision per1) 0)
          (check-equal? (snooze-struct-revision per0) 0)
          (let ([per2 (save! (person-set per1 #:name "Noel"))])
            (check-equal? (snooze-struct-revision per2) 1)
            (check-equal? (snooze-struct-revision per1) 0)
            (check-equal? (snooze-struct-revision per0) 0))))
      (check-equal? (direct-query "select count(id) from people where revision = 0;") (list (list 0)))
      (check-equal? (direct-query "select count(id) from people where revision = 1;") (list (list 1))))
    
    (test-case "save! : cannot save an out-of-date struct"
      (recreate-test-tables/cache)
      (let* ([per1 (save! (make-person "Dave"))]
             [per2 (save! (person-set per1 #:name "Noel"))])
        (check-exn exn:fail:snooze? (cut save! (person-set per1 #:name "Matt")))))
    
    (test-case "save! : cannot save a struct with local-only foreign keys"
      (recreate-test-tables/cache)
      (let* ([per (save! (make-person "Jon"))]
             [pet (make-pet per "Garfield")])
        (check-not-exn (cut save! pet)))
      
      (recreate-test-tables/cache)
      (let* ([per (save! (make-person "Jon"))]
             [pet1 (save! (make-pet per "Garfield"))]
             [pet2 (save! (make-pet (pet-owner pet1) "Garfield"))])
        (check-not-exn (cut save! pet2)))
      
      (recreate-test-tables/cache)
      (let* ([per (make-person "Jon")]
             [pet (make-pet per "Garfield")])
        (check-exn exn:fail:snooze? (cut save! pet)))
      
      (recreate-test-tables/cache)
      (let* ([per0 (make-person "Jon")]
             [per1 (save! per0)]
             [per2 (person-set per1 #:name "Lyman")]
             [pet  (make-pet per2 "Garfield")])
        (check-exn exn:fail:snooze? (cut save! pet))))))

; Provide statements -----------------------------

(provide snooze-save-tests)