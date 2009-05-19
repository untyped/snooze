#lang scheme/base

(require srfi/26
         "snooze-api.ss"
         "test-base.ss"
         "era/era.ss")

; Tests ------------------------------------------

; Tests of foreign key accessors (e.g. pet-owner).

(define snooze-foreign-key-tests
  (test-suite "snooze-foreign-key-tests: (foo-bar foo1) -> bar1"
    
    (test-case "A foreign key relation holds between unsaved structs"
      (recreate-test-tables/cache)
      (let* ([per1        (make-person "Per1")]
             [pet1        (make-pet per1 "Pet1")])
        (check-true  (guid-local? per1))                           ; both local guids
        (check-true  (guid-local? pet1))
        (check-false (eq? (pet-owner pet1) per1))                  ; refer to same struct, but guids are different
        (check-true  (struct-eq? (pet-owner pet1) per1))
        (check-false (send (current-cache) get-vanilla-guid per1)) ; no vanilla guids
        (check-false (send (current-cache) get-vanilla-guid pet1))
        (check-false (send (current-cache) get-vanilla-guid (pet-owner pet1)))))
    
    (test-case "A foreign key relation between unsaved structs is retained under copying"
      (recreate-test-tables/cache)
      (let* ([per1        (make-person "Per1")]
             [pet1        (make-pet per1 "Pet1")]
             [pet2        (pet-set pet1 #:name "Pet2")])
        (check-equal? (pet-owner pet1) (pet-owner pet2))            ; structs are equal? ...
        (check-equal? per1 (pet-owner pet2))
        (check-true (struct-eq? (pet-owner pet1) (pet-owner pet2))) ; ... and struct-eq? ...
        (check-true (struct-eq? per1 (pet-owner pet2)))
        (check-false (eq? per1 (pet-owner pet2)))                   ; ... but not eq?
        (check-false (eq? (pet-owner pet1) (pet-owner pet2)))))
    
    (test-case "A foreign key relation between unsaved structs can be updated (to create a new copy)"
      (recreate-test-tables/cache)
      (let* ([per1        (make-person "Per1")]
             [per2        (make-person "Per2")]
             [pet1        (make-pet per1 "Pet1")]
             [pet2        (pet-set pet1 #:owner per2)])
        (check-equal? per1 (pet-owner pet1))                         ; equality
        (check-false (equal? (pet-owner pet1) (pet-owner pet2)))            
        (check-equal? per2 (pet-owner pet2))
        (check-true  (struct-eq? per1 (pet-owner pet1)))             ; struct-eq?ity ...
        (check-false (struct-eq? (pet-owner pet1) (pet-owner pet2))) 
        (check-true  (struct-eq? per2 (pet-owner pet2)))
        (check-false (eq? per1 (pet-owner pet1)))                    ; eq?ity
        (check-false (eq? per2 (pet-owner pet2))) 
        (check-false (eq? (pet-owner pet1) (pet-owner pet2)))))
    
    (test-case "cache sizes are correct"
      (recreate-test-tables/cache)
      (with-cache
       (let* ([per1 (make-person "Per1")]
              [pet1 (make-pet per1 "Pet1")])
         (collect-garbage)
         (check-cache-size (list 2 0))
         (let ([per2 (pet-owner pet1)])
           (collect-garbage)
           (check-cache-size (list 3 0)))
         (collect-garbage)
         (check-cache-size (list 2 0)))))))

; Provide statements -----------------------------

(provide snooze-foreign-key-tests)
