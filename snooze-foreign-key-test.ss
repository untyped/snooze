#lang scheme/base

(require srfi/26
         "snooze-api.ss"
         "test-base.ss"
         "core/core.ss")

; Tests ------------------------------------------

; Tests of foreign key accessors (e.g. pet-owner).

(define snooze-foreign-key-tests
  (test-suite "snooze-foreign-key-tests: (foo-bar foo1) -> bar1"
    
    (test-case "snooze-struct-ref : loads struct from database"
      (recreate-test-tables)
      (let* ([per  (save! (make-person "Per1"))]
             [pet1 (save! (make-pet per "Pet1"))]
             [pet2 (select-one #:from pet)])
        (check-equal? (snooze-struct-ref pet2 (attr pet owner)) per)))
    
    (test-case "snooze-struct-ref* : loads struct from database"
      (recreate-test-tables)
      (let* ([per  (save! (make-person "Per1"))]
             [pet1 (save! (make-pet per "Pet1"))]
             [pet2 (select-one #:from pet)])
        (check-equal? (caddr (snooze-struct-ref* pet2)) per)))
    
    (test-case "pet-owner : loads struct from database"
      (recreate-test-tables)
      (let* ([per  (save! (make-person "Per1"))]
             [pet1 (save! (make-pet per "Pet1"))]
             [pet2 (select-one #:from pet)])
        (check-equal? (pet-owner pet2) per)))
    
    (test-case "foreign key relation between unsaved structs"
      (recreate-test-tables)
      (let* ([per1 (make-person "Per1")]
             [pet1 (make-pet per1 "Pet1")])
        (check-eq? (pet-owner pet1) per1)))
    
    (test-case "foreign key relation between unsaved structs : retained under copying"
      (recreate-test-tables)
      (let* ([per1 (make-person "Per1")]
             [pet1 (make-pet per1 "Pet1")]
             [pet2 (pet-set pet1 #:name "Pet2")])
        (check-eq? (pet-owner pet1) (pet-owner pet2))))
    
    (test-case "foreign key relation between unsaved structs : updated (to create a new copy)"
      (recreate-test-tables)
      (let* ([per1 (make-person "Per1")]
             [per2 (make-person "Per2")]
             [pet1 (make-pet per1 "Pet1")]
             [pet2 (pet-set pet1 #:owner per2)])
        (check-eq? per1 (pet-owner pet1))
        (check-eq? per2 (pet-owner pet2))))))

; Provide statements -----------------------------

(provide snooze-foreign-key-tests)
