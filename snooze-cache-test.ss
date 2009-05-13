#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "test-data.ss"
         "snooze-cache-test-util.ss"
         "era/era.ss")

; Tests -------------------------------------------

; test-suite
(define snooze-cache-tests
  (test-suite "snooze-cache-tests"
    
    #:before
    recreate-test-tables
    
    #:after
    drop-all-tables
    
    (test-suite "struct/guid equality"
      
      (test-case "create"
        (recreate-test-tables)
        (cache-clear!)
        (let ([per1 (make-person "Dave")]
              [per2 (make-person "Dave")])
          (check-cache-size (list 2))
          (check-not-eq? per1 per2)
          (check-false (guid=? per1 per2))
          (check-equal?  per1 per2)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "Dave")))
      
      (test-case "load"
        (recreate-test-tables)
        (save! (make-person "Dave"))
        (cache-clear!)
        (let ([per1 (select-one #:from person)]
              [per2 (select-one #:from person)])
          (check-cache-size (list 1))
          (check-not-eq? per1 per2)
          (check-true (guid=? per1 per2))
          (check-equal? per1 per2)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "Dave")))
      
      (test-case "create, copy"
        (recreate-test-tables)
        (cache-clear!)
        (let ([per1 (make-person "Dave")]
              [per2 (person-set (make-person "Dave"))])
          (check-cache-size (list 3))
          (check-not-eq? per1 per2)
          (check-false (guid=? per1 per2))
          (check-equal? per1 per2)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "Dave")))
      
      (test-case "load, copy"
        (recreate-test-tables)
        (save! (make-person "Dave"))
        (cache-clear!)
        (let ([per1 (select-one #:from person)]
              [per2 (person-set (select-one #:from person))])
          (check-cache-size (list 2))
          (check-not-eq? per1 per2)
          (check-false (guid=? per1 per2))
          (check-equal? per1 per2)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "Dave")))
      
      (test-case "create, modify"
        (recreate-test-tables)
        (cache-clear!)
        (let* ([per1 (make-person "Dave")]
               [per2 (person-set per1 #:name "David")])
          (check-cache-size (list 2))
          (check-not-eq? per1 per2)
          (check-false (guid=? per1 per2))
          (check-not-equal? per1 per2)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "David")))
      
      (test-case "load, modify"
        (recreate-test-tables)
        (save! (make-person "Dave"))
        (cache-clear!)
        (let* ([per1 (select-one #:from person)]
               [per2 (person-set per1 #:name "David")])
          (check-cache-size (list 2))
          (check-not-eq? per1 per2)
          (check-false (guid=? per1 per2))
          (check-not-equal? per1 per2)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "David")))
      
      (test-case "create, modify, save"
        (recreate-test-tables)
        (cache-clear!)
        (let* ([per1 (make-person "Dave")]
               [per2 (person-set per1 #:name "David")]
               [per3 (save! per2)])
          (check-cache-size (list 2))
          (check-not-eq? per1 per3)
          (check-false (guid=? per1 per3))
          (check-not-equal? per1 per3)
          (check-eq? per2 per3)
          (check-true (guid=? per2 per3))
          (check-equal? per2 per3)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "David")
          (check-equal? (person-name per3) "David")))
      
      (test-case "load, modify, save"
        (recreate-test-tables)
        (save! (make-person "Dave"))
        (cache-clear!)
        (let* ([per1 (select-one #:from person)]
               [per2 (person-set per1 #:name "David")]
               [per3 (save! per2)])
          (check-equal? (equal-hash-code per1)
                        (equal-hash-code per3))
          (check-equal? (equal-hash-code per2)
                        (equal-hash-code per3))
          (check-cache-size (list 1))
          (check-not-eq? per1 per3)
          (check-true (guid=? per1 per3))
          (check-equal? per1 per3)
          (check-eq? per2 per3)
          (check-true (guid=? per2 per3))
          (check-equal? per2 per3)
          (check-equal? (person-name per1) "David")
          (check-equal? (person-name per2) "David")
          (check-equal? (person-name per3) "David"))))
    
    (test-suite "inter-struct references"
      
      (test-case "create both"
        (recreate-test-tables)
        (cache-clear!)
        (let* ([per1 (make-person "Jon")]
               [pet1 (make-pet per1 "Garfield")])
          (check-cache-size (list 2))
          (check-not-eq? per1 pet1)
          (check-false (guid=? per1 pet1))
          (check-not-equal? per1 pet1)
          (check-equal? (person-name per1) "Jon")
          (check-equal? (pet-name pet1) "Garfield")
          (check-eq? (pet-owner pet1) per1)))
      
      (test-case "load both"
        (recreate-test-tables)
        (let ([per1 (save! (make-person "Jon"))])
          (save! (make-pet per1 "Garfield")))
        (cache-clear!)
        (let* ([per1 (select-one #:from person)]
               [pet1 (select-one #:from pet)])
          (check-cache-size (list 2))
          (check-not-eq? per1 pet1)
          (check-false (guid=? per1 pet1))
          (check-not-equal? per1 pet1)
          (check-equal? (person-name per1) "Jon")
          (check-equal? (pet-name pet1) "Garfield")
          (check-not-eq? (pet-owner pet1) per1)
          (check guid=? (pet-owner pet1) per1)
          (check-equal? (pet-owner pet1) per1)))
      
      (test-case "load one, implicitly load other"
        (recreate-test-tables)
        (let ([per1 (save! (make-person "Jon"))])
          (save! (make-pet per1 "Garfield")))
        (cache-clear!)
        (let* ([pet1 (select-one #:from pet)]
               [per1 (pet-owner pet1)])
          (check-cache-size (list 1))
          (check-not-eq? per1 pet1)
          (check-false (guid=? per1 pet1))
          (check-not-equal? per1 pet1)
          (check-cache-size (list 1))
          (check-equal? (person-name per1) "Jon")
          (check-cache-size (list 2))
          (check-equal? (pet-name pet1) "Garfield")
          (check-eq? (pet-owner pet1) per1)
          (check guid=? (pet-owner pet1) per1)
          (check-equal? (pet-owner pet1) per1)))
      
      (test-case "create both, copy one"
        (recreate-test-tables)
        (cache-clear!)
        (let* ([per1 (make-person "Jon")]
               [pet1 (make-pet per1 "Garfield")]
               [per2 (person-set per1)])
          (check-cache-size (list 3))
          (check-not-eq? per1 per2)
          (check-false (guid=? per1 per2))
          (check-equal? per1 per2)
          (check-eq? (pet-owner pet1) per1)
          (check guid=? (pet-owner pet1) per1)
          (check-equal? (pet-owner pet1) per1)
          (check-not-eq? (pet-owner pet1) per2)
          (check-false (guid=? (pet-owner pet1) per2))
          (check-equal? (pet-owner pet1) per2)))
      
      (test-case "create both, modify one"
        (recreate-test-tables)
        (cache-clear!)
        (let* ([per1 (make-person "Lyman")]
               [pet1 (make-pet per1 "Odie")]
               [per2 (person-set per1 #:name "Jon")])
          (check-cache-size (list 3))
          (check-not-eq? per1 per2)
          (check-false (guid=? per1 per2))
          (check-not-equal? per1 per2)
          (check-eq? (pet-owner pet1) per1)
          (check guid=? (pet-owner pet1) per1)
          (check-equal? (pet-owner pet1) per1)
          (check-not-eq? (pet-owner pet1) per2)
          (check-false (guid=? (pet-owner pet1) per2))
          (check-not-equal? (pet-owner pet1) per2)))
      
      (test-case "load, copy one"
        (recreate-test-tables)
        (let ([per1 (save! (make-person "Jon"))])
          (save! (make-pet per1 "Garfield")))
        (cache-clear!)
        (let* ([pet1 (select-one #:from pet)]
               [per1 (pet-owner pet1)]
               [per2 (person-set per1)])
          (check-cache-size (list 3))
          (check-not-eq? per1 per2)
          (check-false (guid=? per1 per2))
          (check-equal? per1 per2)
          (check-eq? (pet-owner pet1) per1)
          (check guid=? (pet-owner pet1) per1)
          (check-equal? (pet-owner pet1) per1)
          (check-not-eq? (pet-owner pet1) per2)
          (check-false (guid=? (pet-owner pet1) per2))
          (check-equal? (pet-owner pet1) per2)))
      
      (test-case "load, modify one"
        (recreate-test-tables)
        (let ([per1 (save! (make-person "Lyman"))])
          (save! (make-pet per1 "Odie")))
        (cache-clear!)
        (let* ([pet1 (select-one #:from pet)]
               [per1 (pet-owner pet1)]
               [per2 (person-set per1 #:name "Jon")])
          (check-cache-size (list 3))
          (check-not-eq? per1 per2)
          (check-false (guid=? per1 per2))
          (check-not-equal? per1 per2)
          (check-eq? (pet-owner pet1) per1)
          (check guid=? (pet-owner pet1) per1)
          (check-equal? (pet-owner pet1) per1)
          (check-not-eq? (pet-owner pet1) per2)
          (check-false (guid=? (pet-owner pet1) per2))
          (check-not-equal? (pet-owner pet1) per2)))
      
      (test-case "create both, copy one, save both"
        (recreate-test-tables)
        (cache-clear!)
        (let* ([per1 (make-person "Jon")]
               [pet1 (make-pet per1 "Garfield")]
               [per2 (person-set per1)])
          ; After the following line:
          ;
          ;   - per2 and per1 will be different people in the database;
          ;   - pet1 will be pointing to per1.
          ;
          ; The programmer has probably made a mistake if they write
          ; code like this. They might expect per2 and per1 to refer to
          ; the same record, but these are unsaved structs: a new ID will
          ; be allocated for each.
          (for-each save! (list per2 per1 pet1))
          (check-cache-size (list 3))
          (check-not-eq? per1 per2)
          (check-false (guid=? per1 per2))
          (check-not-equal? per1 per2)
          (check-eq? (pet-owner pet1) per1)
          (check guid=? (pet-owner pet1) per1)
          (check-equal? (pet-owner pet1) per1)
          (check-not-eq? (pet-owner pet1) per2)
          (check-false (guid=? (pet-owner pet1) per2))
          (check-not-equal? (pet-owner pet1) per2)))

      )))

; Provide statements -----------------------------

(provide snooze-cache-tests)
