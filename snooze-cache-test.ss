#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "test-data.ss"
         "snooze-cache-test-util.ss"
         "era/era.ss")

; Tests -------------------------------------------

(define-syntax-rule (check-equality a b eq guid= equal)
  (begin
    (with-check-info (['actual a] ['expected b] ['comparison 'eq?])
      (if eq
          (check-true (eq? a b))
          (check-false (eq? a b))))
    
    (with-check-info (['actual a] ['expected b] ['comparison 'guid=?])
      (if guid=
          (check-true (guid=? a b))
          (check-false (guid=? a b))))
    
    (with-check-info (['actual a] ['expected b] ['comparison 'equal?])
      (if equal
          (check-true (equal? a b))
          (check-false (equal? a b))))))

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
          (check-equality per1 per2 #f #f #t)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "Dave")))
      
      (test-case "load"
        (recreate-test-tables)
        (save! (make-person "Dave"))
        (cache-clear!)
        (let ([per1 (select-one #:from person)]
              [per2 (select-one #:from person)])
          (check-cache-size (list 1))
          (check-equality per1 per2 #f #t #t)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "Dave")))
      
      (test-case "create, copy"
        (recreate-test-tables)
        (cache-clear!)
        (let ([per1 (make-person "Dave")]
              [per2 (person-set (make-person "Dave"))])
          (check-cache-size (list 3))
          (check-equality per1 per2 #f #f #t)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "Dave")))
      
      (test-case "load, copy"
        (recreate-test-tables)
        (save! (make-person "Dave"))
        (cache-clear!)
        (let ([per1 (select-one #:from person)]
              [per2 (person-set (select-one #:from person))])
          (check-cache-size (list 2))
          (check-equality per1 per2 #f #f #t)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "Dave")))
      
      (test-case "create, modify"
        (recreate-test-tables)
        (cache-clear!)
        (let* ([per1 (make-person "Dave")]
               [per2 (person-set per1 #:name "David")])
          (check-cache-size (list 2))
          (check-equality per1 per2 #f #f #f)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "David")))
      
      (test-case "load, modify"
        (recreate-test-tables)
        (save! (make-person "Dave"))
        (cache-clear!)
        (let* ([per1 (select-one #:from person)]
               [per2 (person-set per1 #:name "David")])
          (check-cache-size (list 2))
          (check-equality per1 per2 #f #f #f)
          (check-equal? (person-name per1) "Dave")
          (check-equal? (person-name per2) "David")))
      
      (test-case "create, modify, save"
        (recreate-test-tables)
        (cache-clear!)
        (let* ([per1 (make-person "Dave")]
               [per2 (person-set per1 #:name "David")]
               [per3 (save! per2)])
          (check-cache-size (list 2))
          (check-equality per1 per3 #f #f #f)
          (check-equality per2 per3 #t #t #t)
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
          (check-cache-size (list 1))
          (check-equality per1 per3 #f #t #t)
          (check-equality per2 per3 #t #t #t)
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
          (check-equality per1 pet1 #f #f #f)
          (check-equality (pet-owner pet1) per1 #t #t #t)
          (check-equal? (person-name per1) "Jon")
          (check-equal? (pet-name pet1) "Garfield")))
      
      (test-case "load both"
        (recreate-test-tables)
        (let ([per1 (save! (make-person "Jon"))])
          (save! (make-pet per1 "Garfield")))
        (cache-clear!)
        (let* ([per1 (select-one #:from person)]
               [pet1 (select-one #:from pet)])
          (check-cache-size (list 2))
          (check-equality per1 pet1 #f #f #f)
          (check-equality (pet-owner pet1) per1 #f #t #t)
          (check-equal? (person-name per1) "Jon")
          (check-equal? (pet-name pet1) "Garfield")))
      
      (test-case "load one, implicitly load other"
        (recreate-test-tables)
        (let ([per1 (save! (make-person "Jon"))])
          (save! (make-pet per1 "Garfield")))
        (cache-clear!)
        (let* ([pet1 (select-one #:from pet)]
               [per1 (pet-owner pet1)])
          (check-cache-size (list 1))
          (check-equality per1 pet1 #f #f #f)
          (check-equality (pet-owner pet1) per1 #t #t #t)
          (check-cache-size (list 1))
          (check-equal? (person-name per1) "Jon")
          (check-cache-size (list 2))
          (check-equal? (pet-name pet1) "Garfield")))
      
      (test-case "create both, copy one"
        (recreate-test-tables)
        (cache-clear!)
        (let* ([per1 (make-person "Jon")]
               [pet1 (make-pet per1 "Garfield")]
               [per2 (person-set per1)])
          (check-cache-size (list 3))
          (check-equality per1 per2 #f #f #t)
          (check-equality (pet-owner pet1) per1 #t #t #t)
          (check-equality (pet-owner pet1) per2 #f #f #t)
          (check-equal? (person-name per1) "Jon")
          (check-equal? (person-name per2) "Jon")
          (check-equal? (pet-name pet1) "Garfield")))
      
      (test-case "create both, modify one"
        (recreate-test-tables)
        (cache-clear!)
        (let* ([per1 (make-person "Lyman")]
               [pet1 (make-pet per1 "Odie")]
               [per2 (person-set per1 #:name "Jon")])
          (check-cache-size (list 3))
          (check-equality per1 per2 #f #f #f)
          (check-equality (pet-owner pet1) per1 #t #t #t)
          (check-equality (pet-owner pet1) per2 #f #f #f)
          (check-equal? (person-name per1) "Lyman")
          (check-equal? (person-name per2) "Jon")
          (check-equal? (pet-name pet1) "Odie")))
      
      (test-case "load, copy one"
        (recreate-test-tables)
        (let ([per1 (save! (make-person "Jon"))])
          (save! (make-pet per1 "Garfield")))
        (cache-clear!)
        (let* ([pet1 (select-one #:from pet)]
               [per1 (pet-owner pet1)]
               [per2 (person-set per1)])
          (check-cache-size (list 3))
          (check-equality per1 per2 #f #f #t)
          (check-equality (pet-owner pet1) per1 #t #t #t)
          (check-equality (pet-owner pet1) per2 #f #f #t)
          (check-equal? (person-name per1) "Jon")
          (check-equal? (pet-name pet1) "Garfield")))
      
      (test-case "load, modify one"
        (recreate-test-tables)
        (let ([per1 (save! (make-person "Lyman"))])
          (save! (make-pet per1 "Odie")))
        (cache-clear!)
        (let* ([pet1 (select-one #:from pet)]
               [per1 (pet-owner pet1)]
               [per2 (person-set per1 #:name "Jon")])
          (check-cache-size (list 3))
          (check-equality per1 per2 #f #f #f)
          (check-equality (pet-owner pet1) per1 #t #t #t)
          (check-equality (pet-owner pet1) per2 #f #f #f)
          (check-equal? (person-name per1) "Lyman")
          (check-equal? (person-name per2) "Jon")
          (check-equal? (pet-name pet1) "Odie")))
      
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
          (check-equality per1 per2 #f #f #f)
          (check-equality (pet-owner pet1) per1 #t #t #t)
          (check-equality (pet-owner pet1) per2 #f #f #f)
          (check-equal? (person-name per1) "Jon")
          (check-equal? (pet-name pet1) "Garfield")))

      (test-case "create both, modify one, save both"
        (recreate-test-tables)
        (cache-clear!)
        (let* ([per1 (make-person "Lyman")]
               [pet1 (make-pet per1 "Odie")]
               [per2 (person-set per1 #:name "Jon")])
          (for-each save! (list per2 per1 pet1))
          (check-cache-size (list 3))
          (check-equality per1 per2 #f #f #f)
          (check-equality (pet-owner pet1) per1 #t #t #t)
          (check-equality (pet-owner pet1) per2 #f #f #f)
          (check-equal? (person-name per1) "Lyman")
          (check-equal? (person-name per2) "Jon")
          (check-equal? (pet-name pet1) "Odie")))
      
      (test-case "load both, copy one, save both"
        (recreate-test-tables)
        (let ([per1 (save! (make-person "Jon"))])
          (save! (make-pet per1 "Garfield")))
        (cache-clear!)
        (let* ([pet1 (select-one #:from pet)]
               [per1 (pet-owner pet1)]
               [per2 (person-set per1)])
          (check-cache-size (list 3))
          (for-each save! (list per2 pet1))
          (check-cache-size (list 2))
          (check-equality per1 per2 #f #t #t)
          (check-equality (pet-owner pet1) per1 #t #t #t)
          (check-equality (pet-owner pet1) per2 #f #t #t)
          (check-equal? (person-name per1) "Jon")
          (check-equal? (pet-name pet1) "Garfield")))

      (test-case "load both, modify one, save both"
        (recreate-test-tables)
        (let ([per1 (save! (make-person "Lyman"))])
          (save! (make-pet per1 "Odie")))
        (cache-clear!)
        (let* ([pet1 (select-one #:from pet)]
               [per1 (pet-owner pet1)]
               [per2 (person-set per1 #:name "Jon")])
          (check-cache-size (list 3))
          (for-each save! (list per2 pet1))
          (check-cache-size (list 2))
          (check-equality per1 per2 #f #t #t)
          (check-equality (pet-owner pet1) per1 #t #t #t)
          (check-equality (pet-owner pet1) per2 #f #t #t)
          (check-equal? (person-name per1) "Jon")
          (check-equal? (person-name per2) "Jon")
          (check-equal? (pet-name pet1) "Odie")))
      
      )))

; Provide statements -----------------------------

(provide snooze-cache-tests)
