#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         srfi/19
         (unlib-in hash time)
         "snooze-api.ss"
         "core/core.ss"
         (prefix-in real: "core/snooze-struct.ss"))

; Tests -------------------------------------------

; test-suite
(define snooze-save-tests
  (test-suite "snooze-save-tests"
    
    (test-case "save! : affects snooze-struct-saved?, temporary-guid? and database-guid?"
      (recreate-test-tables)
      (let* ([per1 (make-person "Dave")]
             [per2 (save! per1)])
        (collect-garbage)
        (check-pred  temporary-guid? (snooze-struct-guid per1))
        (check-false (snooze-struct-saved? per1))
        (check-pred  database-guid? (snooze-struct-guid per2))
        (check-pred  snooze-struct-saved? per2)))
    
    (test-case "save!, person-set : remaps guids appropriately"
      (recreate-test-tables)
      (let* ([per1a   (make-person "Dave")]
             [per1b   (person-set per1a #:name "Noel")]
             [per2a   (save! per1b)]
             [per2b   (person-set per2a #:name "Matt")])
        (check-eq? per1a per1b)
        (check-eq? per1b per2a)
        (check-eq? per2a per2b)
        (check-equal? (person-name per1a) "Dave")
        (check-equal? (person-name per1b) "Noel")
        (check-equal? (person-name per2a) "Noel")
        (check-equal? (person-name per2b) "Matt")))
    
    (test-case "save! : stores information correctly in the database"
      (recreate-test-tables)
      (let* ([per1 (save! (make-person "Dave"))]
             [per2 (save! (make-person "Dave"))]
             [per3 (save! per1)]
             [per4 (save! (make-person "Noel"))])
        (check-equal? (direct-query "select guid,name from person order by guid asc;")
                      (list (list (snooze-struct-id per1) "Dave")
                            (list (snooze-struct-id per2) "Dave")
                            (list (snooze-struct-id per4) "Noel")))))
    
    (test-case "save! : consecutive saves"
      (recreate-test-tables)
      (let* ([per1 (save! (make-person "Jon"))]
             [per2 (save! (person-set per1 #:name "Lyman"))]
             [per3 (save! (make-person "Liz"))]
             [per4 (save! (make-person "Liz"))])
        (check-equal? (direct-query "select count(guid) from person where name = 'Jon';") (list (list 0)))
        (check-equal? (direct-query "select count(guid) from person where name = 'Lyman';") (list (list 1)))
        (check-equal? (direct-query "select count(guid) from person where name = 'Liz';") (list (list 2)))))
    
    (test-case "save! : bad data types"
      (recreate-test-tables)
      (check-exn exn:fail:snooze? (cut save! (make-person 'R2D2)))
      (let ([pet1 (save! (make-pet #f "Garfield"))])
        (check-exn exn:fail:snooze? (cut save! (make-person pet1 "Odie")))))
    
    (test-case "save! : revision incremented on save"
      (recreate-test-tables)
      (let ([per0 (make-person "Dave")])
        (check-false (snooze-struct-revision per0))
        (let ([per1 (save! per0)])
          (check-equal? (snooze-struct-revision per1) 0)
          (check-equal? (snooze-struct-revision per0) 0)
          (let ([per2 (save! (person-set per1 #:name "Noel"))])
            (check-equal? (snooze-struct-revision per2) 1)
            (check-equal? (snooze-struct-revision per1) 0)
            (check-equal? (snooze-struct-revision per0) 0))))
      (check-equal? (direct-query "select count(guid) from person where revision = 0;") (list (list 0)))
      (check-equal? (direct-query "select count(guid) from person where revision = 1;") (list (list 1))))
    
    (test-case "save! : cannot save an out-of-date struct"
      (recreate-test-tables)
      (let* ([per1 (save! (make-person "Dave"))]
             [per2 (save! (person-set per1 #:name "Noel"))])
        (check-exn exn:fail:snooze? (cut save! (person-set per1 #:name "Matt")))))
    
    (test-case "save! : cannot save a struct with local-only foreign keys"
      (recreate-test-tables)
      (let* ([per (save! (make-person "Jon"))]
             [pet (make-pet per "Garfield")])
        (check-not-exn (cut save! pet)))
      
      (recreate-test-tables)
      (let* ([per (save! (make-person "Jon"))]
             [pet1 (save! (make-pet per "Garfield"))]
             [pet2 (save! (make-pet (pet-owner pet1) "Garfield"))])
        (check-not-exn (cut save! pet2)))
      
      (recreate-test-tables)
      (let* ([per (make-person "Jon")]
             [pet (make-pet per "Garfield")])
        (check-exn exn:fail:snooze? (cut save! pet)))
      
      (recreate-test-tables)
      (let* ([per0 (make-person "Jon")]
             [per1 (save! per0)]
             [per2 (person-set per1 #:name "Lyman")]
             [pet  (make-pet per2 "Garfield")])
        (check-exn exn:fail:snooze? (cut save! pet))))
    
    (test-case "save! : data serialized/deserialized successfuly"
      (let* ([struct1 (save! (make-course 'COURSE1 "Course 1" 1 0.1 #t (date->time-tai (make-date 0 0 0 12 01 01 2009 3600)) '(a b c)))]
             [vals1   (snooze-struct-data-ref* struct1)]
             [id1     (snooze-struct-id struct1)]
             [struct2 (find-by-id course id1)]
             [vals2   (snooze-struct-data-ref* struct2)]
             [attrs   (entity-data-attributes course)])
        (for ([attr (in-list attrs)]
              [val1 (in-list vals1)]
              [val2 (in-list vals2)])
          (check-equal? val1 val2 (format "attribute failed: ~a" (attribute-name attr))))))))

; Provide statements -----------------------------

(provide snooze-save-tests)
