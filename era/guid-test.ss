#lang scheme/base
  
(require "../test-base.ss")

(require "../snooze-api.ss"
         "../test-data.ss"
         "era.ss")

; Tests -------------------------------------------

; test-suite
(define guid-tests
  (test-suite "guid"
    
    #:before
    recreate-test-tables
    
    #:after
    drop-all-tables
    
    (test-case "guid-id, guid-serial"
      (let/debug ([person1 (make-person "Dave")])
        
        ; New struct: id = #f, serial <> #f
        (check-false     (guid-id     person1))
        (check-not-false (guid-serial person1))
        
        (let/debug ([person2 (save! person1)])
        
          ; Saved struct: id <> #f, serial = #f
          (check-not-false (guid-id     person1))
          (check-false     (guid-serial person1))
          (check-not-false (guid-id     person2))
          (check-false     (guid-serial person2))
          
          (let/debug ([person3 (delete! person2)])
        
            ; Deleted struct: id = #f, serial <> #f
            (check-false     (guid-id     person1))
            (check-not-false (guid-serial person1))
            (check-not-false (guid-id     person2))
            (check-false     (guid-serial person2))
            (check-false     (guid-id     person3))
            (check-not-false (guid-serial person3))))))))

; Provide statements -----------------------------

(provide guid-tests)
