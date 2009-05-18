#lang scheme/base
  
(require "../test-base.ss")

(require "../snooze-api.ss"
         "era.ss")

; Tests -------------------------------------------

; test-suite
(define guid-tests
  (test-suite "guid"
    
    (test-case "guid-id, guid-serial"
      (recreate-test-tables/cache)
      (let ([person1 (make-person "Dave")])
        
        ; New struct: id = #f, serial <> #f
        (check-false     (guid-id       person1))
        (check-not-false (guid-serial   person1))
        (check-false     (struct-saved? person1))
        
        (let ([person2 (save! person1)])
        
          ; Saved struct: id <> #f, serial = #f
          (check-false     (guid-id       person1))
          (check-false     (guid-id       person2))
          (check-not-false (guid-serial   person1))
          (check-not-false (guid-serial   person2))
          (check-true      (struct-saved? person1))
          (check-true      (struct-saved? person2))
          
          (let ([person3 (delete! person2)])
        
            ; Deleted struct: id = #f, serial <> #f
            (check-false     (guid-id       person1))
            (check-false     (guid-id       person2))
            (check-false     (guid-id       person3))
            (check-not-false (guid-serial   person1))
            (check-not-false (guid-serial   person2))
            (check-not-false (guid-serial   person3))
            (check-false     (struct-saved? person1))
            (check-false     (struct-saved? person2))
            (check-false     (struct-saved? person3))))))))

; Provide statements -----------------------------

(provide guid-tests)
