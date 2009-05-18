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
      (let ([guid (entity-make-vanilla-guid person 123)])
        (check-equal? (guid-id guid) 123)
        (check-false  (guid-serial guid)))
      
      (let ([guid (entity-make-local-guid person)])
        (check-false     (guid-id     guid))
        (check-not-false (guid-serial guid))))))

; Provide statements -----------------------------

(provide guid-tests)
