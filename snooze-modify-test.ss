#lang scheme/base

(require srfi/26
         "snooze-api.ss"
         "test-base.ss"
         "era/era.ss")

; Tests ------------------------------------------

; This tests the functional update process and its effect on structs.

(define snooze-modify-tests
  (test-suite "snooze-modify-tests"
    
    (test-case "copying a saved struct : creates an independent copy, unsaved, with the same vanilla guid"
      (recreate-test-tables/cache)
      (let* ([per      (save! (make-person "Per"))]                   ; saved
             [struct   (send (current-cache) cache-ref/local per)]
             [vanilla  (send (current-cache) get-vanilla-guid per)]
             [per2     (person-set per #:name "Per2")]                ; unsaved
             [struct2  (send (current-cache) cache-ref/local per2)]
             [vanilla2 (send (current-cache) get-vanilla-guid per2)])
        (check-true (guid-local? per))
        (check-true (guid-local? per2))
        (check-pred interned-guid? vanilla)
        (check-pred interned-guid? vanilla2)
        (check-true (eq? vanilla vanilla2))   ; same vanilla GUID
        (check-false (snooze-struct-eq? per per2))   ; different structs in memory
        ))
    
    (test-case "saving a copy of a struct updates the original struct, so that they refer to the same struct"
      (recreate-test-tables/cache)
      (let* ([per      (save! (make-person "Per"))]                   ; saved
             [per2     (save! (person-set per #:name "Per2"))]        ; saved
             [struct   (send (current-cache) cache-ref/local per)]
             [vanilla  (send (current-cache) get-vanilla-guid per)]
             [struct2  (send (current-cache) cache-ref/local per2)]
             [vanilla2 (send (current-cache) get-vanilla-guid per2)])
        (check-equal? (person-name per)  "Per")
        (check-equal? (person-name per2) "Per2")
        (check-true  (guid-local? per))
        (check-true  (guid-local? per2))
        (check-pred  interned-guid? vanilla)
        (check-pred  interned-guid? vanilla2)
        (check-true  (eq? vanilla vanilla2))  ; same vanilla GUID
        (check-false (snooze-struct-eq? per per2)))) ; different structs in memory
    
    (test-case "copying a unsaved struct : cache sizes are correct"
      (recreate-test-tables/cache)
      (with-cache
       (let* ([per  (begin0 (make-person "Per")
                            (collect-garbage)
                            (check-cache-size (list 1 0)))]
              [per2 (begin0 (person-set per #:name "Per2")
                            (collect-garbage)
                            (check-cache-size (list 2 0)))])
         (list per per2)))) ; safe for space
    
    (test-case "copying a saved struct : cache sizes are correct"
      (recreate-test-tables/cache)
      (with-cache
       (let* ([per  (begin0 (save! (make-person "Per"))
                            (collect-garbage)
                            (check-cache-size (list 2 1)))]
              [per2 (begin0 (person-set per #:name "Per2")
                            (collect-garbage)
                            (check-cache-size (list 3 1)))])
         (list per per2)))))) ; safe for space

; Provide statements -----------------------------

(provide snooze-modify-tests)
