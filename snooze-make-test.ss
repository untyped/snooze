#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "era/era.ss"
         (prefix-in real: "era/snooze-struct.ss"))

; Tests -------------------------------------------

; test-suite
(define snooze-make-tests
  (test-suite "snooze-make-tests"
    
    (test-case "make-person : returns a local guid"
      (recreate-test-tables/cache)
      (let* ([per     (make-person "Per")]
             [struct  (send (current-cache) cache-ref/local per)]
             [vanilla (send (current-cache) get-vanilla-guid per)])
        (check-pred guid-local? per)
        (check-false vanilla)
        (check-pred (entity-private-predicate person) struct)
        (check-false (real:struct-guid struct))))
    
    (test-case "make-person, and save: creates a vanilla GUID and a local GUID, distinct from first."
      (recreate-test-tables/cache)
      (let* ([per      (make-person "Per")]
             [struct   (send (current-cache) cache-ref/local per)]
             [vanilla  (send (current-cache) get-vanilla-guid per)]
             [per2     (save! per)]
             [struct2  (send (current-cache) cache-ref/local per2)]
             [vanilla2 (send (current-cache) get-vanilla-guid per2)])
        (check-pred guid-local? per)
        (check-false vanilla)
        (check-pred (entity-private-predicate person) struct)
        (check-false (real:struct-guid struct))
        (check-true (struct-eq? per per2)) ; refer to exactly the same struct
        (check-false (eq? per per2))       ; but the guids are not themselves equal
        (check-not-false vanilla2)))))     ; vanilla2 should be defined

; Provide statements -----------------------------

(provide snooze-make-tests)