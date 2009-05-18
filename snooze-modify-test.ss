#lang scheme/base

(require srfi/26
         "snooze-api.ss"
         "test-base.ss"
         "era/era.ss")

; Tests ------------------------------------------
; This only tests the functional update process and its effect on structs.


(define snooze-modify-tests
  (test-suite "snooze-modify-tests: (foo-set foo1 ...)"
    
    #:before recreate-test-tables
    
    #:after drop-all-tables
   
    (test-case "copying a struct creates an independent copy, unsaved, with the same vanilla guid"
      (let* ([per      (save! (make-person "Per"))]                   ; saved
             [struct   (send (current-cache) cache-ref/local per)]
             [vanilla  (send (current-cache) get-vanilla-guid per)]
             [per2     (person-set per #:name "Per2")]                ; unsaved
             [struct2  (send (current-cache) cache-ref/local per2)]
             [vanilla2 (send (current-cache) get-vanilla-guid per2)])
        (check-true (guid-local? per))
        (check-true (guid-local? per2))
        (check-true (eq? vanilla vanilla2))   ; same vanilla GUID
        (check-false (struct-eq? per per2)))) ; different structs in memory
    
    (test-case "saving a copy of a struct updates the original struct, so that they refer to the same struct"
      (let* ([per      (save! (make-person "Per"))]                   ; saved
             [per2     (save! (person-set per #:name "Per2"))]        ; saved
             [struct   (send (current-cache) cache-ref/local per)]
             [vanilla  (send (current-cache) get-vanilla-guid per)]
             [struct2  (send (current-cache) cache-ref/local per2)]
             [vanilla2 (send (current-cache) get-vanilla-guid per2)])
        (check-true (guid-local? per))
        (check-true (guid-local? per2))
        (check-true (eq? vanilla vanilla2))  ; same vanilla GUID
        (check-true (struct-eq? per per2)))) ; different structs in memory
    ))





; course
(define c1 #f)

; test-suite
#;(define snooze-modify-tests
    (test-suite "snooze-modify-tests"
      
      #:before
      (lambda ()
        (recreate-test-tables)
        (set! c1 (make-course 'code "Name" 12345 1234.5 #t (string->time-tai "2001-01-01 01:01:01"))))
      
      #:after
      drop-all-tables
      
      ; ***** NOTE *****
      ; Each test below depends on the tests before it.
      ; Add/edit tests at your peril!
      ; ****************
      
      (test-case "new struct has #f id"
        (check-false (struct-id c1)))
      
      (test-case "first save! inserts a database record, updates id in struct, and returns the struct"
        (let ([return-value (save! c1)])
          (check-pred integer? (struct-id c1))
          ; (check-equal? (find-by-id course (struct-id c1)) c1)
          (fail "need way of bypassing cache")
          (check-equal? return-value c1)))
      
      (test-case "subsequent save! updates database record and returns the struct id"
        (set-course-value! c1 54321)
        (let ([return-value (save! c1)])
          ;(check-equal? (course-value (find-by-id course (struct-id c1))) 54321)
          (fail "need way of bypassing cache")
          (check-equal? return-value c1)))
      
      (test-case "delete! removes database record and sets struct id to #f"
        (let ([old-id (struct-id c1)])
          (delete! c1)
          (check-false (struct-id c1))
          ;(check-false (find-by-id course old-id))
          (fail "need way of bypassing cache")))
      
      (test-case "delete! of unsaved record raises exception"
        (check-exn exn:fail? (cut delete! c1)))))

; Provide statements -----------------------------

(provide snooze-modify-tests)
