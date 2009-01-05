#lang scheme/base

(require mzlib/etc
         srfi/26/cut)

(require "snooze-syntax.ss"
         "test-base.ss"
         "test-data.ss"
         "test-util.ss"
         "era/era.ss")

(provide make-snooze-modify-tests)
  
; Tests ------------------------------------------

; snooze% -> test-suite
(define (make-snooze-modify-tests snooze)
  (define-snooze-interface snooze)
  
  ; course
  (define course (make-course 'code "Name" 12345 1234.5 #t (string->time-tai "2001-01-01 01:01:01")))
  
  ; test-suite
  (test-suite "snooze-modify-tests"
    
    #:before
    (lambda ()
      (create-table entity:course)
      (create-table entity:person))
    
    #:after
    (lambda ()
      (drop-table entity:course)
      (drop-table entity:person))
    
    ; ***** NOTE *****
    ; Each test below depends on the tests before it.
    ; Add/edit tests at your peril!
    ; ****************
    
    (test-case "new struct has #f id"
      (check-false (struct-id course)))
    
    (test-case "first save! inserts a database record, updates id in struct, and returns the struct"
      (let ([return-value (save! course)])
        (check-pred integer? (struct-id course))
        (check-equal? (find-by-id entity:course (struct-id course)) course)
        (check-equal? return-value course)))
    
    (test-case "subsequent save! updates database record and returns the struct id"
      (set-course-value! course 54321)
      (let ([return-value (save! course)])
        (check-equal? (course-value (find-by-id entity:course (struct-id course))) 54321)
        (check-equal? return-value course)))
    
    (test-case "delete! removes database record and sets struct id to #f"
      (let ([old-id (struct-id course)])
        (delete! course)
        (check-false (struct-id course))
        (check-false (find-by-id entity:course old-id))))
    
    (test-case "delete! of unsaved record raises exception"
      (check-exn exn:fail:snooze? (cut delete! course)))
    
    ;(test-case "save/id+revision!"
    ;  (begin-with-definitions
    ;    (save/id+revision!
    ;     (make-person/defaults #:id       10000
    ;                           #:revision 20000
    ;                           #:name     "Dave"))
    ;    ; person
    ;    (define found (find-by-id entity:person 10000))
    ;    (check-pred person? found "predicate")
    ;    (check-equal? (person-id found) 10000 "id")
    ;    (check-equal? (person-revision found) 20000 "revision")
    ;    (check-equal? (person-name found) "Dave" "name")
    ;    (delete! found)))
    
    ))
