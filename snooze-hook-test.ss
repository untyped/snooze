#lang scheme/base

(require "test-base.ss")

(require "snooze-api.ss"
         "test-data.ss"
         "era/era.ss"
         "sql/sql.ss")

; Helpers ----------------------------------------

; When a hook stage is run successfully (i.e. without raising an exception),
; the struct is stored in the relevant box:
(define saved    (box #f)) ; (box (U hooked #f))
(define deleted  (box #f)) ; (box (U hooked #f))

; Convenience procedure for clearing boxes:
(define (clear-boxes)
  (set-box! saved    #f)
  (set-box! deleted  #f))

; Custom exception type (to avoid confusion with anything outside this file):
(define-struct (exn:unhooked exn) ())

; Each stage checks the "value" field of the struct being saved/deleted.
; If the value matches a "bad value", an exception is thrown. The tests
; below seed the struct with different values to make different stages fail.
(define (create-hook name box bad-value)
  (lambda (continue conn struct)
    (if (= (hooked-value struct) bad-value)
        (raise-exn exn:unhooked "Argh!")
        (begin (set-box! box struct)
               (continue conn struct)))))

; Hey! It's a test entity:
(define-entity hooked
  ([value integer])
  #:on-save   (create-hook 'save   saved   1)
  #:on-delete (create-hook 'delete deleted 4))

; ...and a persistent struct, too:
(define test-hooked #f)

; Tests ------------------------------------------

; test-suite
(define snooze-hook-tests
  (test-suite "snooze-hook-tests"
    
    ; ***** NOTE *****
    ; Each test below depends on the tests before it.
    ; Add/edit tests at your peril!
    ; ****************
    
    ; create table for entity:hooked
    #:before
    (lambda ()
      (drop-all-tables)
      (unless (table-exists? hooked)
        (create-table hooked))
      (set! test-hooked (make-hooked 0)))
    
    ; drop table for entity:hooked
    #:after
    drop-all-tables
    
    (test-case "on-save is called when saving a new struct"
      (set-hooked-value! test-hooked 0)
      (save! test-hooked)
      ; Check which hooks were run successfully:
      (check-eq? (unbox saved)    test-hooked)
      (check-eq? (unbox deleted)  #f)
      (clear-boxes))
    
    (test-case "on-delete is called on delete"
      (set-hooked-value! test-hooked 0)
      (delete! test-hooked)
      ; Check which hooks were run successfully:
      (check-eq? (unbox saved)    #f)
      (check-eq? (unbox deleted)  test-hooked)
      (clear-boxes))
    
    (test-case "saving is aborted when on-save throws an exception"
      (set-hooked-value! test-hooked 1)
      (debug "test-hooked" test-hooked)
      (check-false (struct-saved? test-hooked))
      (check-exn exn:unhooked? (lambda () (save! test-hooked)))
      (check-false (struct-saved? test-hooked))
      (check-pred null? (find-all (sql (select #:from hooked))))
      ; Check which hooks were run successfully:
      (check-eq? (unbox saved)    #f)
      (check-eq? (unbox deleted)  #f))
    
    (test-case "deleting is aborted when on-delete throws an exception"
      (set-hooked-value! test-hooked 4)
      (save! test-hooked)
      (clear-boxes)
      (check-exn exn:unhooked? (lambda () (delete! test-hooked)))
      (check-equal?
       (hooked-value
        (find-one
         (sql (select #:from  hooked
                      #:where (= hooked.guid ,test-hooked)))))
       4)
      ; Check which hooks were run successfully:
      (check-eq? (unbox saved)    #f)
      (check-eq? (unbox deleted)  #f)
      (clear-boxes))))

; Provide statements -----------------------------

(provide snooze-hook-tests)
