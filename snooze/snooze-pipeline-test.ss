#lang scheme/base

(require srfi/26/cut
         (planet untyped/unlib:3/pipeline)
         "snooze.ss"
         "test-base.ss"
         "test-data.ss")

; Helpers ----------------------------------------

; When a pipeline stage is run successfully (i.e. without raising an exception),
; the struct is stored in the relevant box:
(define saved (box #f))    ; (box (U pipelined #f))
(define inserted (box #f)) ; (box (U pipelined #f))
(define updated (box #f))  ; (box (U pipelined #f))
(define deleted (box #f))  ; (box (U pipelined #f))

; Convenience procedure for clearing boxes:
(define (clear-boxes)
  (set-box! saved #f)
  (set-box! inserted #f)
  (set-box! updated #f)
  (set-box! deleted #f))

; Custom exception type (to avoid confusion with anything outside this file):
(define-struct (exn:unpipelined exn) ())

; Each stage checks the "value" field of the struct being saved/deleted.
; If the value matches a "bad value", an exception is thrown. The tests
; below seed the struct with different values to make different stages fail.
(define (create-stage name box bad-value)
  (lambda (continue conn struct)
    (if (= (pipelined-value struct) bad-value)
        (raise-exn exn:unpipelined "Argh!")
        (begin (set-box! box struct)
               (continue conn struct)))))

; Hey! It's a test entity:
(define-persistent-struct pipelined
  ([value  type:integer])
  #:on-save   (list (create-stage 'save   saved    1))
  #:on-insert (list (create-stage 'insert inserted 2))
  #:on-update (list (create-stage 'update updated  3))
  #:on-delete (list (create-stage 'delete deleted  4)))

; ...and a persistent struct, too:
(define test-pipelined (make-pipelined 0))

; Tests ------------------------------------------

; snooze -> test-suite
(define (make-snooze-pipeline-tests snooze)
  (define-snooze-interface snooze)
  
  ; test-suite
  (test-suite "snooze-pipeline-tests"
    
    ; ***** NOTE *****
    ; Each test below depends on the tests before it.
    ; Add/edit tests at your peril!
    ; ****************
    
    ; create table for entity:pipelined
    #:before
    (lambda ()
      (create-table entity:pipelined))
    
    ; drop table for entity:pipelined
    #:after
    (lambda ()
      (drop-table entity:pipelined))
    
    (test-case "on-save and on-insert are called when saving a new struct"
      (set-pipelined-value! test-pipelined 0)
      (save! test-pipelined)
      ; Check which pipelines were run successfully:
      (check-eq? (unbox saved)    test-pipelined)
      (check-eq? (unbox inserted) test-pipelined)
      (check-eq? (unbox updated)  #f)
      (check-eq? (unbox deleted)  #f)
      (clear-boxes))
    
    (test-case "on-save and on-update are called when re-saving a struct"
      (set-pipelined-value! test-pipelined 0)
      (save! test-pipelined)
      ; Check which pipelines were run successfully:
      (check-eq? (unbox saved)    test-pipelined)
      (check-eq? (unbox inserted) #f)
      (check-eq? (unbox updated)  test-pipelined)
      (check-eq? (unbox deleted)  #f)
      (clear-boxes))
    
    (test-case "on-delete is called on delete"
      (set-pipelined-value! test-pipelined 0)
      (delete! test-pipelined)
      ; Check which pipelines were run successfully:
      (check-eq? (unbox saved)    #f)
      (check-eq? (unbox inserted) #f)
      (check-eq? (unbox updated)  #f)
      (check-eq? (unbox deleted)  test-pipelined)
      (clear-boxes))
    
    (test-case "saving is aborted when on-save throws an exception"
      (set-pipelined-value! test-pipelined 1)
      (check-exn exn:unpipelined? 
        (lambda ()
          (save! test-pipelined))
        "check 2")
      (check-false (struct-id test-pipelined)
                   "check 4")
      (check-pred null?
                  (let-alias ([a pipelined])
                    (find-all (sql:select #:from a)))
                  "check 5")
      ; Check which pipelines were run successfully:
      (check-eq? (unbox saved)    #f "check 5")
      (check-eq? (unbox inserted) #f "check 6")
      (check-eq? (unbox updated)  #f "check 7")
      (check-eq? (unbox deleted)  #f "check 8"))
    
    (test-case "saving is aborted when on-insert throws an exception"
      (set-pipelined-value! test-pipelined 2)
      (check-exn exn:unpipelined? (lambda () (save! test-pipelined)))
      (check-false (struct-id test-pipelined))
      (check-pred null? (let ([a (sql:entity 'a entity:pipelined)])
                          (find-all (sql:select #:from a))))
      ; Check which pipelines were run successfully:
      (check-eq? (unbox saved)    test-pipelined)
      (check-eq? (unbox inserted) #f)
      (check-eq? (unbox updated)  #f)
      (check-eq? (unbox deleted)  #f)
      (clear-boxes))
    
    (test-case "saving is aborted when on-update throws an exception"
      (set-pipelined-value! test-pipelined 0)
      (save! test-pipelined)
      (clear-boxes)
      (set-pipelined-value! test-pipelined 3)
      (check-exn exn:unpipelined? (lambda () (save! test-pipelined)))
      (check-equal? (pipelined-value (find-by-id entity:pipelined (struct-id test-pipelined))) 0)
      ; Check which pipelines were run successfully:
      (check-eq? (unbox saved)    test-pipelined)
      (check-eq? (unbox inserted) #f)
      (check-eq? (unbox updated)  #f)
      (check-eq? (unbox deleted)  #f)
      (clear-boxes))
    
    (test-case "deleting is aborted when on-delete throws an exception"
      (set-pipelined-value! test-pipelined 4)
      (save! test-pipelined)
      (clear-boxes)
      (check-exn exn:unpipelined? (lambda () (delete! test-pipelined)))
      (check-equal? (pipelined-value (find-by-id entity:pipelined (struct-id test-pipelined))) 4)
      ; Check which pipelines were run successfully:
      (check-eq? (unbox saved)    #f)
      (check-eq? (unbox inserted) #f)
      (check-eq? (unbox updated)  #f)
      (check-eq? (unbox deleted)  #f)
      (clear-boxes))))

; Provide statements -----------------------------

(provide make-snooze-pipeline-tests)
