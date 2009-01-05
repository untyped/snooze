(module snooze-unit-pipeline-test mzscheme
  
  (require (lib "unitsig.ss")
           (lib "cut.ss" "srfi" "26"))
  
  (require (planet "pipeline.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "era.ss")
           (file "persistent-struct.ss")
           (prefix q: (file "query-lang.ss"))
           (file "snooze-unit.ss")
           (file "snooze-sig.ss")
           (file "test-base.ss")
           (file "test-data.ss")
           (file "test-sig.ss")
           (file "type.ss"))
  
  (provide snooze-unit-pipeline-tests@)
  
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
    (make-stage 
     name
     (lambda (continue conn struct)
       (if (= (pipelined-value struct) bad-value)
           (raise-exn exn:unpipelined "Argh!")
           (begin (set-box! box struct)
                  (continue conn struct))))))
  
  ; Hey! It's a test entity:
  (define-persistent-struct pipelined
    ((value type:integer))
    ((save   (list (create-stage 'save   saved    1)))
     (insert (list (create-stage 'insert inserted 2)))
     (update (list (create-stage 'update updated  3)))
     (delete (list (create-stage 'delete deleted  4)))))
  
  ; ...and a persistent struct, too:
  (define test-pipelined (make-pipelined 0))

  ;; unit snooze-unit-pipeline-tests@ : snooze^ -> test^
  (define snooze-unit-pipeline-tests@
    (unit/sig test^
      (import snooze^)
        
      (define suite
        (test-suite
         "snooze-unit-pipeline-test.ss"
         
         ; ***** NOTE *****
         ; Each test below depends on the tests before it.
         ; Add/edit tests at your peril!
         ; ****************

         ;; create table for entity:pipelined
         #:before
         (lambda ()
          (create-table entity:pipelined))

         ;; drop table for entity:pipelined
         #:after
         (lambda ()
           (drop-table entity:pipelined))
         
         (test-case
          "on-save and on-insert are called when saving a new struct"
          (set-pipelined-value! test-pipelined 0)
          (save! test-pipelined)
          ; Check which pipelines were run successfully:
          (check-eq? (unbox saved)    test-pipelined)
          (check-eq? (unbox inserted) test-pipelined)
          (check-eq? (unbox updated)  #f)
          (check-eq? (unbox deleted)  #f)
          (clear-boxes))
         
         (test-case
          "on-save and on-update are called when re-saving a struct"
          (set-pipelined-value! test-pipelined 0)
          (save! test-pipelined)
          ; Check which pipelines were run successfully:
          (check-eq? (unbox saved)    test-pipelined)
          (check-eq? (unbox inserted) #f)
          (check-eq? (unbox updated)  test-pipelined)
          (check-eq? (unbox deleted)  #f)
          (clear-boxes))
         
         (test-case
          "on-delete is called on delete"
          (set-pipelined-value! test-pipelined 0)
          (delete! test-pipelined)
          ; Check which pipelines were run successfully:
          (check-eq? (unbox saved)    #f)
          (check-eq? (unbox inserted) #f)
          (check-eq? (unbox updated)  #f)
          (check-eq? (unbox deleted)  test-pipelined)
          (clear-boxes))
         
         (test-case
          "saving is aborted when on-save throws an exception"
          (set-pipelined-value! test-pipelined 1)
          (check-exn exn:unpipelined? (lambda () (save! test-pipelined)))
          (check-false (get-id test-pipelined))
          (check-pred null? (let ([a (q:entity entity:pipelined)])
                              (find-all (q:select #:from a))))
          ; Check which pipelines were run successfully:
          (check-eq? (unbox saved)    #f)
          (check-eq? (unbox inserted) #f)
          (check-eq? (unbox updated)  #f)
          (check-eq? (unbox deleted)  #f))
         
         (test-case
          "saving is aborted when on-insert throws an exception"
          (set-pipelined-value! test-pipelined 2)
          (check-exn exn:unpipelined? (lambda () (save! test-pipelined)))
          (check-false (get-id test-pipelined))
          (check-pred null? (let ([a (q:entity entity:pipelined)])
                              (find-all (q:select #:from a))))
          ; Check which pipelines were run successfully:
          (check-eq? (unbox saved)    test-pipelined)
          (check-eq? (unbox inserted) #f)
          (check-eq? (unbox updated)  #f)
          (check-eq? (unbox deleted)  #f)
          (clear-boxes))
         
         (test-case
          "saving is aborted when on-update throws an exception"
          (set-pipelined-value! test-pipelined 0)
          (save! test-pipelined)
          (clear-boxes)
          (set-pipelined-value! test-pipelined 3)
          (check-exn exn:unpipelined? (lambda () (save! test-pipelined)))
          (check-equal? (pipelined-value (find-by-id entity:pipelined (get-id test-pipelined))) 0)
          ; Check which pipelines were run successfully:
          (check-eq? (unbox saved)    test-pipelined)
          (check-eq? (unbox inserted) #f)
          (check-eq? (unbox updated)  #f)
          (check-eq? (unbox deleted)  #f)
          (clear-boxes))
         
         (test-case
          "deleting is aborted when on-delete throws an exception"
          (set-pipelined-value! test-pipelined 4)
          (save! test-pipelined)
          (clear-boxes)
          (check-exn exn:unpipelined? (lambda () (delete! test-pipelined)))
          (check-equal? (pipelined-value (find-by-id entity:pipelined (get-id test-pipelined))) 4)
          ; Check which pipelines were run successfully:
          (check-eq? (unbox saved)    #f)
          (check-eq? (unbox inserted) #f)
          (check-eq? (unbox updated)  #f)
          (check-eq? (unbox deleted)  #f)
          (clear-boxes))
         
         ))
      
      ))

  )
