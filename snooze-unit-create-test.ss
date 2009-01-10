(module snooze-unit-create-test mzscheme
  
  (require (lib "unitsig.ss")
           (lib "cut.ss" "srfi" "26"))
  
  (require (prefix q: (file "query-lang.ss"))
           (file "snooze-unit.ss")
           (file "snooze-sig.ss")
           (file "test-base.ss")
           (file "test-data.ss")
           (file "test-sig.ss"))
  
  (provide snooze-unit-create-tests@)

  ;; unit snooze-unit-create-tests@ : snooze^ -> test^
  (define snooze-unit-create-tests@
    (unit/sig test^
      (import snooze^)
        
      (define suite
        (test-suite
         "snooze-unit-create-test.ss"
         
         (test-case
          "create-table and drop-table work"
          (let ([a (q:entity entity:course)])
            ; Check the table exists
            (check-not-exn (cut find-all (q:select #:from a)))
            (drop-table entity:course)
            ; Check table doesn't exist
            (check-exn exn:fail:snooze? (cut find-all (q:select #:from a)))
            (create-table entity:course)
            ; Check the table exists again
            (check-not-exn (cut find-all (q:select #:from a)))))
         
         (test-case
          "select on non-existant table raises exception"
          (around (drop-table entity:course)
                  (check-exn exn:fail:snooze? 
                             (lambda ()
                               (let ([a (q:entity entity:course)])
                                 (find-all (q:select #:from a)))))
                  (create-table entity:course)))
         
         ))
      
      ))

  )
