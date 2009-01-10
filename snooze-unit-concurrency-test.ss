(module snooze-unit-concurrency-test mzscheme
  
  (require (lib "class.ss")
           (lib "unitsig.ss")
           (lib "cut.ss" "srfi" "26"))
  
  (require (file "era.ss")
           (file "snooze-unit.ss")
           (file "snooze-sig.ss")
           (file "test-base.ss")
           (file "test-data.ss")
           (file "test-sig.ss"))
  
  (provide snooze-unit-concurrency-tests@)
  
  ;; unit snooze-unit-concurrency-tests@ : snooze^ -> test^
  (define snooze-unit-concurrency-tests@
    (unit/sig test^
      (import snooze^)
      
      (define suite
        (test-suite
         "snooze-unit-concurrency-test.ss"
         
         (test-case
          "concurrent connections don't interfere with one another"
          (fail "Not implemented (see source in snooze-unit-concurrency-test.ss for explanation)."))
         
         (test-case
          "concurrent continuations don't interfere with one another"
          (fail "Not implemented (see source in snooze-unit-concurrency-test.ss for explanation)."))
         
         ; THESE ARE THE COMMENTS REFERRED TO ABOVE:
         ;
         ; We need to make a test-case that accurately reconstructs the behaviour of the
         ; web-server, but what with thread-pooling and what have you, there hasn't been
         ; time yet.
         
         ))
      
      ))
  
  )
 
 