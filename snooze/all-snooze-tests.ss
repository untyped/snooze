(module all-snooze-tests mzscheme
  
  (require (lib "unitsig.ss"))
  
  (require (file "generic/all-generic-tests.ss")
           (file "db-sig.ss")
           (file "era-test.ss")
           (file "persistent-struct-test.ss")
           (file "query-lang-test.ss")
           (file "query-util-test.ss")
           (file "snooze.ss")
           (file "snooze-sig.ss")
           (file "snooze-unit.ss")
           (file "snooze-unit-create-test.ss")
           (file "snooze-unit-concurrency-test.ss")
           (file "snooze-unit-find-test.ss")
           (file "snooze-unit-modify-test.ss")
           (file "snooze-unit-pipeline-test.ss")
           (file "snooze-unit-revision-test.ss")
           (file "snooze-unit-transaction-test.ss")
           (file "test-base.ss")
           (file "test-data.ss")
           (file "test-sig.ss"))
  
  (provide make-all-snooze-tests)
  
  ; Tests ----------------------------------------
  
  (define (make-all-snooze-tests db@ backend-tests)
    (define-values/invoke-unit/sig
      (create-tests
       modify-tests
       pipeline-tests
       find-tests
       revision-tests
       transaction-tests
       concurrency-tests
       create-table
       drop-table)
      (compound-unit/sig
        (import)
        (link (db          : db^     (db@))
              (snooze      : snooze^ (snooze@ db))
              (create      : test^   (snooze-unit-create-tests@      snooze))
              (modify      : test^   (snooze-unit-modify-tests@      snooze))
              (pipeline    : test^   (snooze-unit-pipeline-tests@    snooze))
              (find        : test^   (snooze-unit-find-tests@        snooze))
              (revision    : test^   (snooze-unit-revision-tests@    snooze))
              (transaction : test^   (snooze-unit-transaction-tests@ snooze))
              (concurrency : test^   (snooze-unit-concurrency-tests@ snooze)))
        (export (var (create suite)        create-tests)
                (var (modify suite)        modify-tests)
                (var (pipeline suite)      pipeline-tests)
                (var (find suite)          find-tests)
                (var (revision suite)      revision-tests)
                (var (transaction suite)   transaction-tests)
                (var (concurrency suite)   concurrency-tests)
                (var (snooze create-table) create-table)
                (var (snooze drop-table)   drop-table))))
    (test-suite
     "all-snooze-tests"
     
     ;; Create tables for test data
     #:before
     (lambda ()
       (create-table entity:course)
       (create-table entity:programme)
       (create-table entity:programme-structure))
     
     ;; Drop tables for test data
     #:after
     (lambda ()
       (drop-table entity:course)
       (drop-table entity:programme)
       (drop-table entity:programme-structure))
     
     ; These tests can be run without a snooze^ or a db^:
     era-tests
     persistent-struct-tests
     query-lang-tests
     query-util-tests
     
     ; These tests are for the db^ backend:
     all-generic-tests
     backend-tests
     
     ; These tests are for the snooze^ front-end:
     create-tests
     modify-tests
     pipeline-tests
     find-tests
     revision-tests
     transaction-tests
     concurrency-tests
     
     ))
  
  )
 
 
 
