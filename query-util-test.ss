(module query-util-test mzscheme
  
  (require (file "query-core.ss")
           (prefix q: (file "query-lang.ss"))
           (file "query-util.ss")
           (file "test-base.ss")
           (file "test-data.ss")
           (file "type.ss"))
  
  (provide query-util-tests)
  
  ; Tests ----------------------------------------
  
  (define query-util-tests
    (test-suite
     "query-util.ss"
     
     (test-case
      "let-alias"
      (let-alias ([entity1 entity:course]
                  [table1 'table-name])
        (let-alias ([attr1  (q:attr entity1 'code)]
                    [field1 (q:field table1 'field type:text)])
          (check-pred entity? entity1 "check 1")
          (check-pred table?  table1  "check 2")
          (check-pred field?  attr1   "check 3")
          (check-pred field?  field1  "check 4")
          (check-equal? (named-alias entity1)   'entity1      "check 5")
          (check-equal? (named-alias table1)    'table1       "check 6")
          (check-equal? (named-alias attr1)     'entity1-code "check 7")
          (check-equal? (named-alias field1)    'table1-field "check 8")
          (check-equal? (entity-entity entity1) entity:course "check 9")
          (check-equal? (table-name table1)     'table-name   "check 10")
          (check-equal? (field-table attr1)     entity1       "check 11")
          (check-equal? (field-table field1)    table1        "check 12")
          (check-equal? (field-name attr1)      'code         "check 13")
          (check-equal? (field-name field1)     'field        "check 14"))))
     
     (test-case
      "let*-alias"
      (let*-alias ([entity1 entity:course]
                   [table1 'table-name]
                   [attr1  (q:attr entity1 'code)]
                   [field1 (q:field table1 'field type:text)])
        (check-pred entity? entity1 "check 1")
        (check-pred table?  table1  "check 2")
        (check-pred field?  attr1   "check 3")
        (check-pred field?  field1  "check 4")
        (check-equal? (named-alias entity1)   'entity1      "check 5")
        (check-equal? (named-alias table1)    'table1       "check 6")
        (check-equal? (named-alias attr1)     'entity1-code "check 7")
        (check-equal? (named-alias field1)    'table1-field "check 8")
        (check-equal? (entity-entity entity1) entity:course "check 9")
        (check-equal? (table-name table1)     'table-name   "check 10")
        (check-equal? (field-table attr1)     entity1       "check 11")
        (check-equal? (field-table field1)    table1        "check 12")
        (check-equal? (field-name attr1)      'code         "check 13")
        (check-equal? (field-name field1)     'field        "check 14")))
     
     ))
  
  )