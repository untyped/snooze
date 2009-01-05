(module era-test mzscheme
  
  (require (lib "cut.ss" "srfi" "26"))
  
  (require (planet "hash-table.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "era.ss")
           (file "persistent-struct.ss")
           (file "test-base.ss")
           (file "test-data.ss")
           (file "type.ss"))
  
  (provide era-tests)
  
  ; Tests ----------------------------------------
  
  ; Test data:
  
  ;; unsaved : course
  ;; saved : course
  (define unsaved (make-course 'UNSAVED "Unsaved" 1 1.5 #f time1))
  (define saved   (make-course 'SAVED   "Saved"   2 2.5 #t time2))
  (set-id! saved 0)
  (set-revision! saved 1000)
  
  (define unsaved-alist
    `((id       . #f)
      (revision . #f)
      (code     . UNSAVED)
      (name     . "Unsaved")
      (value    . 1)
      (rating   . 1.5)
      (active   . #f)
      (start    . ,time1)))
  
  (define saved-alist
    `((id       . 0)
      (revision . 1000)
      (code     . SAVED)
      (name     . "Saved")
      (value    . 2)
      (rating   . 2.5)
      (active   . #t)
      (start    . ,time2)))

  (define unsaved-hash-table
    (apply make-hash-table/pairs unsaved-alist))
  
  (define saved-hash-table
    (apply make-hash-table/pairs saved-alist))
  
  ;; syntax update-store! : (list-of (list-of any)) any ...
  ;;
  ;; Uesd in tests for for-each/entity and for-each/struct to
  ;; record the data passed to the iterated function... see these
  ;; tests for more information.
  (define-syntax (store-mutator stx)
    (syntax-case stx ()
      [(_ store)
       #'(lambda args
           (set! store `(,@store ,args)))]))
  
  (define era-tests
    (test-suite
     "era.ss"
     
     (test-case
      "get-id returns correct id"
      (check-equal? (get-id unsaved) #f)
      (check-equal? (get-id saved) 0))
     
     (test-case
      "set-id! changes the id"
      (let ([s (make-course 'FOO "Foo" 999 333.3 #t time1)])
        (check-equal? (get-id s) #f)
        (set-id! s 1)
        (check-equal? (get-id s) 1)))
     
     (test-case
      "get-revision returns correct revision"
      (check-equal? (get-revision unsaved) #f)
      (check-equal? (get-revision saved) 1000))
     
     (test-case
      "set-revision! changes the revision"
      (let ([s (make-course 'FOO "Foo" 999 333.3 #t time1)])
        (check-equal? (get-revision s) #f)
        (set-revision! s 1000)
        (check-equal? (get-revision s) 1000)))
     
     (test-eq?
      "struct-entity returns the correct entity"
      (struct-entity unsaved)
      entity:course)
     
     (test-exn
      "struct-entity throws exn:fail:contract when argument isn't a persistent struct"
      exn:fail:contract?
      (lambda ()
        (define-struct test (a b c))
        (struct-entity (make-test 1 2 3))))
     
     (test-case
      "call-with-entity transparently returns a value"
      (check-equal?
       (call-with-entity saved
         (lambda (entity) 
           123))
       123)
      (check-equal?
       (call-with-entity unsaved
         (lambda (entity)
           #f))
       #f))
     
     (test-case
      "has-attribute? works as expected"
      (check-true  (has-attribute? entity:course 'code) #t)
      (check-true  (has-attribute? entity:course 'name) #t)
      (check-true  (has-attribute? entity:course 'value) #t)
      (check-true  (has-attribute? entity:course 'active) #t)
      (check-false (has-attribute? entity:course 'fake) #f))
     
     (test-case
      "get-attribute returns the expected attribute"
      (let ([attrib (get-attribute entity:course 'id)])
        (check-equal? (attribute-name attrib) 'id)
        (check-equal? (attribute-type attrib) type:id))
      (let ([attrib (get-attribute entity:course 'revision)])
        (check-equal? (attribute-name attrib) 'revision)
        (check-equal? (attribute-type attrib) type:revision))
      (let ([attrib (get-attribute entity:course 'code)])
        (check-equal? (attribute-name attrib) 'code)
        (check-equal? (attribute-type attrib) type:symbol))
      (let ([attrib (get-attribute entity:course 'name)])
        (check-equal? (attribute-name attrib) 'name)
        (check-equal? (attribute-type attrib) type:text))
      (let ([attrib (get-attribute entity:course 'value)])
        (check-equal? (attribute-name attrib) 'value)
        (check-equal? (attribute-type attrib) type:integer))
      (let ([attrib (get-attribute entity:course 'rating)])
        (check-equal? (attribute-name attrib) 'rating)
        (check-equal? (attribute-type attrib) type:real))
      (let ([attrib (get-attribute entity:course 'start)])
        (check-equal? (attribute-name attrib) 'start)
        (check-equal? (attribute-type attrib) type:time-tai)))
     
     (test-case
      "get-attribute-index returns the expected index"
      ; ID has index 0
      (check-equal? (get-attribute-index entity:course 'id)       0)
      (check-equal? (get-attribute-index entity:course 'revision) 1)
      (check-equal? (get-attribute-index entity:course 'code)     2)
      (check-equal? (get-attribute-index entity:course 'name)     3)
      (check-equal? (get-attribute-index entity:course 'value)    4)
      (check-equal? (get-attribute-index entity:course 'rating)   5)
      (check-equal? (get-attribute-index entity:course 'active)   6)
      (check-equal? (get-attribute-index entity:course 'start)    7))
     
     (test-case
      "get-attribute-value returns the expected value"
      (check-equal? (get-attribute-value unsaved 'id)       #f)
      (check-equal? (get-attribute-value unsaved 'revision) #f)
      (check-equal? (get-attribute-value unsaved 'code)     'UNSAVED)
      (check-equal? (get-attribute-value unsaved 'name)     "Unsaved")
      (check-equal? (get-attribute-value unsaved 'value)    1)
      (check-equal? (get-attribute-value unsaved 'rating)   1.5)
      (check-equal? (get-attribute-value unsaved 'active)   #f)
      (check-equal? (get-attribute-value unsaved 'start)    time1)
      (check-equal? (get-attribute-value saved   'id)       0)
      (check-equal? (get-attribute-value saved   'revision) 1000)
      (check-equal? (get-attribute-value saved   'code)     'SAVED)
      (check-equal? (get-attribute-value saved   'name)     "Saved")
      (check-equal? (get-attribute-value saved   'value)    2)
      (check-equal? (get-attribute-value saved   'rating)    2.5)
      (check-equal? (get-attribute-value saved   'active)   #t)
      (check-equal? (get-attribute-value saved   'start)    time2))
     
    (test-case
      "get-attribute-xxx functions all throw an exception when applied to a non-existant field name"
      (let ()
        (check-exn exn:fail:snooze? (lambda () (get-attribute       entity:course 'a)) "get-attribute test failed")
        (check-exn exn:fail:snooze? (lambda () (get-attribute-index entity:course 'a)) "get-attribute-index test failed")
        (check-exn exn:fail:snooze? (lambda () (get-attribute-value saved 'a))         "get-attribute-value test failed")))
    
     (test-case
      "get-attribute-xxx functions all throw an exception when applied to a non-persistent struct"
      (let ()
        (define-struct test-struct (a b c))
        (define test (make-test-struct 1 2 3))
        (check-exn exn:fail:contract? (lambda () (get-attribute       struct:test-struct 'a)) "get-attribute test failed")
        (check-exn exn:fail:contract? (lambda () (get-attribute-index struct:test-struct 'a)) "get-attribute-index test failed")
        (check-exn exn:fail:contract? (lambda () (get-attribute-value test 'a))               "get-attribute-value test failed")))
     
     (test-case
      "map-attributes/entity iterates through all attributes except id and revision"
      (check-equal?
       (map-attributes/entity (cut list <> <>) entity:course)
       `((code   ,type:symbol)
         (name   ,type:text)
         (value  ,type:integer)
         (rating ,type:real)
         (active ,type:boolean)
         (start  ,type:time-tai))))
     
     (test-case
      "map-attributes/struct iterates through all attributes except id and revision"
      (check-equal?
       (map-attributes/struct (cut list <> <> <>) unsaved)
       `((code   ,type:symbol   UNSAVED)
         (name   ,type:text     "Unsaved")
         (value  ,type:integer  1)
         (rating ,type:real     1.5)
         (active ,type:boolean  #f)
         (start  ,type:time-tai ,time1)))
      (check-equal?
       (map-attributes/struct (cut list <> <> <>) saved)
       `((code   ,type:symbol   SAVED)
         (name   ,type:text     "Saved")
         (value  ,type:integer  2)
         (rating ,type:real     2.5)
         (active ,type:boolean  #t)
         (start  ,type:time-tai ,time2))))
     
     (test-case
      "for-each-attribute/entity iterates through all attributes except id and revision"
      (let* ([store null]
             [update! (store-mutator store)]) ; function that appends argument list to store
        (for-each-attribute/entity update! entity:course)
        (check-equal?
         store
         `((code   ,type:symbol)
           (name   ,type:text)
           (value  ,type:integer)
           (rating ,type:real)
           (active ,type:boolean)
           (start  ,type:time-tai)))))
     
     (test-case
      "for-each-attribute/struct iterates through all attributes except id and revision"
      (let* ([store1 null]
             [store2 null]
             [update-store1! (store-mutator store1)]  ; function that appends argument list to store1
             [update-store2! (store-mutator store2)]) ; function that appends argument list to store2
        (for-each-attribute/struct update-store1! unsaved)
        (for-each-attribute/struct update-store2! saved)
        (check-equal?
         store1
         `((code   ,type:symbol  UNSAVED)
           (name   ,type:text    "Unsaved")
           (value  ,type:integer 1)
           (rating ,type:real    1.5)
           (active ,type:boolean #f)
           (start  ,type:time-tai ,time1)))
        (check-equal?
         store2
         `((code   ,type:symbol  SAVED)
           (name   ,type:text    "Saved")
           (value  ,type:integer 2)
           (rating ,type:real    2.5)
           (active ,type:boolean #t)
           (start  ,type:time-tai ,time2)))))
     
     (test-case
      "persistent-struct->hash-table works as expected"
      (check-equal? (persistent-struct->hash-table unsaved) unsaved-hash-table)
      (check-equal? (persistent-struct->hash-table saved)   saved-hash-table))
     
     (test-case
      "persistent-struct->alist works as expected"
      (check-equal? (persistent-struct->alist unsaved) unsaved-alist)
      (check-equal? (persistent-struct->alist saved)   saved-alist))
     
     (test-case
      "make-blank-persistent-struct works as expected"
      (check-equal?
       (make-blank-persistent-struct entity:course)
       (make-course #f #f #f #f #f #f)))
     
     (test-case
      "hash-table->persistent-struct works as expected"
      (let* ([foo1 (hash-table->persistent-struct
                    entity:course
                    (make-hash-table/pairs
                     (cons 'id       9876) ; Shouldn't be added to the struct
                     (cons 'revision 8765) ; Shouldn't be added to the struct
                     (cons 'code     'CODE)
                     (cons 'name     "Name")
                     (cons 'value    123)
                     (cons 'rating   123.4)
                     (cons 'active   #t)
                     (cons 'start    time1)))]
             [foo2 (make-course 'CODE' "Name" 123 123.4 #t time1)])
        (check equal? foo1 foo2 "Basic equality check failed")
        (check equal? (get-id foo1) #f "ID check failed")
        (check equal? (get-revision foo1) #f "Revision check failed")))
     
     (test-case
      "set-attributes/hash-table! overwrites attributes appropriately"
      (let ([foo   (make-course 'CODE' "Name" 123 123.4 #t time1)]
            [table (make-hash-table/pairs
                    (cons 'code   'CODE2)
                    (cons 'name   "Name2")
                    (cons 'value  321)
                    (cons 'rating 432.1)
                    (cons 'active #f)
                    (cons 'start time2))])
        (set-attributes/hash-table! foo table)
        (check-equal? (course-code foo)   'CODE2  "Code failed")
        (check-equal? (course-name foo)   "Name2" "Name failed")
        (check-equal? (course-value foo)  321     "Value failed")
        
        (check-equal? (course-active foo) #f      "Active failed")
        (check-equal? (course-start foo)  time2   "Start failed")))
     
     (test-case
      "set-attributes/hash-table! does not overwrite ID or revision"
      (let ([foo   (make-course 'CODE' "Name" 123 123.4 #t time1)]
            [table (make-hash-table/pairs
                    (cons 'id 12345)
                    (cons 'revision 23456))])
        (set-attributes/hash-table! foo table)
        (check-equal? (get-id foo) #f)
        (check-equal? (get-revision foo) #f)))
     
     (test-case
      "set-attributes/hash-table! ignores attributes that aren't in the entity"
      (let ([foo   (make-course 'CODE' "Name" 123 123.4 #t time1)]
            [table (make-hash-table/pairs
                    (cons 'code 'CODE2)
                    (cons 'cøde 'CODE3)
                    (cons 'name "Name2")
                    (cons 'n "Name3")
                    (cons 'value 321)
                    (cons 'val 4321)
                    (cons 'active #f)
                    (cons 'active2 #t)
                    (cons 'start time2))])
        (set-attributes/hash-table! foo table)
        (check-equal? (course-code foo)   'CODE2  "Code failed")
        (check-equal? (course-name foo)   "Name2" "Name failed")
        (check-equal? (course-value foo)  321     "Value failed")
        (check-equal? (course-rating foo) 123.4   "Rating failed")
        (check-equal? (course-active foo) #f      "Active failed")
        (check-equal? (course-start foo)  time2   "Start failed")))
     
     (test-case
      "set-attributes/alist! overwrites attributes appropriately"
      (let ([foo    (make-course 'CODE' "Name" 123 123.4 #t time1)]
            [fields (list (cons 'code 'CODE2)
                          (cons 'name "Name2")
                          (cons 'value 321)
                          (cons 'rating 432.1)
                          (cons 'active #f)
                          (cons 'start time2))])
        (set-attributes/alist! foo fields)
        (check-equal? (course-code foo)   'CODE2  "Code failed")
        (check-equal? (course-name foo)   "Name2" "Name failed")
        (check-equal? (course-value foo)  321     "Value failed")
        (check-equal? (course-rating foo) 432.1   "Rating failed")
        (check-equal? (course-active foo) #f      "Active failed")
        (check-equal? (course-start foo)  time2   "Start failed")))
     
     (test-case
      "set-attributes/alist! does not overwrite ID or revision"
      (let ([foo    (make-course 'CODE' "Name" 123 123.4 #t time1)]
            [fields (list (cons 'id 12345)
                          (cons 'revision 23456))])
        (set-attributes/alist! foo fields)
        (check-equal? (get-id foo) #f)
        (check-equal? (get-revision foo) #f)))
     
     (test-case
      "set-attributes/alist! ignores attributes that aren't in the entity"
      (let ([foo    (make-course 'CODE' "Name" 123 123.4 #t time1)]
            [fields (list (cons 'code 'CODE2)
                          (cons 'cøde 'CODE3)
                          (cons 'name "Name2")
                          (cons 'n "Name3")
                          (cons 'value 321)
                          (cons 'val 4321)
                          (cons 'active #f)
                          (cons 'active2 #t)
                          (cons 'start time2))])
        (set-attributes/alist! foo fields)
        (check-equal? (course-code foo)   'CODE2  "Code failed")
        (check-equal? (course-name foo)   "Name2" "Name failed")
        (check-equal? (course-value foo)  321     "Value failed")
        (check-equal? (course-rating foo) 123.4   "Rating failed")
        (check-equal? (course-active foo) #f      "Active failed")
        (check-equal? (course-start foo)  time2   "Start failed")))
     
     ))
  
  )
 