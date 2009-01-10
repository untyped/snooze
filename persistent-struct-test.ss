(module persistent-struct-test mzscheme
  
  (require (lib "plt-match.ss"))
  
  (require (file "era.ss")
           (file "persistent-struct.ss")
           (file "test-base.ss")
           (file "test-data.ss")
           (file "type.ss"))
  
  (provide persistent-struct-tests)
  
  (define-persistent-struct with-fields
    ([a type:integer]
     [b type:integer/1]))
  
  (define-persistent-struct with-pipelines
    ([a type:integer]
     [b type:integer/1])
    ())
  
  (define-persistent-struct with-properties
    ([a type:integer]
     [b type:integer/1])
    ()
    ())

  (define-syntax (check-identifier-bound stx)
    (syntax-case stx ()
      [(_ arg)
       (if (identifier-binding #'arg)
           #'(begin (void))
           #'(begin (fail (format "Identitifer \"~a\" is not bound." 'arg))))]))
  
  (define persistent-struct-tests
    (test-suite
     "persistent-struct.ss"
     
     (test-case
      "Test all forms of make-persistent-struct"
      (fail "Not implemented"))
     
     ; The very fact this test compiles indicates that it is successful.
     (test-case
      "define-persistent-struct defines an appropriate constructor, id accessor and revision accessor"
      (check-false (course-id       (make-course 'CODE "Name" 123 123.4 #t time1)))
      (check-false (course-revision (make-course 'CODE "Name" 123 123.4 #t time1))))
      
     (test-case
      "constructor retrieved from entity allows specification of id and revision"
      (let ([construct (entity-constructor entity:course)])
        (check-pred course? (construct #f #f 'CODE "Name" 123 123.4 #t time1))))
     
     (test-case
      "define-persistent-struct defines correct metadata"
      (check-pred entity? entity:course)
      (check-eq? (entity-name entity:course) 'course)
      (let ([attrs (entity-fields entity:course)])
        (check = (length attrs) 8) ; 6 data attributes plus id and revision
        (check-eq? (attribute-name (list-ref attrs 0)) 'id)
        (check-eq? (attribute-type (list-ref attrs 0)) type:id)
        (check-eq? (attribute-name (list-ref attrs 1)) 'revision)
        (check-eq? (attribute-type (list-ref attrs 1)) type:revision)
        (check-eq? (attribute-name (list-ref attrs 2)) 'code)
        (check-eq? (attribute-type (list-ref attrs 2)) type:symbol)
        (check-eq? (attribute-name (list-ref attrs 3)) 'name)
        (check-eq? (attribute-type (list-ref attrs 3)) type:text)
        (check-eq? (attribute-name (list-ref attrs 4)) 'value)
        (check-eq? (attribute-type (list-ref attrs 4)) type:integer/1)
        (check-eq? (attribute-name (list-ref attrs 5)) 'rating)
        (check-eq? (attribute-type (list-ref attrs 5)) type:real)
        (check-eq? (attribute-name (list-ref attrs 6)) 'active)
        (check-eq? (attribute-type (list-ref attrs 6)) type:boolean/t)
        (check-eq? (attribute-name (list-ref attrs 7)) 'start)
        (check-eq? (attribute-type (list-ref attrs 7)) type:time-tai)))
     
     (test-case
      "persistent structures have fields assigned correctly"
      (let ([course (make-course 'CODE "Name" 123 123.4 #t time1)])
        (check-equal? (course-id course)       #f)
        (check-equal? (course-revision course) #f)
        (check-equal? (course-code course)     'CODE)
        (check-equal? (course-name course)     "Name")
        (check-equal? (course-value course)    123)
        (check-equal? (course-rating course)   123.4)
        (check-equal? (course-active course)   #t)
        (check-equal? (course-start course)    time1)))
     
     (test-case
      "\"with-fields\" structure was defined with all the right accoutrements"
      (check-identifier-bound with-fields)
      (check-pred persistent-struct? struct:with-fields)
      (check-pred procedure?         make-with-fields)
      (check-pred procedure?         make-with-fields/defaults)
      (check-pred entity?            entity:with-fields)
      (check-pred procedure?         with-fields?)
      (check-pred procedure?         with-fields-ref)
      (check-pred procedure?         with-fields-set!)
      (check-pred procedure?         with-fields-id)
      (check-pred procedure?         with-fields-revision)
      (check-pred procedure?         with-fields-a)
      (check-pred procedure?         set-with-fields-a!)
      (check-pred procedure?         with-fields-b)
      (check-pred procedure?         set-with-fields-b!))

     (test-case
      "\"with-pipelines\" structure was defined with all the right accoutrements"
      (check-identifier-bound with-pipelines)
      (check-pred persistent-struct? struct:with-pipelines)
      (check-pred procedure?         make-with-pipelines)
      (check-pred procedure?         make-with-pipelines/defaults)
      (check-pred entity?            entity:with-pipelines)
      (check-pred procedure?         with-pipelines?)
      (check-pred procedure?         with-pipelines-ref)
      (check-pred procedure?         with-pipelines-set!)
      (check-pred procedure?         with-pipelines-id)
      (check-pred procedure?         with-pipelines-revision)
      (check-pred procedure?         with-pipelines-a)
      (check-pred procedure?         set-with-pipelines-a!)
      (check-pred procedure?         with-pipelines-b)
      (check-pred procedure?         set-with-pipelines-b!))
          
     (test-case
      "\"with-properties\" structure was defined with all the right accoutrements"
      (check-identifier-bound with-properties)
      (check-pred persistent-struct? struct:with-properties)
      (check-pred procedure?         make-with-properties)
      (check-pred procedure?         make-with-properties/defaults)
      (check-pred entity?            entity:with-properties)
      (check-pred procedure?         with-properties?)
      (check-pred procedure?         with-properties-ref)
      (check-pred procedure?         with-properties-set!)
      (check-pred procedure?         with-properties-id)
      (check-pred procedure?         with-properties-revision)
      (check-pred procedure?         with-properties-a)
      (check-pred procedure?         set-with-properties-a!)
      (check-pred procedure?         with-properties-b)
      (check-pred procedure?         set-with-properties-b!))
     
     (test-case
      "plt-match works as expected"
      (match (make-with-fields 1 2)
        [(struct with-fields (a b))
         (check-equal? a 1)
         (check-equal? b 2)]))
          
     ))
  )
