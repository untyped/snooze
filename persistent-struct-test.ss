#lang scheme/base

(require (for-syntax scheme/base)
         scheme/match
         scheme/serialize
         srfi/26/cut
         "snooze.ss"
         "test-base.ss"
         "test-data.ss"
         "test-util.ss")

; Helpers --------------------------------------

(define time-tai1 (string->time-tai "2001-01-01 01:01:01"))

(define-syntax (check-identifier-bound stx)
  (syntax-case stx ()
    [(_ arg)
     (if (identifier-binding #'arg)
         #'(begin (void))
         #'(begin (fail (format "Identitifer \"~a\" is not bound." 'arg))))]))

; Test data --------------------------------------

(define-persistent-struct thing
  ([a type:integer]
   [b type:string]))

; Tests ----------------------------------------

(define persistent-struct-tests
  (test-suite "persistent-struct.ss"
    
    (test-case "new entity and attribute syntax"
      (check-eq? person entity:person)
      (check-eq? (attr person id) attr:person-id)
      (check-eq? (attr entity:person id) attr:person-id))
    
    ; The very fact this test compiles indicates that it is successful.
    (test-case "define-persistent-struct defines an appropriate constructor, id accessor and revision accessor"
      (check-false (person-id       (make-person "Dave")))
      (check-false (person-revision (make-person "Dave"))))
    
    (test-case "constructor retrieved from entity allows specification of id and revision"
      (let ([construct (entity-constructor entity:person)])
        (check-pred person? (construct #f #f "Dave"))))
    
    (test-case "define-persistent-struct defines correct metadata"
      (check-pred entity? course)
      (check-pred entity? entity:course)
      (check-eq? course entity:course)
      (check-eq? (entity-name entity:course) 'course)
      (check-eq? (entity-table-name entity:course) 'Course)
      (let ([attrs (entity-attributes entity:course)])
        (check = (length attrs) 8) ; 6 data attributes plus id and revision
        (check-eq? (attribute-name (list-ref attrs 0)) 'id)
        (check-eq? (attribute-type (list-ref attrs 0)) type:id)
        (check-eq? (attribute-name (list-ref attrs 1)) 'revision)
        (check-eq? (attribute-type (list-ref attrs 1)) type:revision)
        (check-eq? (attribute-name (list-ref attrs 2)) 'code)
        (check-eq? (attribute-type (list-ref attrs 2)) type:symbol)
        (check-eq? (attribute-name (list-ref attrs 3)) 'name)
        (check-eq? (attribute-type (list-ref attrs 3)) type:string)
        (check-eq? (attribute-name (list-ref attrs 4)) 'value)
        (check-eq? (attribute-type (list-ref attrs 4)) type:integer)
        (check-eq? (attribute-name (list-ref attrs 5)) 'rating)
        (check-eq? (attribute-type (list-ref attrs 5)) type:real)
        (check-eq? (attribute-name (list-ref attrs 6)) 'active)
        (check-eq? (attribute-type (list-ref attrs 6)) type:boolean)
        (check-eq? (attribute-name (list-ref attrs 7)) 'start)
        (check-eq? (attribute-type (list-ref attrs 7)) type:time-tai))
      (let ([attrs (entity-attributes entity:pet)])
        (check-eq? (attribute-name (list-ref attrs 2)) 'owner-id)
        (check-eq? (attribute-column-name (list-ref attrs 2)) 'ownerID)
        (check-eq? (attribute-type (list-ref attrs 2)) type:integer)))
    
    (test-case "persistent structures have fields assigned correctly"
      (let ([course (make-course 'CODE "Name" 123 123.4 #t time-tai1)])
        (check-equal? (course-id course)       #f)
        (check-equal? (course-revision course) #f)
        (check-equal? (course-code course)     'CODE)
        (check-equal? (course-name course)     "Name")
        (check-equal? (course-value course)    123)
        (check-equal? (course-rating course)   123.4)
        (check-equal? (course-active course)   #t)
        (check-equal? (course-start course)    time-tai1)))
    
    (test-case "define-persistent-struct: basic form"
      (check-identifier-bound thing)
      (check-pred struct-has-entity? struct:thing        "check 1")
      (check-pred persistent-struct? (make-thing 1 2)    "check 2")
      (check-pred procedure?         make-thing          "check 3")
      (check-pred procedure?         make-thing/defaults "check 4")
      (check-pred entity?            entity:thing        "check 5")
      (check-pred procedure?         thing?              "check 6")
      (check-pred procedure?         thing-id            "check 7")
      (check-pred procedure?         set-thing-id!       "check 8")
      (check-pred procedure?         thing-revision      "check 9")
      (check-pred procedure?         set-thing-revision! "check 10")
      (check-pred procedure?         thing-a             "check 11")
      (check-pred procedure?         set-thing-a!        "check 12")
      (check-pred procedure?         thing-b             "check 13")
      (check-pred procedure?         set-thing-b!        "check 14"))
    
    (test-case "plt-match"
      (check-identifier-bound thing)
      (match (make-thing 1 2)
        [(struct thing (id revision a b))
         (check-equal? id #f "id")
         (check-equal? revision #f "revision")
         (check-equal? a 1 "a")
         (check-equal? b 2 "b")]))
    
    (test-case "serialize and deserialize"
      (let ([person (make-person "Dave")])
        (check-not-exn (cut serialize person) "person")
        (check-equal? person (deserialize (serialize person)) "person: no id or revision")
        (set-struct-id! person 12345)
        (set-struct-revision! person 54321)
        (check-equal? person (deserialize (serialize person)) "person: id and revision"))
      (let ([course (make-course 'CODE "Name" 123 123.4 #t time-tai1)])
        (check-not-exn (cut serialize course) "course")
        (check-equal? course (deserialize (serialize course)) "course: no id or revision")
        (set-struct-id! course 12345)
        (set-struct-revision! course 54321)
        (check-equal? course (deserialize (serialize course)) "course: id and revision")))))

; Provide statements -----------------------------

(provide persistent-struct-tests)
