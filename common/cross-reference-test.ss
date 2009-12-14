#lang scheme/base

(require "../test-base.ss")

(require srfi/19
         (unlib-in hash gen)
         "../core/core.ss"
         "../sql/sql.ss"
         "cross-reference.ss")

(require/expose "cross-reference.ss"
  (column->struct-index))

; Helpers and test data --------------------------

(define-entity vehicle
  ([owner    person]
   [occupant pet]
   [name     string]))

; integer integer string -> person
(define (test-person id revision name)
  ((entity-private-constructor person)
   (entity-make-guid person id)
   revision
   name))

; integer integer person-guid string -> person
(define (test-pet id revision owner-guid name)
  ((entity-private-constructor pet)
   (entity-make-guid pet id)
   revision
   owner-guid
   name))

; integer integer person-guid pet-guid string -> person
(define (test-vehicle id revision owner-guid occupant-guid name)
  ((entity-private-constructor vehicle)
   (entity-make-guid vehicle id)
   revision
   owner-guid
   occupant-guid
   name))

; natural -> guid
(define (person-guid id) (entity-make-guid person id))
(define (pet-guid    id) (entity-make-guid pet    id))

(define (entity-columns entity)
  (let ([def (entity-default-alias entity)])
    (map (cut sql:alias def <>)
         (entity-attributes entity))))

(define (make-cross-referencer . args)
  (send/apply (send (current-snooze) get-database) make-cross-referencer args))

(define-check (check-cross-referenced actual expected)
  (if (pair? actual)
      (for ([actual   (in-list actual)]
            [expected (in-list expected)])
        (check-equal? actual expected))
      (check-equal? actual expected)))

; Tests ----------------------------------------

(define cross-reference-tests
  (test-suite "cross-reference.ss"
    
    (test-equal? "column->struct-index"
      (for/list ([i (in-range 0 10)])
        (column->struct-index i (list 0 1 2 3 4 5 6 7 8 9) (list 1 4 1 4)))
      (list 0 1 1 1 1 2 3 3 3 3))
    
    (test-case "make-cross-referencer: single entity pass-through"
      (recreate-test-tables)
      (let* ([per1   (test-person 1 2 "Dave")]
             [per2   (test-person 2 3 "David")]
             [input  (list per1 per2)]
             [xref*  (make-cross-referencer (entity-columns person) person #hash())]
             [xref   (cut xref* <> (transaction-frame-push #f))]
             [do-row (g:map xref (g:list input))])
        (check-cross-referenced (do-row) per1)
        (check-cross-referenced (do-row) per2)
        (check-pred g:end? (do-row))))
    
    (test-case "make-cross-referencer: twin entity pass-through"
      (recreate-test-tables)
      (let* ([per1   (test-person 1 2 "Dave")]
             [per2   (test-person 2 3 "David")]
             [per3   (test-person 3 4 "Dave")]
             [per4   (test-person 4 5 "David")]
             [input  (list (list per1 per2)
                           (list per3 per4))]
             [xref*  (make-cross-referencer (entity-columns person)
                                            (list person person)
                                            #hash())]
             [xref   (cut xref* <> (transaction-frame-push #f))]
             [do-row (g:map xref (g:list input))])
        (check-cross-referenced (do-row) (list per1 per2))
        (check-cross-referenced (do-row) (list per3 per4))
        (check-pred g:end? (do-row))))
    
    (test-case "make-cross-referencer: one cross reference"
      (recreate-test-tables)
      (let* ([per1   (test-person 1 2 "Jon")]
             [per2   (test-person 2 3 "Lyman")]
             [pet1   (test-pet 1 2 (person-guid 1) "Garfield")]
             [pet2   (test-pet 2 3 (person-guid 2) "Odie")]
             [input  (list (list pet1 per1)
                           (list pet2 per2))]
             [xref*  (make-cross-referencer (append (entity-columns pet) (entity-columns person))
                                            (list pet person)
                                            (build-hash (sql-list pet.owner person.guid)))]
             [xref   (cut xref* <> (transaction-frame-push #f))]
             [do-row (g:map xref (g:list input))])
        ; We use struct->vector and vector-ref below to avoid Snooze's default dereferencing behaviour:
        
        ; Cross-reference the first row:
        (check-cross-referenced (do-row) (list pet1 per1))
        (check-equal? (vector-ref (struct->vector pet1) 3) per1)
        (check-equal? (vector-ref (struct->vector pet2) 3) (person-guid 2))
        ; Cross-reference the second row:
        (check-cross-referenced (do-row) (list pet2 per2))
        (check-equal? (vector-ref (struct->vector pet1) 3) per1)
        (check-equal? (vector-ref (struct->vector pet2) 3) per2)
        (check-pred g:end? (do-row))))
    
    (test-case "make-cross-referencer: multiple cross references"
      (recreate-test-tables)
      (let* ([per1   (test-person 1 2 "Jon")]
             [per2   (test-person 2 3 "Lyman")]
             [pet1   (test-pet 1 2 (person-guid 1) "Garfield")]
             [pet2   (test-pet 2 3 (person-guid 2) "Odie")]
             [veh1   (test-vehicle 1 2 (person-guid 1) (pet-guid 1) "Red car")]
             [veh2   (test-vehicle 2 3 (person-guid 2) (pet-guid 2) "Red car")]
             [input  (list (list pet1 per1 veh1)
                           (list pet2 per2 veh2))]
             [xref*  (make-cross-referencer (append (entity-columns pet)
                                                    (entity-columns person)
                                                    (entity-columns vehicle))
                                            (list pet person vehicle)
                                            (build-hash (sql-list pet.owner        person.guid
                                                                  vehicle.owner    person.guid
                                                                  vehicle.occupant pet.guid)))]
             [xref   (cut xref* <> (transaction-frame-push #f))]
             [do-row (g:map xref (g:list input))])
        ; We use struct->vector and vector-ref below to avoid Snooze's default dereferencing behaviour:
        ; Cross-reference the first row:
        (check-cross-referenced (do-row) (list pet1 per1 veh1))
        (check-equal? (vector-ref (struct->vector pet1) 3) per1)
        (check-equal? (vector-ref (struct->vector veh1) 3) per1)
        (check-equal? (vector-ref (struct->vector veh1) 4) pet1)
        (check-equal? (vector-ref (struct->vector pet2) 3) (person-guid 2))
        (check-equal? (vector-ref (struct->vector veh2) 3) (person-guid 2))
        (check-equal? (vector-ref (struct->vector veh2) 4) (pet-guid 2))
        ; Cross-reference the second row:
        (check-cross-referenced (do-row) (list pet2 per2 veh2))
        (check-equal? (vector-ref (struct->vector pet1) 3) per1)
        (check-equal? (vector-ref (struct->vector veh1) 3) per1)
        (check-equal? (vector-ref (struct->vector veh1) 4) pet1)
        (check-equal? (vector-ref (struct->vector pet2) 3) per2)
        (check-equal? (vector-ref (struct->vector veh2) 3) per2)
        (check-equal? (vector-ref (struct->vector veh2) 4) pet2)
        (check-pred g:end? (do-row))))
    
    (test-case "make-cross-referencer: mixture of entities and single columns"
      (recreate-test-tables)
      (let-sql ([expr1 (sql pet.owner)]   ; Trying to confuse the cross-referencer
                [expr2 (sql person.guid)] ; Trying to confuse the cross-referencer
                [expr3 (sql pet.owner)])
        (let* ([per1   (test-person 1 2 "Jon")]
               [per2   (test-person 2 3 "Lyman")]
               [pet1   (test-pet 1 2 (person-guid 1) "Garfield")]
               [pet2   (test-pet 2 3 (person-guid 2) "Odie")]
               [input  (list (list (person-guid 1000) pet1 (person-guid 1200) per1 (person-guid 1230))
                             (list (person-guid 2000) pet2 (person-guid 2300) per2 (person-guid 2340)))]
               [xref*  (make-cross-referencer (append (list expr1)
                                                      (entity-columns pet)
                                                      (list expr2)
                                                      (entity-columns person)
                                                      (list expr3))
                                              (list type:integer pet type:integer person type:integer)
                                              (build-hash (sql-list pet.owner person.guid)))]
               [xref   (cut xref* <> (transaction-frame-push #f))]
               [do-row (g:map xref (g:list input))])
          ; We use struct->vector and vector-ref below to avoid Snooze's default dereferencing behaviour:
          ; Cross-reference the first row:
          (check-cross-referenced (do-row) (list (person-guid 1000) pet1 (person-guid 1200) per1 (person-guid 1230)))
          (check-equal? (vector-ref (struct->vector pet1) 3) per1)
          (check-equal? (vector-ref (struct->vector pet2) 3) (person-guid 2))
          ; Cross-reference the second row:
          (check-cross-referenced (do-row) (list (person-guid 2000) pet2 (person-guid 2300) per2 (person-guid 2340)))
          (check-equal? (vector-ref (struct->vector pet1) 3) per1)
          (check-equal? (vector-ref (struct->vector pet2) 3) per2)
          (check-pred g:end? (do-row)))))))

; Helpers ----------------------------------------

; list -> hasheq
(define (build-hash args)
  (let ([ans (make-hash)])
    (let loop ([args args])
      (match args
        [(list-rest key val rest)
         (hash-set! ans key val)
         (loop rest)]
        [(list) ans]))))

; Provide statements -----------------------------

(provide cross-reference-tests)
