#lang scheme/base

(require "../test-base.ss")

(require srfi/19
         (unlib-in gen)
         "../core/era.ss"
         "extract.ss")

; Persistent structs ---------------------------

; integer integer string -> person
(define (test-person id revision name)
  ((entity-private-constructor person)
   (entity-make-vanilla-guid person id)
   revision
   name))

; integer integer string boolean -> person
(define (test-pet id revision owner-guid name)
  ((entity-private-constructor pet)
   (entity-make-vanilla-guid pet id)
   revision
   owner-guid
   name))

; natural -> guid
(define make-person-guid
  (cut entity-make-vanilla-guid person <>))

; natural -> guid
(define make-pet-guid
  (cut entity-make-vanilla-guid pet <>))

; query -> (U single-item-extractor multiple-item-extractor)
(define (make-query-extractor query)
  (send (send (current-snooze) get-database) make-query-extractor query))

; (U entity #f) -> single-item-extractor
(define (make-single-item-extractor entity)
  (send (send (current-snooze) get-database) make-single-item-extractor entity))

; (listof (U entity #f)) -> multiple-item-extractor
(define (make-multiple-item-extractor entities)
  (send (send (current-snooze) get-database) make-multiple-item-extractor entities))

(define-check (check-extracted actual expected)
  (cond [(pair? actual)
         (for ([actual   (in-list actual)]
               [expected (in-list expected)])
           (if (guid? actual)
               (check-equal? (guid-ref actual) expected)
               (check-equal? actual expected)))]
        [(guid? actual)
         (check-equal? (guid-ref actual) expected)]
        [else
         (check-equal? actual expected)]))

; Tests ----------------------------------------

(define extract-tests
  (test-suite "extract.ss"
    
    (test-case "make-single-item-struct-extractor : empty generator"
      (recreate-test-tables/cache)
      (let* ([input    (list 1)]
             [extract  (make-single-item-extractor #f)]
             [expected 1])
        (check-extracted (extract input) expected)))
    
    (test-case "make-single-item-extractor: one row in generator"
      (recreate-test-tables/cache)
      (let* ([input   (list (list (make-person-guid 1) 2 "Dave")
                            (list (make-person-guid 4) 5 "Noel")
                            (list (make-person-guid 7) 8 "Matt"))]
             [extract (make-single-item-extractor person)]
             [do-row  (g:map extract (g:list input))])
        (check-extracted (do-row) (test-person 1 2 "Dave"))
        (check-extracted (do-row) (test-person 4 5 "Noel"))
        (check-extracted (do-row) (test-person 7 8 "Matt"))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
    
    (test-case "make-multiple-item-extractor : empty generator"
      (recreate-test-tables/cache)
      (let* ([input    (list 1 (make-person-guid 2) 3 "Dave" 4)]
             [extract  (make-multiple-item-extractor (list #f person #f))]
             [expected (list 1 (test-person 2 3 "Dave") 4)])
        (check-extracted (extract input) expected)))
    
    (test-case "make-multiple-item-extractor : one row in generator"
      (recreate-test-tables/cache)
      (let* ([input   (list (list (make-person-guid 1) 2 "Dave" 3)
                            (list (make-person-guid 4) 5 "Noel" 6)
                            (list (make-person-guid 7) 8 "Matt" 9))]
             [extract (make-multiple-item-extractor (list person #f))]
             [do-row  (g:map extract (g:list input))])
        (check-extracted (do-row) (list (test-person 1 2 "Dave") 3))
        (check-extracted (do-row) (list (test-person 4 5 "Noel") 6))
        (check-extracted (do-row) (list (test-person 7 8 "Matt") 9))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
    
    (test-case "make-multiple-item-extractor : null results"
      (recreate-test-tables/cache)
      (let* ([input   (list (list (make-person-guid 1) 2 "Dave" 3)
                            (list #f #f #f #f)
                            (list (make-person-guid 3) 4 "Noel" 5)
                            (list #f #f #f 6)
                            (list (make-person-guid 7) 8 "Matt" 9))]
             [extract (make-multiple-item-extractor (list person #f))]
             [do-row  (g:map extract (g:list input))])
        (check-extracted (do-row) (list (test-person 1 2 "Dave") 3))
        (check-extracted (do-row) (list #f #f))
        (check-extracted (do-row) (list (test-person 3 4 "Noel") 5))
        (check-extracted (do-row) (list #f 6))
        (check-extracted (do-row) (list (test-person 7 8 "Matt") 9))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
    
    (test-case "make-multiple-item-extractor : extracting multiple structs"
      (recreate-test-tables/cache)
      (let* ([input   (list (list (make-person-guid 1) 2 "Dave" #f #f #f #f)
                            (list (make-person-guid 3) 4 "Noel" (intern-guid (make-pet-guid 5)) 6 (intern-guid (make-person-guid 3)) "William")
                            (list (make-person-guid 3) 4 "Noel" (intern-guid (make-pet-guid 7)) 8 (intern-guid (make-person-guid 3)) "Henry"))]
             [extract (make-multiple-item-extractor (list person pet))]
             [do-row  (g:map extract (g:list input))])
        (check-extracted (do-row) (list (test-person 1 2 "Dave") #f))
        (check-extracted (do-row) (list (test-person 3 4 "Noel") (test-pet 5 6 (intern-guid (make-person-guid 3)) "William")))
        (check-extracted (do-row) (list (test-person 3 4 "Noel") (test-pet 7 8 (intern-guid (make-person-guid 3)) "Henry")))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
    
    (test-case "make-single-item-extractor : caching"
      (recreate-test-tables/cache)
      (let* ([input (list (list (make-person-guid 1) 2 "Dave" 28)
                          (list (make-person-guid 5) 6 "Noel" 29)
                          (list (make-person-guid 9) 0 "Matt" 30)
                          (list (make-person-guid 1) 2 "Dave" 28)
                          (list (make-person-guid 5) 6 "Noel" 29)
                          (list (make-person-guid 9) 0 "Matt" 30))]
             
             [extract (make-single-item-extractor person)]
             [do-row  (g:map extract (g:list input))]
             
             [dave1 (do-row)]
             [noel1 (do-row)]
             [matt1 (do-row)]
             [dave2 (do-row)]
             [noel2 (do-row)]
             [matt2 (do-row)])
        
        (check-equal? dave1 dave2)
        (check-equal? noel1 noel2)
        (check-equal? matt1 matt2)
        (check-pred g:end? (do-row))))
    
    (test-case "make-multiple-item-extractor : caching"
      (recreate-test-tables/cache)
      (let* ([input (list (list (make-person-guid 1) 2 "Dave" 28)
                          (list (make-person-guid 5) 6 "Noel" 29)
                          (list (make-person-guid 9) 0 "Matt" 30)
                          (list (make-person-guid 1) 2 "Dave" 28)
                          (list (make-person-guid 5) 6 "Noel" 29)
                          (list (make-person-guid 9) 0 "Matt" 30))]
             
             [extract (make-multiple-item-extractor (list person))]
             [do-row  (g:map extract (g:list input))]
             
             [dave1 (do-row)]
             [noel1 (do-row)]
             [matt1 (do-row)]
             [dave2 (do-row)]
             [noel2 (do-row)]
             [matt2 (do-row)])
        
        (check-equal? dave1 dave2)
        (check-equal? noel1 noel2)
        (check-equal? matt1 matt2)
        (check-pred g:end? (do-row))))))

; Provide statements -----------------------------

(provide extract-tests)
