#lang scheme/base

(require "../test-base.ss")

(require srfi/19
         (unlib-in gen)
         "../test-data.ss"
         "../era/era.ss"
         "extract.ss")

; Persistent structs ---------------------------

; integer integer string -> person
(define (test-person id revision name)
  (make-person #:guid     (entity-make-vanilla-guid person id)
               #:revision revision name))

; integer integer string boolean -> person
(define (test-pet id revision owner-id name)
  (make-pet #:guid     (entity-make-vanilla-guid pet id)
            #:revision revision owner-id name))

; query -> (U single-item-extractor multiple-item-extractor)
(define (make-query-extractor query)
  (send (send (current-snooze) get-database) make-query-extractor query))

; (U entity #f) -> single-item-extractor
(define (make-single-item-extractor entity)
  (send (send (current-snooze) get-database) make-single-item-extractor entity))

; (listof (U entity #f)) -> multiple-item-extractor
(define (make-multiple-item-extractor entities)
  (send (send (current-snooze) get-database) make-multiple-item-extractor entities))

; Tests ----------------------------------------

(define extract-tests
  (test-suite "extract.ss"
    
    (test-case "make-single-item-struct-extractor : empty generator"
      (let* ([input    (list 1)]
             [extract  (make-single-item-extractor #f)]
             [expected 1])
        (check-equal? (extract input) expected)))
    
    (test-case "make-single-item-extractor: one row in generator"
      (let* ([input   (list (list 1 2 "Dave")
                            (list 4 5 "Noel")
                            (list 7 8 "Matt"))]
             [extract (make-single-item-extractor person)]
             [do-row  (g:map extract (g:list input))])
        (check-equal? (do-row) (test-person 1 2 "Dave"))
        (check-equal? (do-row) (test-person 4 5 "Noel"))
        (check-equal? (do-row) (test-person 7 8 "Matt"))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
    
    (test-case "make-multiple-item-extractor : empty generator"
      (let* ([input    (list 1 2 3 "Dave" 4)]
             [extract  (make-multiple-item-extractor (list #f person #f))]
             [expected (list 1 (test-person 2 3 "Dave") 4)])
        (check-equal? (extract input) expected)))
    
    (test-case "make-multiple-item-extractor : one row in generator"
      (let* ([input   (list (list 1 2 "Dave" 3)
                            (list 4 5 "Noel" 6)
                            (list 7 8 "Matt" 9))]
             [extract (make-multiple-item-extractor (list person #f))]
             [do-row  (g:map extract (g:list input))])
        (check-equal? (do-row) (list (test-person 1 2 "Dave") 3))
        (check-equal? (do-row) (list (test-person 4 5 "Noel") 6))
        (check-equal? (do-row) (list (test-person 7 8 "Matt") 9))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
    
    (test-case "make-multiple-item-extractor : null results"
      (let* ([input   (list (list 1 2 "Dave" 3)
                            (list #f #f #f #f)
                            (list 3 4 "Noel" 5)
                            (list #f #f #f 6)
                            (list 7 8 "Matt" 9))]
             [extract (make-multiple-item-extractor (list person #f))]
             [do-row  (g:map extract (g:list input))])
        (check-equal? (do-row) (list (test-person 1 2 "Dave") 3))
        (check-equal? (do-row) (list #f #f))
        (check-equal? (do-row) (list (test-person 3 4 "Noel") 5))
        (check-equal? (do-row) (list #f 6))
        (check-equal? (do-row) (list (test-person 7 8 "Matt") 9))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
    
    (test-case "make-multiple-item-extractor : extracting multiple structs"
      (let* ([input   (list (list 1 2 "Dave" #f #f #f #f)
                            (list 3 4 "Noel" 5 6 3 "William")
                            (list 3 4 "Noel" 7 8 3 "Henry"))]
             [extract (make-multiple-item-extractor (list person pet))]
             [do-row  (g:map extract (g:list input))])
        (check-equal? (do-row) (list (test-person 1 2 "Dave") #f))
        (check-equal? (do-row) (list (test-person 3 4 "Noel") (test-pet 5 6 3 "William")))
        (check-equal? (do-row) (list (test-person 3 4 "Noel") (test-pet 7 8 3 "Henry")))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
    
    (test-case "make-single-item-extractor : caching"
      (let* ([input (list (list 1 2 "Dave" 28)
                          (list 5 6 "Noel" 29)
                          (list 9 0 "Matt" 30)
                          (list 1 2 "Dave" 28)
                          (list 5 6 "Noel" 29)
                          (list 9 0 "Matt" 30))]
             
             [extract (make-single-item-extractor person)]
             [do-row  (g:map extract (g:list input))]
             
             [dave1 (do-row)]
             [noel1 (do-row)]
             [matt1 (do-row)]
             [dave2 (do-row)]
             [noel2 (do-row)]
             [matt2 (do-row)])
        
        (check-eq? dave1 dave2)
        (check-eq? noel1 noel2)
        (check-eq? matt1 matt2)
        (check-pred g:end? (do-row))))
    
    (test-case "make-multiple-item-extractor : caching"
      (let* ([input (list (list 1 2 "Dave" 28)
                          (list 5 6 "Noel" 29)
                          (list 9 0 "Matt" 30)
                          (list 1 2 "Dave" 28)
                          (list 5 6 "Noel" 29)
                          (list 9 0 "Matt" 30))]
             
             [extract (make-multiple-item-extractor (list person))]
             [do-row  (g:map extract (g:list input))]
             
             [dave1 (do-row)]
             [noel1 (do-row)]
             [matt1 (do-row)]
             [dave2 (do-row)]
             [noel2 (do-row)]
             [matt2 (do-row)])
        
        (check-eq? dave1 dave2)
        (check-eq? noel1 noel2)
        (check-eq? matt1 matt2)
        (check-pred g:end? (do-row))))))

; Provide statements -----------------------------

(provide extract-tests)
