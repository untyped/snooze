#lang scheme/base

(require mzlib/etc
         srfi/19
         srfi/26
         (planet untyped/unlib:3/gen)
         "extract.ss"
         "test-base.ss"
         "test-data.ss"
         "era/era.ss")

; Persistent structs ---------------------------

; integer integer string -> person
(define (test-person id revision name)
  (make-person #:guid (entity-make-guid person id) #:revision revision name))

; integer integer string boolean -> person
(define (test-pet id revision owner-id name)
  (make-pet #:guid (entity-make-guid pet id) #:revision revision owner-id name))

; Tests ----------------------------------------

(define extract-tests
  (test-suite "extract.ss"
    
    (test-case "struct-extractor: single item mode"
      (let* ([input    (list 1)]
             [extract  (make-struct-extractor #f (current-snooze))]
             [expected 1])
        (check-equal? (extract input) expected)))
    
    (test-case "struct-extractor: multi-item mode"
      (let* ([input    (list 1 2 3 "Dave" 4)]
             [extract  (make-struct-extractor (list #f person #f) (current-snooze))]
             [expected (list 1 (test-person 2 3 "Dave") 4)])
        (check-equal? (extract input) expected)))
    
    (test-case "struct-extractor: single-item in generator"
      (let* ([input   (list (list 1 2 "Dave")
                            (list 4 5 "Noel")
                            (list 7 8 "Matt"))]
             [extract (make-struct-extractor person (current-snooze))]
             [do-row  (g:map extract (g:list input))])
        (check-equal? (do-row) (test-person 1 2 "Dave"))
        (check-equal? (do-row) (test-person 4 5 "Noel"))
        (check-equal? (do-row) (test-person 7 8 "Matt"))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
    
    (test-case "struct-extractor: multi-item in generator"
      (let* ([input   (list (list 1 2 "Dave" 3)
                            (list 4 5 "Noel" 6)
                            (list 7 8 "Matt" 9))]
             [extract (make-struct-extractor (list person #f) (current-snooze))]
             [do-row  (g:map extract (g:list input))])
        (check-equal? (do-row) (list (test-person 1 2 "Dave") 3))
        (check-equal? (do-row) (list (test-person 4 5 "Noel") 6))
        (check-equal? (do-row) (list (test-person 7 8 "Matt") 9))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
    
    (test-case "struct-extractor: null results"
      (let* ([input   (list (list 1 2 "Dave" 3)
                              (list #f #f #f #f)
                              (list 3 4 "Noel" 5)
                              (list #f #f #f 6)
                              (list 7 8 "Matt" 9))]
             [extract (make-struct-extractor (list person #f) (current-snooze))]
             [do-row  (g:map extract (g:list input))])
        (check-equal? (do-row) (list (test-person 1 2 "Dave") 3))
        (check-equal? (do-row) (list #f #f))
        (check-equal? (do-row) (list (test-person 3 4 "Noel") 5))
        (check-equal? (do-row) (list #f 6))
        (check-equal? (do-row) (list (test-person 7 8 "Matt") 9))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
    
    (test-case "struct-extractor: extracting multiple structs"
      (begin-with-definitions
        
        (define input   (list (list 1 2 "Dave" #f #f #f #f)
                              (list 3 4 "Noel" 5 6 3 "William")
                              (list 3 4 "Noel" 7 8 3 "Henry")))
        (define extract (make-struct-extractor (list person pet) (current-snooze)))
        (define do-row  (g:map extract (g:list input)))
        
        (check-equal? (do-row) (list (test-person 1 2 "Dave") #f))
        (check-equal? (do-row) (list (test-person 3 4 "Noel") (test-pet 5 6 3 "William")))
        (check-equal? (do-row) (list (test-person 3 4 "Noel") (test-pet 7 8 3 "Henry")))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
    
    (test-case "struct-extractor: caching and re-using structs"
      (begin-with-definitions
        
        (define input    (list (list 1 2 "Dave" 28)
                               (list 5 6 "Noel" 29)
                               (list 9 0 "Matt" 30)
                               (list 1 2 "Dave" 28)
                               (list 5 6 "Noel" 29)
                               (list 9 0 "Matt" 30)))
        
        (define extract1 (make-struct-extractor person (current-snooze)))
        (define extract2 (make-struct-extractor (list person) (current-snooze)))
        
        (define do-row1 (g:map extract1 (g:list input)))
        (define do-row2 (g:map extract2 (g:list input)))
        
        (define dave1a (do-row1))
        (define noel1a (do-row1))
        (define matt1a (do-row1))
        (define dave1b (do-row1))
        (define noel1b (do-row1))
        (define matt1b (do-row1))
        
        (check-eq? dave1a dave1b)
        (check-eq? noel1a noel1b)
        (check-eq? matt1a matt1b)
        (check-pred g:end? (do-row1))
        
        (define dave2a (car (do-row2)))
        (define noel2a (car (do-row2)))
        (define matt2a (car (do-row2)))
        (define dave2b (car (do-row2)))
        (define noel2b (car (do-row2)))
        (define matt2b (car (do-row2)))
        
        (check-eq? dave2a dave2b)
        (check-eq? noel2a noel2b)
        (check-eq? matt2a matt2b)
        (check-pred g:end? (do-row2))))))

; Provide statements -----------------------------

(provide extract-tests)
