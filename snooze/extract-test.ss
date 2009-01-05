#lang scheme/base

(require mzlib/etc
         srfi/19/time
         srfi/26/cut
         (planet untyped/unlib:3/gen)
         "persistent-struct.ss"
         "test-base.ss"
         "test-data.ss"
         "extract.ss"
         "era/era.ss")

; Persistent structs ---------------------------

; integer integer string -> person
(define (test-person id revision name)
  (define ans (make-person name))
  (set-struct-id! ans id)
  (set-struct-revision! ans revision)
  ans)

; integer integer string boolean -> person
(define (test-pet id revision owner-id name)
  (define ans (make-pet owner-id name))
  (set-struct-id! ans id)
  (set-struct-revision! ans revision)
  ans)

; Tests ----------------------------------------

(define extract-tests
  (test-suite "extract.ss"
    
    (test-case "struct-extractor: single item mode"
      (begin-with-definitions
        
        (define input    (vector 1))
        (define extract  (make-struct-extractor #f))
        (define expected 1)
        
        (check-equal? (extract input) expected "check 1")))
    
    (test-case "struct-extractor: multi-item mode"
      (begin-with-definitions
        
        (define input    (vector 1 2 3 "Dave" 4))
        (define extract  (make-struct-extractor (list #f entity:person #f)))
        (define expected (list 1 (test-person 2 3 "Dave") 4))
        
        (check-equal? (extract input) expected "check 1")))
    
    (test-case "struct-extractor: single-item in generator"
      (begin-with-definitions
        
        (define input   (list (vector 1 2 "Dave")
                              (vector 4 5 "Noel")
                              (vector 7 8 "Matt")))
        (define extract (make-struct-extractor entity:person))
        (define do-row  (g:map extract (g:list input)))
        
        (check-equal? (do-row) (test-person 1 2 "Dave") "check 1")
        (check-equal? (do-row) (test-person 4 5 "Noel") "check 2")
        (check-equal? (do-row) (test-person 7 8 "Matt") "check 3")
        (check-pred g:end? (do-row) "check 4")
        (check-pred g:end? (do-row) "check 5")))
    
    (test-case "struct-extractor: multi-item in generator"
      (begin-with-definitions
        
        (define input   (list (vector 1 2 "Dave" 3)
                              (vector 4 5 "Noel" 6)
                              (vector 7 8 "Matt" 9)))
        (define extract (make-struct-extractor (list entity:person #f)))
        (define do-row  (g:map extract (g:list input)))
        
        (check-equal? (do-row) (list (test-person 1 2 "Dave") 3) "check 1")
        (check-equal? (do-row) (list (test-person 4 5 "Noel") 6) "check 2")
        (check-equal? (do-row) (list (test-person 7 8 "Matt") 9) "check 3")
        (check-pred g:end? (do-row) "check 4")
        (check-pred g:end? (do-row) "check 5")))
    
    (test-case "struct-extractor: null results"
      (begin-with-definitions
        
        (define input   (list (vector 1 2 "Dave" 3)
                              (vector #f #f #f #f)
                              (vector 3 4 "Noel" 5)
                              (vector #f #f #f 6)
                              (vector 7 8 "Matt" 9)))
        (define extract (make-struct-extractor (list entity:person #f)))
        (define do-row  (g:map extract (g:list input)))
        
        (check-equal? (do-row) (list (test-person 1 2 "Dave") 3) "check 1")
        (check-equal? (do-row) (list #f #f) "check 2")
        (check-equal? (do-row) (list (test-person 3 4 "Noel") 5) "check 3")
        (check-equal? (do-row) (list #f 6) "check 4")
        (check-equal? (do-row) (list (test-person 7 8 "Matt") 9) "check 5")
        (check-pred g:end? (do-row) "check 6")
        (check-pred g:end? (do-row) "check 7")))
    
    (test-case "struct-extractor: extracting multiple structs"
      (begin-with-definitions
        
        (define input   (list (vector 1 2 "Dave" #f #f #f #f)
                              (vector 3 4 "Noel" 5 6 3 "William")
                              (vector 3 4 "Noel" 7 8 3 "Henry")))
        (define extract (make-struct-extractor (list entity:person entity:pet)))
        (define do-row  (g:map extract (g:list input)))
        
        (check-equal? (do-row) (list (test-person 1 2 "Dave") #f) "check 1")
        (check-equal? (do-row) (list (test-person 3 4 "Noel") (test-pet 5 6 3 "William")) "check 2")
        (check-equal? (do-row) (list (test-person 3 4 "Noel") (test-pet 7 8 3 "Henry")) "check 3")
        (check-pred g:end? (do-row) "check 4")
        (check-pred g:end? (do-row) "check 5")))
    
    (test-case "struct-extractor: caching and re-using structs"
      (begin-with-definitions
        
        (define input    (list (vector 1 2 "Dave" 28)
                               (vector 5 6 "Noel" 29)
                               (vector 9 0 "Matt" 30)
                               (vector 1 2 "Dave" 28)
                               (vector 5 6 "Noel" 29)
                               (vector 9 0 "Matt" 30)))
        
        (define extract1 (make-struct-extractor entity:person))
        (define extract2 (make-struct-extractor (list entity:person)))
        
        (define do-row1 (g:map extract1 (g:list input)))
        (define do-row2 (g:map extract2 (g:list input)))
        
        (define dave1a (do-row1))
        (define noel1a (do-row1))
        (define matt1a (do-row1))
        (define dave1b (do-row1))
        (define noel1b (do-row1))
        (define matt1b (do-row1))
        
        (check-eq? dave1a dave1b "check 1")
        (check-eq? noel1a noel1b "check 2")
        (check-eq? matt1a matt1b "check 3")
        (check-pred g:end? (do-row1) "check 4")
        
        (define dave2a (car (do-row2)))
        (define noel2a (car (do-row2)))
        (define matt2a (car (do-row2)))
        (define dave2b (car (do-row2)))
        (define noel2b (car (do-row2)))
        (define matt2b (car (do-row2)))
        
        (check-eq? dave2a dave2b "check 5")
        (check-eq? noel2a noel2b "check 6")
        (check-eq? matt2a matt2b "check 7")
        (check-pred g:end? (do-row2) "check 8")))))

; Provide statements -----------------------------

(provide extract-tests)
