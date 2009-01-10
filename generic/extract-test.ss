(module extract-test mzscheme
  
  (require (lib "time.ss" "srfi" "19")
           (lib "cut.ss" "srfi" "26"))
  
  (require (planet "gen.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "../era.ss")
           (file "../persistent-struct.ss")
           (file "../test-base.ss")
           (file "../test-data.ss")
           (file "../type.ss")
           (file "extract.ss")
           (file "sql.ss"))
  
  (provide extract-tests)
  
  ; Persistent structs ---------------------------
  
  ;; person : (struct string integer)
  (define-persistent-struct person
    ((name type:text)
     (age type:integer)))
  
  ;; motor : (struct string boolean)
  (define-persistent-struct motor
    ((model type:text)
     (blue? type:boolean)))
  
  ;; test-person : integer integer string integer -> person
  (define (test-person id revision name age)
    (let ([ans (make-person name age)])
      (set-id! ans id)
      (set-revision! ans revision)
      ans))
  
  ;; test-motor : integer integer string boolean -> person
  (define (test-motor id revision model blue?)
    (let ([ans (make-motor model blue?)])
      (set-id! ans id)
      (set-revision! ans revision)
      ans))
  
  ;; text-struct : integer integer persistent-struct -> persistent-struct
  (define (test-struct id revision struct)
    (set-id! struct id)
    (set-revision! struct revision)
    struct)
  
  ;;test-date : integer integer integer integer integer integer -> time-tai
  (define (test-time year month day hour minute second)
    (date->time-tai (make-srfi:date 0 second minute hour day month year 0)))
  
  ; Tests ----------------------------------------
  
  (define extract-tests
    (test-suite
     "extract.ss"
     
     ; struct-extractor: -----------------------------
     
     (test-case
      "struct-extractor: basic operation (list mode)"
      (check-equal? ((make-struct-extractor (list #f entity:programme #f) #f)
                     (vector 1 2 3 'C100 "Biology" 4))
                    (list 1 (test-struct 2 3 (make-programme 'C100 "Biology")) 4)
                    "extract a struct from struct/non-struct data"))
     
     (test-case
      "struct-extractor: basic operation (single item mode)"
      (check-equal? ((make-struct-extractor (list #f) #t)
                     (vector 1))
                    1)
      (check-equal? ((make-struct-extractor (list entity:programme) #t)
                     (vector 2 3 'C100 "Biology"))
                    (test-struct 2 3 (make-programme 'C100 "Biology"))
                    "extract a struct from struct/non-struct data"))
     
     (test-case
      "struct-extractor: returns g:end when there is no data"
      (let ([do-row (g:map (make-struct-extractor (list entity:person) #t)
                           (list->generator null))])
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row)))
      (let ([do-row (g:map (make-struct-extractor (list entity:person) #f)
                           (list->generator null))])
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
     
     (test-case
      "struct-extractor: extracts a single struct"
      (let ([do-row (g:map (make-struct-extractor (list entity:person) #f)
                           (list->generator (list (vector 1 2 "Dave" 28))))])
        (check-equal? (do-row) (list (test-person 1 2 "Dave" 28)))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row)))
      (let ([do-row (g:map (make-struct-extractor (list entity:person) #t)
                           (list->generator (list (vector 1 2 "Dave" 28))))])
        (check-equal? (do-row) (test-person 1 2 "Dave" 28))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))
        (check-pred g:end? (do-row))))
     
     (test-case
      "struct-extractor: can be called on successive rows"
      (let ([do-row (g:map (make-struct-extractor (list entity:person) #f)
                           (list->generator (list (vector 1 2 "Dave" 28)
                                                  (vector 2 3 "Noel" 29))))])
        (check-equal? (do-row) (list (test-person 1 2 "Dave" 28)))
        (check-equal? (do-row) (list (test-person 2 3 "Noel" 29)))
        (check-pred g:end? (do-row)))
      (let ([do-row (g:map (make-struct-extractor (list entity:person) #t)
                           (list->generator (list (vector 1 2 "Dave" 28)
                                                  (vector 2 3 "Noel" 29))))])
        (check-equal? (do-row) (test-person 1 2 "Dave" 28))
        (check-equal? (do-row) (test-person 2 3 "Noel" 29))
        (check-pred g:end? (do-row))))
     
     (test-case
      "struct-extractor: deals with null results"
      (let ([do-row (g:map (make-struct-extractor (list entity:person) #f)
                           (list->generator (list (vector 1 2 "Dave" 28)
                                                  (vector #f #f #f #f)
                                                  (vector 2 3 "Noel" 29))))])
        (check-equal? (do-row) (list (test-person 1 2 "Dave" 28)))
        (check-equal? (do-row) (list #f))
        (check-equal? (do-row) (list (test-person 2 3 "Noel" 29)))
        (check-pred g:end? (do-row)))
      (let ([do-row (g:map (make-struct-extractor (list entity:person) #t)
                           (list->generator (list (vector 1 2 "Dave" 28)
                                                  (vector #f #f #f #f)
                                                  (vector 2 3 "Noel" 29))))])
        (check-equal? (do-row) (test-person 1 2 "Dave" 28))
        (check-equal? (do-row) #f)
        (check-equal? (do-row) (test-person 2 3 "Noel" 29))
        (check-pred g:end? (do-row))))
     
     (test-case
      "struct-extractor: extracts rows of two structs effectively"
      (let ([do-row (g:map (make-struct-extractor (list entity:person entity:motor) #f)
                           (list->generator (list (vector 1 2 "Dave" 28 3 4 "Rover" #t)
                                                  (vector 5 6 "Noel" 29 7 8 "Micra" #f)
                                                  (vector 9 0 "Matt" 30 #f #f #f #f))))])
        (check-equal? (do-row) (list (test-person 1 2 "Dave" 28) (test-motor 3 4 "Rover" #t)))
        (check-equal? (do-row) (list (test-person 5 6 "Noel" 29) (test-motor 7 8 "Micra" #f)))
        (check-equal? (do-row) (list (test-person 9 0 "Matt" 30) #f))
        (check-pred g:end? (do-row))))
     
     (test-case
      "struct-extractor: caches and re-uses structs"
      (let ([do-row (g:map (make-struct-extractor (list entity:person) #f)
                           (list->generator (list (vector 1 2 "Dave" 28)
                                                  (vector 5 6 "Noel" 29)
                                                  (vector 9 0 "Matt" 30)
                                                  (vector 1 2 "Dave" 28)
                                                  (vector 5 6 "Noel" 29)
                                                  (vector 9 0 "Matt" 30))))])
        (define dave1 (car (do-row)))
        (define noel1 (car (do-row)))
        (define matt1 (car (do-row)))
        (define dave2 (car (do-row)))
        (define noel2 (car (do-row)))
        (define matt2 (car (do-row)))
        (check-eq? dave1 dave2      "check 1a")
        (check-eq? noel1 noel2      "check 2a")
        (check-eq? matt1 matt2      "check 3a")
        (check-pred g:end? (do-row) "check 4a"))
      (let ([do-row (g:map (make-struct-extractor (list entity:person) #t)
                           (list->generator (list (vector 1 2 "Dave" 28)
                                                  (vector 5 6 "Noel" 29)
                                                  (vector 9 0 "Matt" 30)
                                                  (vector 1 2 "Dave" 28)
                                                  (vector 5 6 "Noel" 29)
                                                  (vector 9 0 "Matt" 30))))])
        (define dave1 (do-row))
        (define noel1 (do-row))
        (define matt1 (do-row))
        (define dave2 (do-row))
        (define noel2 (do-row))
        (define matt2 (do-row))
        (check-eq? dave1 dave2      "check 1b")
        (check-eq? noel1 noel2      "check 2b")
        (check-eq? matt1 matt2      "check 3b")
        (check-pred g:end? (do-row) "check 4b")))
     
     ))
  
  )
 