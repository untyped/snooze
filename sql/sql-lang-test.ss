#lang scheme/base

(require mzlib/etc
         (only-in srfi/1 unzip2)
         srfi/19
         srfi/26
         "../test-base.ss"
         "../test-data.ss"
         "../era/era.ss"
         "sql-alias.ss"
         (prefix-in sql: "sql-lang.ss")
         "sql-struct.ss"
         "sql-syntax.ss")

; Tests ------------------------------------------

; test-suite
(define sql-lang-tests
  (test-suite "sql-lang.ss"
    
    (test-case "alias"
      (begin-with-definitions
        
        (define p (make-entity-alias 'p person))
        (define q (make-query null #f p #f null null #f #f #f null null null))
        
        (check-equal? (sql:alias 'p person) p "entity")
        
        (check-equal? (sql:alias 'q q)
                      (make-query-alias 'q q)
                      "query")
        
        (check-equal? (sql:alias 'l (make-literal 1))
                      (make-expression-alias 'l (make-literal 1))
                      "expression")
        
        (check-exn exn:fail:contract?
          (cut sql:alias 'm (make-expression-alias 'l (make-literal 1)))
          "expression-alias")
        
        (check-equal? (sql:alias p (attr person name))
                      (make-attribute-alias p (attr person name))
                      "attribute")
        
        (check-exn exn:fail:contract?
          (cut sql:alias 'p-name p (attr pet name))
          "incorrect attribute")))
    
    (test-case "expressions : function arities and self-quoting literals"
      (begin-with-definitions
        
        (define-values (sql:ops args)
          (unzip2 (list (list sql:and                (list #t #f #t))
                        (list sql:or                 (list #t #f #t))
                        (list sql:not                (list #t))
                        (list sql:+                  (list 1 2.5 3))
                        (list sql:-                  (list 1 2.5 3))
                        (list sql:*                  (list 1 2.5 3))
                        (list sql:/                  (list 1 2.5))
                        (list sql:string-append      (list 'a "b" 'c))
                        (list sql:string-replace     (list "mat" "a" "e"))
                        (list sql:=                  (list #t 'a))
                        (list sql:<>                 (list #t 'a))
                        (list sql:<                  (list 1 2.5))
                        (list sql:>                  (list 'a "b"))
                        (list sql:<=                 (list (current-time time-tai) (current-time time-utc)))
                        (list sql:>=                 (list (current-time time-tai) (current-time time-utc)))
                        (list sql:like               (list "a" 'b))
                        (list sql:regexp-match       (list "a" 'b))
                        (list sql:regexp-match-ci    (list "a" 'b))
                        (list sql:regexp-replace     (list "a" 'b "c"))
                        (list sql:regexp-replace-ci  (list "a" 'b "c"))
                        (list sql:regexp-replace*    (list "a" 'b "c"))
                        (list sql:regexp-replace*-ci (list "a" 'b "c"))
                        (list sql:null?              (list #t)))))
        
        (map (lambda (sql:op args)
               (define literals (map sql:literal args))
               (check-equal? (apply sql:op args)
                             (apply sql:op literals)
                             (format "~s" (cons sql:op args))))
             sql:ops
             args)))
    
    (test-case "sql:in"
      (define-alias p person)
      (define-alias c course)
      
      (check-not-exn
        (cut sql:in 123 '(123 234 345))
        "valid list")
      
      (check-exn exn:fail:contract?
        (cut sql:in 123 '(#f 234 345))
        "invalid list (list items of different types)")
      
      (check-exn exn:fail:contract?
        (cut sql:in "123" '(123 234 345))
        "invalid list (arguments of different types)")
      
      (check-not-exn
        (cut sql:in 123 (sql:select #:what (sql c.value) #:from c))
        "valid integer subquery")
      
      (check-not-exn
        (cut sql:in (make-person "Dave") (sql:select #:what (sql p.guid) #:from p))
        "valid guid subquery")
      
      (check-exn exn:fail:contract?
        (cut sql:in 123 (sql:select #:from p))
        "invalid subquery (incorrect arity)")
      
      (check-exn exn:fail:contract?
        (cut sql:in "123" (sql:select #:what (sql p.guid) #:from p))
        "invalid subquery (arguments of different types)"))
    
    (test-case "sql:if and sql:cond"
      (check-equal? (sql:cond [#t "a"] [#f "b"] [else "c"])
                    (sql:if #t "a" (sql:if #f "b" "c"))
                    "else")
      (check-equal? (sql:cond [#t "a"] [#f "b"])
                    (sql:if #t "a" (sql:if #f "b"))
                    "no else")
      (check-true (integer-type? (expression-type (sql:cond [#t 1] [#f 2.0]))) "single type cond")
      (check-true (real-type? (expression-type (sql:cond [#t 1] [#f 2.1]))) "multiple type cond"))
    
    (test-case "joins: terms in join condition must be part of join operands"
      (begin-with-definitions
        
        (define-alias p1 person)
        (define-alias p2 person)
        (define-alias p3 person)
        
        (check-not-exn 
          (cut sql:inner p1 p2 (sql:= (sql p1.name) (sql p2.name)))
          "valid")
        
        (check-exn exn:fail:contract?
          (cut sql:inner p1 p3 (sql:= (sql p1.name) (sql p2.name)))
          "invalid")
        
        (check-exn exn:fail:contract?
          (cut sql:inner p1 p3 (sql:= (sql p1.name) (sql p2.name)))
          "tricky")))
    
    (test-case "select : single-item versus multi-item"
      (begin-with-definitions
        
        (define-alias p1 person)
        (define-alias p2 person)
        
        (define select1 (sql:select #:what (list (sql p1.name)) #:from p1))
        (define select2 (sql:select #:what (sql p1.name) #:from p1))
        
        (check-equal? (query-what select1) (query-what select2) "what")
        (check-equal? (query-extract-info select1) (list (query-extract-info select2)) "expand-info")))
    
    (test-case "select : no #:what"
      (begin-with-definitions
        
        (define p1 (sql:alias 'p1 person))
        (define p2 (sql:alias 'p2 pet))
        
        (define select1 (sql:select #:from p1))
        
        (check-equal? (query-extract-info (sql:select #:from p1))
                      person
                      "entity")
        
        (check-equal? (query-extract-info (sql:select #:from (sql:outer p1 p2)))
                      (list person pet)
                      "join")))
    
    (test-case "select : queries in #:from get quoted"
      (begin-with-definitions
        
        (define p1 (sql:alias 'p1 person))
        
        (check-pred query-alias?
                    (query-from (sql:select #:from (sql:select #:from p1)))
                    "query")))
    
    (test-case "select : members of #:what must be defined in #:from"
      (begin-with-definitions 
        
        (define-alias p1 person)
        (define-alias p2 person)
        (define-alias p1-count (sql:count* p1))
        
        (check-not-exn
          (cut sql:select #:what (sql p1.name) #:from p1)
          "correct entity")
        
        (check-not-exn
          (cut sql:select #:what (sql p1.name) #:from (sql:outer p1 p2))
          "join")
        
        (check-exn exn:fail:contract?
          (cut sql:select #:what (sql p1.name) #:from p2)
          "incorrect entity")
        
        (check-not-exn
          (cut sql:select #:what (sql p1.name) #:from (sql:select #:what (sql p1.name) #:from p1))
          "subquery with correct #:what")
        
        (check-exn exn:fail:contract?
          (cut sql:select #:what (sql p1.name) #:from (sql:select #:what (sql p1.guid) #:from p1))
          "subquery with incorrect #:what")
        
        (check-not-exn
          (cut sql:select #:what p1-count #:from (sql:select #:what p1-count #:from p1))
          "redeclared expression")))
    
    (test-case "select : members of #:distinct must be defined in #:from"
      (begin-with-definitions 
        
        (define-alias p1 person)
        (define-alias p2 person)
        (define-alias p1-count (sql:count* p1))
        
        (check-not-exn
          (cut sql:select #:distinct #t #:from p1)
          "no entity")
        
        (check-not-exn
          (cut sql:select #:distinct (sql:= (sql p1.name) "x") #:from p1)
          "single entity")
        
        (check-not-exn
          (cut sql:select #:distinct (sql:= (sql p1.name) (sql p2.name)) #:from (sql:outer p1 p2))
          "join")
        
        (check-exn exn:fail:contract?
          (cut sql:select #:distinct (sql:= (sql p1.name) "x") #:from p2)
          "incorrect entity")
        
        (check-not-exn
          (cut sql:select #:distinct (sql:= (sql p1.name) "x") #:from (sql:select #:what (sql p1.name) #:from p1))
          "subquery with correct #:what")
        
        (check-exn exn:fail:contract?
          (cut sql:select #:distinct (sql:= (sql p1.name) "x") #:from (sql:select #:what (sql p1.guid) #:from p1))
          "subquery with incorrect #:what")
        
        (check-not-exn
          (cut sql:select #:distinct (sql:= p1-count 1) #:from (sql:select #:what p1-count #:from p1))
          "redeclared expression")))
    
    (test-case "select : members of #:where must be defined in #:from"
      (begin-with-definitions
        
        (define-alias p1 person)
        (define-alias p2 person)
        
        (define p1-max-revision (sql:alias 'p1-max-revision (sql:max (sql p1.guid))))
        
        (check-not-exn
          (cut sql:select #:what (sql p1.guid) #:from p1 #:where (sql:= (sql p1.name) "Dave"))
          "entity")
        
        (check-not-exn
          (cut sql:select #:what (sql p1.guid) #:from (sql:outer p1 p2) #:where (sql:= (sql p1.name) "Dave"))
          "join")
        
        (check-not-exn
          (cut sql:select #:what (sql p1.guid) #:from (sql:select #:from p1) #:where (sql:= (sql p1.name) "Dave"))
          "subquery")
        
        (check-not-exn
          (cut sql:select 
               #:what  (sql p2.guid)
               #:from  (sql:outer (sql:select #:what p1-max-revision #:from p1) p2)
               #:where (sql:= (sql p2.guid) p1-max-revision))
          "expression alias")
        
        (check-exn exn:fail:contract?
          (cut sql:select #:from p2 #:where (sql:= (sql p1.name) "Dave"))
          "incorrect entity")))
    
    (test-case "select : members of #:order must be defined in #:what or #:from"
      (begin-with-definitions 
        
        (define-alias p1 person)
        (define-alias p2 person)
        
        (define-alias expr1 (sql:string-append (sql p1.name) "abc"))
        
        (check-not-exn
          (cut sql:select #:what (sql p1.guid) #:from p1 #:order (list (sql:asc (sql p1.name))))
          "entity")
        
        (check-exn exn:fail:contract?
          (cut sql:select #:what (sql p1.guid) #:from p1 #:order (list (sql:asc (sql p2.name))))
          "incorrect entity")
        
        (check-not-exn
          (cut sql:select
               #:what  (sql p1.guid)
               #:from  (sql:outer p1 p2)
               #:order (list (sql:asc (sql p1.name))))
          "join")
        
        (check-not-exn
          (cut sql:select
               #:what  (sql p1.guid)
               #:from  (sql:select #:from p1)
               #:order (list (sql:asc (sql p1.name))))
          "subquery")
        
        (check-exn exn:fail:contract?
          (cut sql:select
               #:what  (sql p2.name)
               #:from  (sql:select #:from p2)
               #:order (list (sql:asc (sql p1.name))))
          "incorrect subquery")
        
        (check-not-exn
          (cut sql:select
               #:what  (list (sql p1.guid) expr1)
               #:from  (sql:select #:from (sql:outer p1 p2))
               #:order (list (sql:asc expr1) (sql:desc (sql p1.guid)) (sql:asc (sql p2.guid))))
          "expressions and attributes in #:what")))
    
    (test-case "select : extract-info"
      (begin-with-definitions
        
        (define-alias p1 person)
        (define-alias p2 person)
        
        (define p1-count-id     (sql:count   (sql p1.guid)))
        (define p1-count        (sql:count*  p1))
        (define p1-max-revision (sql:max     (sql p1.revision)))
        (define p1-min-revision (sql:min     (sql p1.revision)))
        (define p1-average-id   (sql:average (sql p1.revision)))
        
        (check-equal? (query-extract-info (sql:select #:what (sql p1.revision) #:from p1))
                      (make-integer-type #f)
                      "single attribute")
        
        (check-equal? (query-extract-info (sql:select #:what (list (sql p1.revision) (sql p1.name)) #:from p1))
                      (list (make-integer-type #f)
                            (make-string-type #t #f))
                      "list of attributes")
        
        (check-equal? (query-extract-info (sql:select #:from p1))
                      person
                      "single entity")
        
        (check-equal? (query-extract-info (sql:select #:from (sql:outer p1 p2)))
                      (list person person)
                      "multiple entities")
        
        (check-equal? (query-extract-info (sql:select #:what (list p1-count-id
                                                                   p1-count
                                                                   p1-max-revision
                                                                   p1-min-revision
                                                                   p1-average-id)
                                                      #:from p1))
                      (list (make-integer-type #t)
                            (make-integer-type #t)
                            (make-integer-type #f)
                            (make-integer-type #f)
                            (make-real-type    #t))
                      "aggregates")
        
        (check-equal? (query-extract-info (sql:select #:what (list (sql:alias 'column1 (sql:+ (sql p1.revision) (sql p1.revision)))
                                                                   (sql:alias 'column2 (sql:+ p1-count-id 1.5))
                                                                   (sql:alias 'column3 (sql:string-append (sql p1.name) " rocks!"))
                                                                   (sql:alias 'column4 (sql:> (sql p1.revision) 123)))
                                                      #:from p1))
                      (list type:integer type:real type:string type:boolean)
                      "expressions")))))

; Provide statements -----------------------------

(provide sql-lang-tests)

