#lang scheme/base

(require mzlib/etc
         scheme/class
         scheme/match
         srfi/19/time
         srfi/26/cut
         (planet untyped/unlib:3/time)
         "../snooze.ss"
         "../snooze-syntax.ss"
         "../test-base.ss"
         "audit.ss")

; Tests ----------------------------------------

(define-snooze-struct audit-metadata
  ([transaction-id (make-integer-type #f #f)]
   [message        (make-string-type #t #f 256)]))

; snooze% -> test-suite
(define (make-audit-tests snooze)
  (define-snooze-interface snooze)
  
  ; audit-trail<%>
  (define trail 
    (new (class audit-trail%
           ; audit-transaction string -> audit-metadata
           (define/override (make-metadata txn message)
             (make-audit-metadata (struct-id txn) message))
           (super-new))
         [snooze   snooze]
         [entities (list course person pet)]))
  
  (define-audit-interface trail)
  
  (define-alias ENTITY audit-entity)
  (define-alias ATTR   audit-attribute)
  (define-alias TXN    audit-transaction)
  (define-alias META   audit-metadata)
  (define-alias DELTA  audit-delta)
  
  ; -> (listof audit-attribute)
  (define (find-attrs)
    (find-all (sql:select #:from ATTR)))
  
  ; -> (listof audit-transaction)
  (define (find-txns)
    (find-all (sql:select #:from TXN #:order (list (sql:asc TXN-id)))))
  
  ; -> (listof audit-metadata)
  (define (find-metas)
    (find-all (sql:select #:from META #:order (list (sql:asc META-transaction-id)))))
  
  ; -> (listof audit-delta)
  (define (find-deltas)
    (find-all (sql:select #:from DELTA #:order (list (sql:asc DELTA-id)))))
  
  ; -> void
  (define (clear-trail!)
    (send trail clear!)
    (for-each delete! (find-metas)))
  
  ; entity attribute snooze-struct -> (listof any)
  (define (find-history entity att struct)
    (map (cut audit-delta-value <> (attribute-type att))
         (find-all (sql:select #:what  DELTA
                               #:from  (sql:inner (sql:inner ENTITY ATTR (sql:= ENTITY-id ATTR-entity-id))
                                                  DELTA (sql:= ATTR-id DELTA-attribute-id))
                               #:where (sql:and (sql:= ENTITY-name (entity-table-name entity))
                                                (sql:= ATTR-name (attribute-column-name att))
                                                (sql:= DELTA-struct-id (struct-id struct)))
                               #:order (list (sql:asc DELTA-id))))))
  
  ; test-suite
  (test-suite "audit"
    
    #:before
    (lambda ()
      (drop-all-tables)
      (send trail init!)
      (create-table audit-metadata)
      (for-each create-table (list course person pet)))
    
    #:after
    drop-all-tables
    
    ; Initialising -------------------------------
    
    (test-case "init-audit-trail!"
      (check-true (table-exists? audit-attribute))
      (check-true (table-exists? audit-transaction))
      (check-true (table-exists? audit-delta))
      (check-true (procedure? (send snooze get-transaction-hook)))
      (check-true (procedure? (entity-delete-hook person)))
      (check-true (procedure? (entity-delete-hook pet))))
    
    ; Auditing changes ---------------------------
    
    #;(test-case "audit-attributes generated correctly"
        (fail "Not implemented."))
    
    (test-case "audit basic insert, update and delete"
      (begin-with-definitions
        (clear-trail!)
        
        (check-pred null? (find-txns))
        (check-pred null? (find-deltas))
        
        (define person1 (save! (make-person "Dave")))
        
        (check-equal? (length (find-txns)) 1)
        (check-equal? (length (find-deltas)) 1)
        
        (define person2 (save! (copy-person person1 #:name "Noel")))
        
        (check-equal? (length (find-txns)) 2)
        (check-equal? (length (find-deltas)) 2)
        
        (delete! person2)
        
        (check-equal? (length (find-txns)) 3)
        (check-equal? (length (find-deltas)) 3)))
    
    (test-case "audit sequence"
      (begin-with-definitions
        (clear-trail!)
        
        (define-values (person1 person2)
          (apply values (call-with-transaction
                         (lambda ()
                           (list (save! (make-person "Dave"))
                                 (save! (make-person "Noel"))))
                         "0")))
        
        (define deltas (find-deltas))
        
        (check-equal? (audit-delta-struct-id (car deltas))  (struct-id person1))
        (check-equal? (audit-delta-struct-id (cadr deltas)) (struct-id person2))))
    
    (test-case "audit the different attribute types"
      (begin-with-definitions
        (define time (string->time-tai "2001-01-01 01:01:01"))
        (define course1 (save! (make-course 'COURSE "Course" 123 1.23 #t time)))
        
        (clear-trail!)
        
        (save! (copy-course course1
                            #:code   'ESRUOC
                            #:name   "esruoC"
                            #:value  321
                            #:rating 3.21
                            #:active #f
                            #:start  (current-time time-tai)))
        
        (check-equal? (find-history course (attr course id)       course1) (list))
        (check-equal? (find-history course (attr course revision) course1) (list))
        (check-equal? (find-history course (attr course code)     course1) (list 'COURSE))
        (check-equal? (find-history course (attr course name)     course1) (list "Course"))
        (check-equal? (find-history course (attr course value)    course1) (list 123))
        (check-equal? (find-history course (attr course rating)   course1) (list 1.23))
        (check-equal? (find-history course (attr course start)    course1) (list time))
        
        (check-true (andmap (lambda (delta)
                              (eq? (audit-delta-struct-id delta) (struct-id course1)))
                            (find-deltas)))
        
        (check-true (andmap (lambda (delta)
                              (eq? (audit-delta-struct-revision delta) (struct-revision course1)))
                            (find-deltas)))))
    
    (test-case "audit insert/update sequence correctly summarised"
      (begin-with-definitions
        
        (clear-trail!)
        
        (define person1
          (call-with-transaction
           (lambda ()
             (let ([ans (save! (make-person "Dave"))])
               (save! (copy-person ans #:name "Noel"))))
           "insert/update sequence"))
        
        (define txns (find-txns))
        (define metas (find-metas))
        (define deltas (find-deltas))
        
        (check-equal? (length txns) 1)
        (check-equal? (audit-metadata-message (car metas)) "insert/update sequence")
        
        (check-equal? (length deltas) 2)
        (check-equal? (car deltas)
                      (copy-audit-delta (make-insert-delta (car txns) (struct-guid person1))
                                        #:id       (struct-id (car deltas))
                                        #:revision 0))
        (check-equal? (cadr deltas)
                      (copy-audit-delta (make-update-delta (car txns) (struct-guid person1) 0 (attr person name) "Dave")
                                        #:id       (struct-id (cadr deltas))
                                        #:revision 0))))
    
    (test-case "insert/delete sequence correctly summarised"
      (begin-with-definitions
        
        (clear-trail!)
        
        (define person1
          (call-with-transaction
           (lambda ()
             (define ans (save! (make-person "Dave")))
             (delete! (copy-person ans))
             ans)
           "insert/delete sequence"))
        
        (define txns (find-txns))
        (define metas (find-metas))
        (define deltas (find-deltas))
        
        (check-equal? (length txns) 1)
        (check-equal? (audit-metadata-message (car metas)) "insert/delete sequence")
        
        (check-equal? (length deltas) 2)
        (check-equal? (car deltas)
                      (copy-audit-delta (make-insert-delta (car txns) (struct-guid person1))
                                        #:id       (struct-id (car deltas))
                                        #:revision 0))
        (check-equal? (cadr deltas)
                      (copy-audit-delta (make-delete-delta (car txns) (struct-guid person1) 0 (attr person name) "Dave")
                                        #:id       (struct-id (cadr deltas))
                                        #:revision 0))))
    
    (test-case "update/update sequence correctly summarised"
      (begin-with-definitions
        
        (clear-trail!)
        
        (define person1
          (let ([ans (save! (make-person "Dave"))])
            (clear-trail!)
            (call-with-transaction
             (lambda ()
               (save! (copy-person (save! (copy-person ans #:name "Noel")) #:name "Matt")))
             "update/update sequence")
            ans))
        
        (define txns (find-txns))
        (define metas (find-metas))
        (define deltas (find-deltas))
        
        (check-equal? (length txns) 1)
        (check-equal? (audit-metadata-message (car metas)) "update/update sequence")
        (check-equal? (length deltas) 2)
        (check-equal? (car deltas)
                      (copy-audit-delta (make-update-delta (car txns) (struct-guid person1) 0 (attr person name) "Dave")
                                        #:id       (struct-id (car deltas))
                                        #:revision 0))
        (check-equal? (cadr deltas)
                      (copy-audit-delta (make-update-delta (car txns) (struct-guid person1) 1 (attr person name) "Noel")
                                        #:id       (struct-id (cadr deltas))
                                        #:revision 0))))
    
    (test-case "audit update/delete sequence correctly summarised"
      (begin-with-definitions
        
        (clear-trail!)
        
        (define person1
          (let ([ans (save! (make-person "Dave"))])
            (clear-trail!)
            (call-with-transaction
             (lambda ()
               (delete! (save! (copy-person ans #:name "Noel"))))
             "update/delete sequence")
            ans))
        
        (define txns (find-txns))
        (define metas (find-metas))
        (define deltas (find-deltas))
        
        (check-equal? (length txns) 1)
        (check-equal? (audit-metadata-message (car metas)) "update/delete sequence")
        (check-equal? (length deltas) 2)
        (check-equal? (car deltas)
                      (copy-audit-delta (make-update-delta (car txns) (struct-guid person1) 0 (attr person name) "Dave")
                                        #:id       (struct-id (car deltas))
                                        #:revision 0))
        (check-equal? (cadr deltas)
                      (copy-audit-delta (make-delete-delta (car txns) (struct-guid person1) 1 (attr person name) "Noel")
                                        #:id       (struct-id (cadr deltas))
                                        #:revision 0))))
    
    (test-case "audit trail not written when transaction aborted"
      (before (clear-trail!)
              (begin-with-definitions
                
                (let/ec escape
                  (call-with-transaction
                   (lambda ()
                     (save! (make-person "Dave"))
                     (escape #f))
                   "aborted with escape continuation"))
                
                (define txns (find-txns))
                (define metas (find-metas))
                (define deltas (find-deltas))
                
                (check-equal? (length txns) 0)
                (check-equal? (length deltas) 0))))
    
    (test-case "nested transaction audited at outermost transaction"
      (before (clear-trail!)
              (begin-with-definitions
                
                (let/ec escape
                  (call-with-transaction
                   (lambda ()
                     (save! (make-person "Dave"))
                     (call-with-transaction
                      (lambda ()
                        (save! (make-person "Noel")))
                      "first inner")
                     (call-with-transaction
                      (lambda ()
                        (save! (make-person "Matt")))
                      "second inner"))
                   "outer"))
                
                (define txns (find-txns))
                (define metas (find-metas))
                (define deltas (find-deltas))
                
                (check-equal? (length txns) 1)
                (check-equal? (length deltas) 3)
                
                (check-true (andmap (lambda (delta)
                                      (equal? (audit-delta-transaction-id delta)
                                              (audit-transaction-id (car txns))))
                                    deltas)))))
    
    (test-case "inner transaction aborted"
      (begin-with-definitions
        
        (define dave (save! (make-person "Dave")))
        (define noel (save! (make-person "Noel")))
        
        (clear-trail!)
        
        (call-with-transaction
         (lambda ()
           (call-with-transaction
            (lambda ()
              (set-person-name! dave "Dave 2")
              (save! dave))
            "Inner 2")
           (let/ec escape
             (call-with-transaction
              (lambda ()
                (set-person-name! dave "Dave 3")
                (set-person-name! noel "Noel 2")
                (save! dave)
                (save! noel)
                (escape #f))
              "Inner 1"))
           "Outer"))
        
        (define txns (find-txns))
        (define metas (find-metas))
        (define deltas (find-deltas))
        
        (check-equal? (length txns) 1) ; Outer transaction only
        (check-equal? (length deltas) 1)
        
        (check-true (andmap (lambda (delta)
                              (= (audit-delta-struct-id delta) (struct-id dave)))
                            deltas))
        
        (check-true (andmap (lambda (delta)
                              (= (audit-delta-struct-revision delta) (sub1 (struct-revision dave))))
                            deltas))
        
        (check-equal? (person-name dave) "Dave 2") ; Make sure structs are rolled back correctly
        (check-equal? (person-name noel) "Noel"))) ; Make sure structs are rolled back correctly
    
    ; Querying and rollback ----------------------
    
    (test-case "audit-transaction-deltas"
      (before (clear-trail!)
              (begin-with-definitions
                (define person1 (save! (make-person "Dave")))
                
                (define txns (find-txns))
                (define metas (find-metas))
                (define person1-txn (car txns))
                
                (define deltas (audit-transaction-deltas person1-txn))
                
                (check-equal? deltas (find-deltas)))))
    
    (test-case "id->attribute"
      (before (clear-trail!)
              (begin-with-definitions
                (define person1 (save! (make-person "Dave")))
                (save! (copy-person person1 #:name "Noel"))
                
                ; audit-attribute
                (define audit-attr (car (find-attrs)))
                
                ; attribute
                (define att (id->attribute (struct-id audit-attr)))
                
                (check-eq? att (attr person name)))))
    
    (test-case "audit-deltas->guids"
      (begin-with-definitions
        (define noel (save! (make-person "Noel")))
        (define william (save! (make-pet (struct-id noel) "William")))
        (define henry (save! (make-pet (struct-id noel) "Henry")))
        
        (clear-trail!)
        
        (call-with-transaction
         (lambda ()
           (set-pet-name! william "Henry")
           (set-pet-name! henry "William")
           (save! william)
           (save! henry))
         "Swapping cat names")
        
        ; audit-transaction
        (define txn (car (find-txns)))
        
        ; (listof audit-delta)
        (define deltas (audit-transaction-deltas txn))
        
        ; (listof guid)
        (define guids (audit-deltas->guids deltas))
        
        (check-not-false (member (struct-guid william) guids))
        (check-not-false (member (struct-guid henry) guids))
        (check-false (member (struct-guid noel) guids))))
    
    (test-case "audit-struct-history"
      (begin-with-definitions
        (clear-trail!)
        
        (define noel (save! (make-person "Noel")))
        (set-person-name! noel "Dave")
        (save! noel)
        (set-person-name! noel "Matt")
        (save! noel)
        (save! (make-person "Bree"))
        (save! (make-pet (struct-id noel) "William"))
        (save! (make-pet (struct-id noel) "Henry"))
        
        ; audit-transaction ; the INSERT transaction
        (define txn (car (find-txns)))
        
        ; (listof audit-delta)
        (define history (audit-struct-history (struct-guid noel) txn))
        
        ; (listof audit-delta)
        (define insert-history 
          (filter (lambda (delta) (equal? (audit-delta-type delta) 'I))
                  history))
        
        ; (listof audit-delta)
        (define id-history 
          (filter (lambda (delta) (equal? (audit-delta-attribute delta) (attr person id)))
                  history))
        
        ; (listof audit-delta)
        (define revision-history 
          (filter (lambda (delta) (equal? (audit-delta-attribute delta) (attr person revision)))
                  history))
        
        ; (listof audit-delta)
        (define name-history 
          (filter (lambda (delta) (equal? (audit-delta-attribute delta) (attr person name)))
                  history))
        
        (check-equal? (length history) 3)
        (check-equal? (length insert-history) 1)
        (check-equal? (audit-delta-attribute (car insert-history)) #f)
        (check-equal? (length id-history) 0)
        (check-equal? (length revision-history) 0)
        (check-equal? (length name-history) 2)
        (check-equal? (map (cut audit-delta-value <> type:string) name-history) (list "Dave" "Noel"))))
    
    (test-case "revert-delta!: single update"
      (begin-with-definitions
        (define noel (save! (make-person "Noel")))
        
        (define noel-id (struct-id noel))
        
        (clear-trail!)
        
        (set-person-name! noel "Dave")
        (save! noel)
        
        ; audit-transaction ; the UPDATE transaction
        (define txn (car (find-txns)))
        
        ; (listof audit-delta)
        (define history (audit-struct-history (struct-guid noel) txn))
        
        (foldl (cut revert-delta! (struct-guid noel) <> <>)
               noel
               history)
        
        (check-equal? (person-name noel) "Noel")
        (check-equal? (struct-revision noel) 1)
        (check-equal? (struct-id noel) noel-id)))
    
    (test-case "revert-delta!: insert, update and delete"
      (begin-with-definitions
        (define noel (save! (make-person "Noel")))
        (define dave (save! (make-person "Dave")))
        
        (define noel-id (struct-id noel))
        (define dave-id (struct-id dave))
        
        (define noel-guid (struct-guid noel))
        (define dave-guid (struct-guid dave))
        
        (clear-trail!)
        
        (set-person-name! noel "Noel 2.0")
        (save! noel)
        (delete! dave)
        (define matt (save! (make-person "Matt"))) 
        (define matt-guid (struct-guid matt))
        
        ; audit-transaction ; the UPDATE transaction
        (define txn (car (find-txns)))
        
        (define original-noel (foldl (cut revert-delta! noel-guid <> <>) noel (audit-struct-history noel-guid txn)))
        (define original-dave (foldl (cut revert-delta! dave-guid <> <>) dave (audit-struct-history dave-guid txn)))
        (define original-matt (foldl (cut revert-delta! matt-guid <> <>) matt (audit-struct-history matt-guid txn)))
        
        (check-equal? (person-name original-noel) "Noel")
        (check-equal? (struct-revision original-noel) 1)
        (check-equal? (struct-id original-noel) noel-id)
        (check-equal? (person-name original-dave) "Dave")
        (check-equal? (struct-revision original-dave) 0)
        (check-equal? (struct-id original-dave) dave-id)
        (check-equal? original-matt #f)))
    
    (test-case "audit-snapshot"
      (begin-with-definitions
        (define noel (save! (make-person "Noel")))
        (define dave (save! (make-person "Dave")))
        
        (define noel-id (struct-id noel))
        (define dave-id (struct-id dave))
        
        (define noel-guid (struct-guid noel))
        (define dave-guid (struct-guid dave))
        
        (clear-trail!)
        
        (set-person-name! noel "Noel 2.0")
        (save! noel)
        (delete! dave)
        (define matt (save! (make-person "Matt"))) 
        (define matt-guid (struct-guid matt))
        
        ; audit-transaction ; the UPDATE transaction
        (define txn (car (find-txns)))
        
        (define original-noel (audit-struct-snapshot noel-guid (audit-struct-history noel-guid txn)))
        (define original-dave (audit-struct-snapshot dave-guid (audit-struct-history dave-guid txn)))
        (define original-matt (audit-struct-snapshot matt-guid (audit-struct-history matt-guid txn)))
        
        (check-equal? (person-name original-noel) "Noel")
        (check-equal? (struct-revision original-noel) 1)
        (check-equal? (struct-id original-noel) noel-id)
        (check-equal? (person-name original-dave) "Dave")
        (check-equal? (struct-revision original-dave) 0)
        (check-equal? (struct-id original-dave) dave-id)
        (check-equal? original-matt #f)
        
        (define intermediate-noel (audit-struct-snapshot noel-guid (audit-struct-history noel-guid txn #f)))
        (define intermediate-dave (audit-struct-snapshot dave-guid (audit-struct-history dave-guid txn #f)))
        (define intermediate-matt (audit-struct-snapshot matt-guid (audit-struct-history matt-guid txn #f)))
        
        (check-equal? (person-name intermediate-noel) "Noel 2.0")
        (check-equal? (struct-revision intermediate-noel) 1)
        (check-equal? (struct-id intermediate-noel) noel-id)
        (check-equal? (person-name intermediate-dave) "Dave")
        (check-equal? (struct-revision intermediate-dave) 0)
        (check-equal? (struct-id intermediate-dave) dave-id)
        (check-equal? intermediate-matt #f)))
    
    (test-case "audit-transaction-affected"
      (begin-with-definitions
        (clear-trail!)
        
        (match-define 
            (list noel dave)
          (call-with-transaction
           (lambda ()
             (list (save! (make-person "Noel"))
                   (save! (make-person "Dave"))))
           "0"))
        
        (define matt
          (call-with-transaction 
           (lambda ()
             (save! (copy-person dave #:name "Dave II"))
             (save! (make-person "Matt")))
           "1"))
        
        (define bree
          (call-with-transaction
           (lambda ()
             (save! (make-person "Bree")))
           "2"))
        
        (call-with-transaction
         (lambda ()
           (set-person-name! matt "Matt the Second")
           (save! matt))
         "3")
        
        (define william
          (call-with-transaction
           (lambda ()
             (save! (copy-person matt #:name "Matt the Third"))
             (save! (make-pet #f "William")))
           "4"))
        
        (define affected (audit-transaction-affected (car (find-txns))))
        
        (define-values (txn0 txn1 txn2 txn3 txn4)
          (apply values (find-txns)))
        
        (check-equal? (hash-ref affected (struct-guid noel) #f) txn0)
        (check-equal? (hash-ref affected (struct-guid dave) #f) txn0)
        (check-equal? (hash-ref affected (struct-guid matt) #f) txn1)
        (check-equal? (hash-ref affected (struct-guid bree) #f) #f)
        (check-equal? (hash-ref affected (struct-guid william) #f) txn4)))
    
    (test-case "audit-roll-back!"
      (begin-with-definitions
        (clear-trail!)
        
        (match-define 
            (list noel dave)
          (call-with-transaction
           (lambda ()
             (list (save! (make-person "Noel"))
                   (save! (make-person "Dave"))))
           "0"))
        
        (define matt
          (call-with-transaction 
           (lambda ()
             (save! (copy-person dave #:name "Dave II"))
             (save! (make-person "Matt")))
           "1"))
        
        (define bree
          (call-with-transaction 
           (lambda ()
             (save! (make-person "Bree")))
           "2"))
        
        (call-with-transaction
         (lambda ()
           (set-person-name! matt "Matt the Second")
           (save! matt))
         "3")
        
        (define william
          (call-with-transaction
           (lambda ()
             (save! (copy-person matt #:name "Matt the Third"))
             (save! (make-pet #f "William")))
           "4"))
        
        (define txn1 (cadr (find-txns)))
        
        (define affected (audit-transaction-affected txn1))
        
        (audit-roll-back! affected "Rollback 1")
        
        (define txns (find-txns))
        (define metas (find-metas))
        
        (check-equal? (length txns) 6)
        (check-equal? (map audit-metadata-message metas) 
                      (list "0" "1" "2" "3" "4" "Rollback 1"))
        (check-equal? (person-name (find-by-id person (struct-id dave))) "Dave")
        (check-equal? (person-name (find-by-id person (struct-id noel))) "Noel")
        (check-false  (find-by-id person (struct-id matt)))
        (check-equal? (find-by-id person (struct-id bree)) bree)
        (check-false  (find-by-id pet (struct-id william)))))))

; Provide statements -----------------------------

(provide make-audit-tests)
