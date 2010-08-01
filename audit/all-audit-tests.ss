#lang scheme/base

(require "../test-base.ss")

(require srfi/19
         (planet untyped/unlib:3/time)
         "../main.ss"
         "audit.ss")

; Tests ----------------------------------------

(define-entity audit-metadata
  ([transaction audit-transaction]
   [message     string #:max-length 1024 #:default "No message"])
  #:plural audit-metadata)

; Initialised in #:before stage of tests:
(define trail #f)
  
(define find-attrs  find-audit-attributes)
(define find-txns   find-audit-transactions)
(define find-metas  find-all-audit-metadata)
(define find-deltas find-audit-deltas)
  
; -> void
(define (clear-trail!)
  (send trail clear!)
  (for-each delete! (find-metas)))
  
; entity attribute snooze-struct -> (listof any)
(define (find-history entity att struct)
  (map (cut audit-delta-value <> (attribute-type att))
       (select-all #:what  audit-delta
                   #:from  (inner audit-attribute audit-delta (= audit-attribute.id audit-delta.attribute-id))
                   #:where (and (= audit-attribute.table ,(entity-table-name entity))
                                (= audit-attribute.name  ,(attribute-column-name att))
                                (= audit-delta.struct-id ,(snooze-struct-id struct)))
                   #:order ((asc audit-delta.guid)))))
  
; [snooze] -> test-suite
(define/provide-test-suite all-audit-tests
  
    #:before
    (lambda ()
      (drop-all-tables)
      (create-table course)
      (create-table person)
      (create-table pet)
      (set! trail (new audit-trail%
                       [entities        (list course person pet)]
                       [metadata-entity audit-metadata])))
    
    #:after
    drop-all-tables
    
    ; Initialising -------------------------------
    
    (test-case "init-audit-trail!"
      (check-true (table-exists? audit-attribute))
      (check-true (table-exists? audit-transaction))
      (check-true (table-exists? audit-delta))
      (check-true (procedure? (send (current-snooze) get-transaction-hook)))
      (check-true (procedure? (entity-on-delete person)))
      (check-true (procedure? (entity-on-delete pet))))
    
    ; Auditing changes ---------------------------
    
    #;(test-case "audit-attributes generated correctly"
        (fail "Not implemented."))
    
    (test-case "audit basic insert, update and delete"
      (clear-trail!)
      
      (check-pred null? (find-txns))
      (check-pred null? (find-deltas))
      
      (let ([person1 (save! (make-person "Dave"))])
        
        (check-equal? (length (find-txns)) 1)
        (check-equal? (length (find-deltas)) 1)
        
        (let ([person2 (save! (person-set person1 #:name "Noel"))])
          
          (check-equal? (length (find-txns)) 2)
          (check-equal? (length (find-deltas)) 2)
          
          (delete! person2)
          
          (check-equal? (length (find-txns)) 3)
          (check-equal? (length (find-deltas)) 3))))
    
    (test-case "audit sequence"
      (clear-trail!)
      
      (let-values ([(person1 person2)
                    (apply values (with-transaction #:metadata (list "0")
                                    (list (save! (make-person "Dave"))
                                          (save! (make-person "Noel")))))]
                   [(deltas) (find-deltas)])
        
        (check-equal? (audit-delta-struct-id (car deltas))  (snooze-struct-id person1))
        (check-equal? (audit-delta-struct-id (cadr deltas)) (snooze-struct-id person2))))
    
    (test-case "audit the different attribute types"
      (let* ([time    (string->time-tai "2001-01-01 01:01:01")]
             [course1 (save! (make-course 'COURSE "Course" 123 1.23 #t time))])
        
        (clear-trail!)
        
        (save! (course-set course1
                           #:code   'ESRUOC
                           #:name   "esruoC"
                           #:value  321
                           #:rating 3.21
                           #:active #f
                           #:start  (current-time time-tai)))
        
        (check-equal? (find-history course (attr course guid)     course1) (list))
        (check-equal? (find-history course (attr course revision) course1) (list))
        (check-equal? (find-history course (attr course code)     course1) (list 'COURSE))
        (check-equal? (find-history course (attr course name)     course1) (list "Course"))
        (check-equal? (find-history course (attr course value)    course1) (list 123))
        (check-equal? (find-history course (attr course rating)   course1) (list 1.23))
        (check-equal? (find-history course (attr course start)    course1) (list time))
        
        (check-true (andmap (lambda (delta)
                              (eq? (audit-delta-struct-id delta) (snooze-struct-id course1)))
                            (find-deltas)))))
    
    (test-case "audit insert/update sequence correctly summarised"
      (clear-trail!)
      
      (let ([person1 (with-transaction #:metadata (list "insert/update sequence")
                       (let ([ans (save! (make-person "Dave"))])
                         (save! (person-set ans #:name "Noel"))))]
            [txns    (find-txns)]
            [metas   (find-metas)]
            [deltas  (find-deltas)])
        
        (check-equal? (length txns) 1)
        (check-equal? (audit-metadata-message (car metas)) "insert/update sequence")
        
        (check-equal? (length deltas) 2)
        (check-equal? (car deltas)
                      (audit-delta-set (make-insert-delta (car txns) (snooze-struct-guid person1))
                                       #:id       (snooze-struct-id (car deltas))
                                       #:revision 0))
        (check-equal? (cadr deltas)
                      (audit-delta-set (make-update-delta (car txns) (snooze-struct-guid person1) 0 (attr person name) "Dave")
                                       #:id       (snooze-struct-id (cadr deltas))
                                       #:revision 0))))
    
    (test-case "insert/delete sequence correctly summarised"
      (clear-trail!)
      
      (let ([person1 (with-transaction #:metadata (list "insert/delete sequence")
                       (let ([ans (save! (make-person "Dave"))])
                         (delete! (person-set ans))
                         ans))]
            [txns    (find-txns)]
            [metas   (find-metas)]
            [deltas  (find-deltas)])
        
        (check-equal? (length txns) 1)
        (check-equal? (audit-metadata-message (car metas)) "insert/delete sequence")
        
        (check-equal? (length deltas) 2)
        (check-equal? (car deltas)
                      (audit-delta-set (make-insert-delta (car txns) (snooze-struct-guid person1))
                                       #:id       (snooze-struct-id (car deltas))
                                       #:revision 0))
        (check-equal? (cadr deltas)
                      (audit-delta-set (make-delete-delta (car txns) (snooze-struct-guid person1) 0 (attr person name) "Dave")
                                       #:id       (snooze-struct-id (cadr deltas))
                                       #:revision 0))))
    
    (test-case "update/update sequence correctly summarised"
      (clear-trail!)
      
      (let ([person1 (let ([ans (save! (make-person "Dave"))])
                       (clear-trail!)
                       (with-transaction #:metadata (list "update/update sequence")
                         (save! (person-set (save! (person-set ans #:name "Noel")) #:name "Matt")))
                         ans)]
            [txns    (find-txns)]
            [metas   (find-metas)]
            [deltas  (find-deltas)])
        
        (check-equal? (length txns) 1)
        (check-equal? (audit-metadata-message (car metas)) "update/update sequence")
        (check-equal? (length deltas) 2)
        (check-equal? (car deltas)
                      (audit-delta-set (make-update-delta (car txns) (snooze-struct-guid person1) 0 (attr person name) "Dave")
                                       #:id       (snooze-struct-id (car deltas))
                                       #:revision 0))
        (check-equal? (cadr deltas)
                      (audit-delta-set (make-update-delta (car txns) (snooze-struct-guid person1) 1 (attr person name) "Noel")
                                       #:id       (snooze-struct-id (cadr deltas))
                                       #:revision 0))))
    
    (test-case "audit update/delete sequence correctly summarised"
      (clear-trail!)
      
      (let ([person1 (let ([ans (save! (make-person "Dave"))])
                       (clear-trail!)
                       (with-transaction #:metadata (list "update/delete sequence")
                         (delete! (save! (person-set ans #:name "Noel"))))
                       ans)]
            [txns    (find-txns)]
            [metas   (find-metas)]
            [deltas  (find-deltas)])
        
        (check-equal? (length txns) 1)
        (check-equal? (audit-metadata-message (car metas)) "update/delete sequence")
        (check-equal? (length deltas) 2)
        (check-equal? (car deltas)
                      (audit-delta-set (make-update-delta (car txns) (snooze-struct-guid person1) 0 (attr person name) "Dave")
                                       #:id       (snooze-struct-id (car deltas))
                                       #:revision 0))
        (check-equal? (cadr deltas)
                      (audit-delta-set (make-delete-delta (car txns) (snooze-struct-guid person1) 1 (attr person name) "Noel")
                                       #:id       (snooze-struct-id (cadr deltas))
                                       #:revision 0))))
    
    (test-case "audit trail not written when transaction aborted"
      (clear-trail!)
      
      (let/ec escape
        (with-transaction #:metadata (list "aborted with escape continuation")
          (save! (make-person "Dave"))
          (escape #f)))
      
      (let ([txns    (find-txns)]
            [metas   (find-metas)]
            [deltas  (find-deltas)])
        
        (check-equal? (length txns) 0)
        (check-equal? (length deltas) 0)))
    
    (test-case "nested transaction audited at outermost transaction"
      (clear-trail!)
      
      (let/ec escape
        (with-transaction #:metadata (list "outer")
          (save! (make-person "Dave"))
          (with-transaction #:metadata (list "first inner")
            (save! (make-person "Noel")))
          (with-transaction #:metadata (list "second inner")
            (save! (make-person "Matt")))))
      
      (let ([txns    (find-txns)]
            [metas   (find-metas)]
            [deltas  (find-deltas)])
        
        (check-equal? (length txns) 1)
        (check-equal? (length deltas) 3)
        
        (check-true (andmap (lambda (delta)
                              (equal? (audit-delta-transaction delta) (car txns)))
                            deltas))))
    
    (test-case "inner transaction aborted"
      (let ([dave (save! (make-person "Dave"))]
            [noel (save! (make-person "Noel"))])
        
        (clear-trail!)
        
        (with-transaction #:metadata (list "Outer")
          (with-transaction #:metadata (list "Inner 2")
            (save! (person-set dave #:name "Dave 2")))
          (let/ec escape
            (with-transaction #:metadata (list "Inner 1")
              (save! (person-set dave #:name "Dave 3"))
              (save! (person-set noel #:name "Noel 2"))
              (escape #f))))
        
        (let ([txns    (find-txns)]
              [metas   (find-metas)]
              [deltas  (find-deltas)])
          
          (check-equal? (length txns) 1) ; Outer transaction only
          (check-equal? (length deltas) 1)
          
          (check-true (andmap (lambda (delta)
                                (= (audit-delta-struct-id delta) (snooze-struct-id dave)))
                              deltas))
                    
          (check-equal? (person-name dave) "Dave 2")  ; Make sure structs are rolled back correctly
          (check-equal? (person-name noel) "Noel")))) ; Make sure structs are rolled back correctly
    
    ; Querying and rollback ----------------------
    
    #;(test-case "audit-transaction-deltas"
        (before (clear-trail!)
                (begin-with-definitions
                  (define person1 (save! (make-person "Dave")))
                  
                  (define txns (find-txns))
                  (define metas (find-metas))
                  (define person1-txn (car txns))
                  
                  (define deltas (audit-transaction-deltas person1-txn))
                  
                  (check-equal? deltas (find-deltas)))))
    
    #;(test-case "id->attribute"
        (before (clear-trail!)
                (begin-with-definitions
                  (define person1 (save! (make-person "Dave")))
                  (save! (person-set person1 #:name "Noel"))
                  
                  ; audit-attribute
                  (define audit-attr (car (find-attrs)))
                  
                  ; attribute
                  (define att (id->attribute (snooze-struct-id audit-attr)))
                  
                  (check-eq? att (attr person name)))))
    
    #;(test-case "audit-deltas->guids"
        (begin-with-definitions
          (define noel (save! (make-person "Noel")))
          (define william (save! (make-pet (snooze-struct-id noel) "William")))
          (define henry (save! (make-pet (snooze-struct-id noel) "Henry")))
          
          (clear-trail!)
          
          (with-transaction #:metadata (list "Swapping cat names")
            (set-pet-name! william "Henry")
            (set-pet-name! henry "William")
            (save! william)
            (save! henry))
          
          ; audit-transaction
          (define txn (car (find-txns)))
          
          ; (listof audit-delta)
          (define deltas (audit-transaction-deltas txn))
          
          ; (listof guid)
          (define guids (audit-deltas->guids deltas))
          
          (check-not-false (member (snooze-struct-guid william) guids))
          (check-not-false (member (snooze-struct-guid henry) guids))
          (check-false (member (snooze-struct-guid noel) guids))))
    
    #;(test-case "audit-struct-history"
        (begin-with-definitions
          (clear-trail!)
          
          (define noel (save! (make-person "Noel")))
          (set-person-name! noel "Dave")
          (save! noel)
          (set-person-name! noel "Matt")
          (save! noel)
          (save! (make-person "Bree"))
          (save! (make-pet (snooze-struct-id noel) "William"))
          (save! (make-pet (snooze-struct-id noel) "Henry"))
          
          ; audit-transaction ; the INSERT transaction
          (define txn (car (find-txns)))
          
          ; (listof audit-delta)
          (define history (audit-struct-history (snooze-struct-guid noel) txn))
          
          ; (listof audit-delta)
          (define insert-history 
            (filter (lambda (delta) (equal? (audit-delta-type delta) 'I))
                    history))
          
          ; (listof audit-delta)
          (define id-history 
            (filter (lambda (delta) (equal? (audit-delta-attribute delta) (attr person guid)))
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
    
    #;(test-case "revert-delta!: single update"
        (begin-with-definitions
          (define noel (save! (make-person "Noel")))
          
          (define noel-id (snooze-struct-id noel))
          
          (clear-trail!)
          
          (set-person-name! noel "Dave")
          (save! noel)
          
          ; audit-transaction ; the UPDATE transaction
          (define txn (car (find-txns)))
          
          ; (listof audit-delta)
          (define history (audit-struct-history (snooze-struct-guid noel) txn))
          
          (foldl (cut revert-delta! (snooze-struct-guid noel) <> <>)
                 noel
                 history)
          
          (check-equal? (person-name noel) "Noel")
          (check-equal? (snooze-struct-revision noel) 1)
          (check-equal? (snooze-struct-id noel) noel-id)))
    
    #;(test-case "revert-delta!: insert, update and delete"
        (begin-with-definitions
          (define noel (save! (make-person "Noel")))
          (define dave (save! (make-person "Dave")))
          
          (define noel-id (snooze-struct-id noel))
          (define dave-id (snooze-struct-id dave))
          
          (define noel-guid (snooze-struct-guid noel))
          (define dave-guid (snooze-struct-guid dave))
          
          (clear-trail!)
          
          (set-person-name! noel "Noel 2.0")
          (save! noel)
          (delete! dave)
          (define matt (save! (make-person "Matt"))) 
          (define matt-guid (snooze-struct-guid matt))
          
          ; audit-transaction ; the UPDATE transaction
          (define txn (car (find-txns)))
          
          (define original-noel (foldl (cut revert-delta! noel-guid <> <>) noel (audit-struct-history noel-guid txn)))
          (define original-dave (foldl (cut revert-delta! dave-guid <> <>) dave (audit-struct-history dave-guid txn)))
          (define original-matt (foldl (cut revert-delta! matt-guid <> <>) matt (audit-struct-history matt-guid txn)))
          
          (check-equal? (person-name original-noel) "Noel")
          (check-equal? (snooze-struct-revision original-noel) 1)
          (check-equal? (snooze-struct-id original-noel) noel-id)
          (check-equal? (person-name original-dave) "Dave")
          (check-equal? (snooze-struct-revision original-dave) 0)
          (check-equal? (snooze-struct-id original-dave) dave-id)
          (check-equal? original-matt #f)))
    
    #;(test-case "audit-snapshot"
        (begin-with-definitions
          (define noel (save! (make-person "Noel")))
          (define dave (save! (make-person "Dave")))
          
          (define noel-id (snooze-struct-id noel))
          (define dave-id (snooze-struct-id dave))
          
          (define noel-guid (snooze-struct-guid noel))
          (define dave-guid (snooze-struct-guid dave))
          
          (clear-trail!)
          
          (set-person-name! noel "Noel 2.0")
          (save! noel)
          (delete! dave)
          (define matt (save! (make-person "Matt"))) 
          (define matt-guid (snooze-struct-guid matt))
          
          ; audit-transaction ; the UPDATE transaction
          (define txn (car (find-txns)))
          
          (define original-noel (audit-struct-snapshot noel-guid (audit-struct-history noel-guid txn)))
          (define original-dave (audit-struct-snapshot dave-guid (audit-struct-history dave-guid txn)))
          (define original-matt (audit-struct-snapshot matt-guid (audit-struct-history matt-guid txn)))
          
          (check-equal? (person-name original-noel) "Noel")
          (check-equal? (snooze-struct-revision original-noel) 1)
          (check-equal? (snooze-struct-id original-noel) noel-id)
          (check-equal? (person-name original-dave) "Dave")
          (check-equal? (snooze-struct-revision original-dave) 0)
          (check-equal? (snooze-struct-id original-dave) dave-id)
          (check-equal? original-matt #f)
          
          (define intermediate-noel (audit-struct-snapshot noel-guid (audit-struct-history noel-guid txn #f)))
          (define intermediate-dave (audit-struct-snapshot dave-guid (audit-struct-history dave-guid txn #f)))
          (define intermediate-matt (audit-struct-snapshot matt-guid (audit-struct-history matt-guid txn #f)))
          
          (check-equal? (person-name intermediate-noel) "Noel 2.0")
          (check-equal? (snooze-struct-revision intermediate-noel) 1)
          (check-equal? (snooze-struct-id intermediate-noel) noel-id)
          (check-equal? (person-name intermediate-dave) "Dave")
          (check-equal? (snooze-struct-revision intermediate-dave) 0)
          (check-equal? (snooze-struct-id intermediate-dave) dave-id)
          (check-equal? intermediate-matt #f)))
    
    #;(test-case "audit-transaction-affected"
        (begin-with-definitions
          (clear-trail!)
          
          (match-define 
           (list noel dave)
           (with-transaction #:metadata (list "0")
             (list (save! (make-person "Noel"))
                   (save! (make-person "Dave")))))
          
          (define matt
            (with-transaction #:metadata (list "1")
              (save! (person-set dave #:name "Dave II"))
              (save! (make-person "Matt"))))
          
          (define bree
            (with-transaction #:metadata (list "2")
              (save! (make-person "Bree"))))
          
          (with-transaction #:metadata (list "3")
            (set-person-name! matt "Matt the Second")
             (save! matt))
          
          (define william
            (with-transaction #:metadata (list "4")
              (save! (person-set matt #:name "Matt the Third"))
              (save! (make-pet #f "William"))))
          
          (define affected (audit-transaction-affected (car (find-txns))))
          
          (define-values (txn0 txn1 txn2 txn3 txn4)
            (apply values (find-txns)))
          
          (check-equal? (hash-ref affected (snooze-struct-guid noel) #f) txn0)
          (check-equal? (hash-ref affected (snooze-struct-guid dave) #f) txn0)
          (check-equal? (hash-ref affected (snooze-struct-guid matt) #f) txn1)
          (check-equal? (hash-ref affected (snooze-struct-guid bree) #f) #f)
          (check-equal? (hash-ref affected (snooze-struct-guid william) #f) txn4)))
    
    #;(test-case "audit-roll-back!"
        (begin-with-definitions
          (clear-trail!)
          
          (match-define 
           (list noel dave)
           (with-transaction #:metadata (list "0")
             (list (save! (make-person "Noel"))
                   (save! (make-person "Dave")))))
          
          (define matt
            (with-transaction #:metadata (list "1")
              (save! (person-set dave #:name "Dave II"))
              (save! (make-person "Matt"))))
          
          (define bree
            (with-transaction #:metadata (list "2")
              (save! (make-person "Bree"))))
          
          (with-transaction #:metadata (list "3")
            (set-person-name! matt "Matt the Second")
            (save! matt))
          
          (define william
            (with-transaction #:metadata (list "4")
              (save! (person-set matt #:name "Matt the Third"))
              (save! (make-pet #f "William"))))
          
          (define txn1 (cadr (find-txns)))
          
          (define affected (audit-transaction-affected txn1))
          
          (audit-roll-back! affected "Rollback 1")
          
          (define txns (find-txns))
          (define metas (find-metas))
          
          (check-equal? (length txns) 6)
          (check-equal? (map audit-metadata-message metas) 
                        (list "0" "1" "2" "3" "4" "Rollback 1"))
          (check-equal? (person-name (find-by-id person (snooze-struct-id dave))) "Dave")
          (check-equal? (person-name (find-by-id person (snooze-struct-id noel))) "Noel")
          (check-false  (find-by-id person (snooze-struct-id matt)))
          (check-equal? (find-by-id person (snooze-struct-id bree)) bree)
          (check-false  (find-by-id pet (snooze-struct-id william))))))
