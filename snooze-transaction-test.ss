#lang scheme/base

(require "test-base.ss")

(require scheme/class
         srfi/26
         "snooze-api.ss"
         "core/core.ss"
         "sql/sql.ss")

; Helpers --------------------------------------

; (-> any) -> any
(define (call-with-transaction/rollback thunk)
  (letrec ([ans ans]) ; starts out undefined
    (with-handlers ([exn:fail? (lambda _ ans)])
      (call-with-transaction
       (lambda ()
         (set! ans (thunk))
         (error))))))

(define (call-with-transaction-hook new-hook thunk)
  (let ([old-hook (send (current-snooze) get-transaction-hook)])
    (dynamic-wind
     (lambda ()
       (send (current-snooze) set-transaction-hook! new-hook))
     thunk
     (lambda ()
       (send (current-snooze) set-transaction-hook! old-hook)))))

; Tests ----------------------------------------

; test-suite
(define/provide-test-suite snooze-transaction-tests
  
  ; ***** NOTE *****
  ; Each test below depends on the tests before it. Add/edit tests at your peril!
  ; ****************
  
  (test-suite "commit"
    (test-case "insert"
      (before
       (recreate-test-tables)
       (let ([p1 (make-person/defaults #:name "A")])
         (check-false (find-person #:name "A"))
         (let ([p2 (call-with-transaction
                    (lambda ()
                      (save! p1)))])
           (check snooze-guid=? p2 p1)
           (check snooze-data=? p2 p1)
           (check-not-equal? p2 p1)
           ; Check against DB:
           (let ([p3 (find-person #:name "A")])
             (check snooze-guid=? p3 p1)
             (check snooze-data=? p3 p1)
             (check-not-equal? p3 p1))))))
    (test-case "update"
      (before
       (recreate-test-tables)
       (let ([p1 (save! (make-person/defaults #:name "A"))])
         (check-equal? (find-person #:name "A") p1)
         (let ([p2 (call-with-transaction
                    (lambda ()
                      (save! (person-set p1 #:name "B"))))])
           (check snooze-guid=? p2 p1)
           (check-false (snooze-data=? p2 p1))
           ; Check against DB:
           (check-false (find-person #:name "A"))
           (let ([p3 (find-person #:name "B")])
             (check-equal? p3 p2))))))
    (test-case "delete"
      (before
       (recreate-test-tables)
       (let ([p1 (save! (make-person/defaults #:name "A"))])
         (check-equal? (find-person #:name "A") p1)
         (let ([p2 (call-with-transaction
                    (lambda ()
                      (delete! (person-set p1 #:name "B"))))])
           (check snooze-guid=? p2 p1)
           ; Check against DB:
           (check-false (find-person #:name "A") p2)
           (check-false (find-person #:name "B") p2))))))
  
  (test-suite "rollback"
    
    (test-case "insert"
      (before
       (recreate-test-tables)
       (let ([p1 (make-person/defaults #:name "A")])
         (check-false (find-person #:name "A"))
         (let ([p2 (call-with-transaction/rollback
                    (lambda ()
                      (save! p1)))])
           (check-false (snooze-struct-saved? p1))
           (check-false (snooze-struct-saved? p2))
           ; Check against DB:
           (check-false (find-person #:name "A")))
         ; Check repeat transaction:
         (check-not-exn (cut save! p1)))))
    
    (test-case "update"
      (before
       (recreate-test-tables)
       (let ([p1 (save! (make-person/defaults #:name "A"))])
         (check-equal? (find-person #:name "A") p1)
         (let ([p2 (call-with-transaction/rollback
                    (lambda ()
                      (save! (person-set p1 #:name "B"))))])
           (check snooze-guid=? p2 p1)
           (check-false (snooze-data=? p2 p1))
           ; Check against DB:
           (check-equal? (find-person #:name "A") p1)
           (check-false (find-person #:name "B")))
         ; Check repeat transaction:
         (check-not-exn (cut save! p1)))))
    
    (test-case "delete"
      (before
       (recreate-test-tables)
       (let ([p1 (save! (make-person/defaults #:name "A"))])
         (check-equal? (find-person #:name "A") p1)
         (let ([p2 (call-with-transaction/rollback
                    (lambda ()
                      (delete! (person-set p1 #:name "B"))))])
           (check snooze-guid=? p2 p1)
           ; Check against DB:
           (check-equal? (find-person #:name "A") p1)
           (check-false (find-person #:name "B")))
         ; Check repeat transaction:
         (check-not-exn (cut save! p1)))))
    
    (test-case "nested transactions"
      (before
       (recreate-test-tables)
       (let ([p1 (save! (make-person/defaults #:name "A"))]
             [p2 #f]
             [p3 #f])
         (with-handlers ([exn? void])
           (call-with-transaction
            (lambda ()
              (set! p2 (save! (person-set p1 #:name "B")))
              (call-with-transaction
               (lambda ()
                 (set! p3 (save! (person-set p2 #:name "C")))
                 (error "aborting transaction"))))))
         (check-pred snooze-struct-saved? p1)
         (check snooze-guid=? p2 p1)
         (check snooze-guid=? p3 p1)
         (check-equal? (person-name p1) "A")
         (check-equal? (person-name p2) "B")
         (check-equal? (person-name p3) "C")
         ; Check against DB:
         (check-equal? (find-person #:name "A") p1)
         (check-false (find-person #:name "B"))
         (check-false (find-person #:name "C"))
         ; Check repeat transaction:
         (check-exn exn:fail:snooze:revision? (cut save! p3))
         (check-exn exn:fail:snooze:revision? (cut save! p2))
         (check-not-exn (cut save! p1)))))
    
    (test-case "inner nested transaction"
      (before
       (recreate-test-tables)
       (let ([p1 (save! (make-person/defaults #:name "A"))]
             [p2 #f]
             [p3 #f])
         (call-with-transaction
          (lambda ()
            (set! p2 (save! (person-set p1 #:name "B")))
            (with-handlers ([exn? void])
              (call-with-transaction
               (lambda ()
                 (set! p3 (save! (person-set p2 #:name "C")))
                 (error "aborting transaction"))))))
         (check-pred snooze-struct-saved? p1)
         (check snooze-guid=? p2 p1)
         (check snooze-guid=? p3 p1)
         (check-equal? (person-name p1) "A")
         (check-equal? (person-name p2) "B")
         (check-equal? (person-name p3) "C")
         ; Check against DB:
         (if (send (send (current-snooze) get-database) supports-nested-transactions?)
             (begin (check-false (find-person #:name "A"))
                    (check-equal? (find-person #:name "B") p2)
                    (check-false (find-person #:name "C")))
             (begin (check-false (find-person #:name "A"))
                    (check-false (find-person #:name "B"))
                    (check-equal? (find-person #:name "C") p3)))
         ; Check repeat transaction:
         (if (send (send (current-snooze) get-database) supports-nested-transactions?)
             (begin (check-exn exn:fail:snooze:revision? (cut save! p1))
                    (check-exn exn:fail:snooze:revision? (cut save! p3))
                    (check-not-exn (cut save! p2)))
             (begin (check-exn exn:fail:snooze:revision? (cut save! p1))
                    (check-exn exn:fail:snooze:revision? (cut save! p2))
                    (check-not-exn (cut save! p3))))))))
    
    
    (test-case "continuation jumps"
      ; General continuation jump out:
      (define _
        (let/cc escape
          (call-with-transaction
           (lambda ()
             (check-exn exn:fail:contract:continuation?
               (lambda ()
                 (escape #f)))))))
      ; Escape continuation jump out:
      (define resume
        (check-not-exn
          (lambda ()
            (let/ec escape
              (call-with-transaction
               (lambda ()
                 (let/cc resume
                   (escape resume))))))))
      ; General continuation jump in:
      (check-exn exn:fail:contract:continuation?
        (lambda ()
          (resume #f))))
    
    (test-suite "hooks"
      
      (test-case "called"
        (before
         (recreate-test-tables)
         (let ([num-transactions 0])
           (call-with-transaction-hook
            (lambda (continue conn . args)
              (set! num-transactions (add1 num-transactions))
              (apply continue conn args))
            (lambda ()
              (delete! (save! (make-person/defaults #:name "Dave")))))
           (check-equal? num-transactions 2))))
      
      (test-case "abort before body"
        (before
         (recreate-test-tables)
         (call-with-transaction-hook
          (lambda (continue conn . args)
            (error "escaping")
            (apply continue conn args))
          (lambda ()
            (with-handlers ([exn? void])
              (save! (make-person/defaults #:name "Dave")))))
         (check-pred null? (find-people))))
      
      (test-case "abort after body"
        (before
         (recreate-test-tables)
         (call-with-transaction-hook
          (lambda (continue conn . args)
            (begin0 (apply continue conn args)
                    (error "escaping")))
          (lambda ()
            (with-handlers ([exn? void])
              (save! (make-person/defaults #:name "Dave")))))
         (check-pred null? (find-people))))))
  