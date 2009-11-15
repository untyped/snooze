#lang scheme/base

(require "test-base.ss")

(require "snooze-api.ss"
         "core/core.ss"
         "sql/sql.ss")

; Helpers ----------------------------------------

(define saved   (box #f))
(define deleted (box #f))

(define (clear-boxes!)
  (set-box! saved #f)
  (set-box! deleted #f))

(define-values (old-save-hook old-delete-hook)
  (values (entity-on-save   person)
          (entity-on-delete person)))

(define (new-save-hook continue conn per)
  (if (equal? (person-name per) "Jason")
      (error "Aieeeeeee!")
      (let ([per2 (continue conn per)])
        (set-box! saved per2)
        per2)))

(define (new-delete-hook continue conn per)
  (if (equal? (person-name per) "Freddy")
      (error "Aieeeeeee!")
      (begin
        (set-box! deleted per)
        (continue conn per))))

; Tests ------------------------------------------

; test-suite
(define/provide-test-suite snooze-hook-tests
  
  #:before
  (lambda ()
    (set-entity-on-save!   person new-save-hook)
    (set-entity-on-delete! person new-delete-hook))
  
  #:after
  (lambda ()
    (set-entity-on-save!   person old-save-hook)
    (set-entity-on-delete! person old-delete-hook))
  
  (test-suite "on-save"
    
    (test-case "normal"
      (around
       (recreate-test-tables)
       (let* ([p1 (make-person "Freddy")]
              [p2 (save! p1)])
         (check snooze=? p2 p1)
         ; Check side effects:
         (check-equal? (unbox saved) p2)
         (check-false (unbox deleted))
         ; Check database:
         (check-equal? (find-people) (list p2)))
       (clear-boxes!)))
    
    (test-case "error"
      (around
       (recreate-test-tables)
       (let* ([p1 (make-person "Jason")]
              [p2 (with-handlers ([exn? (lambda _ #f)])
                    (save! p1))])
         (check-false p2)
         ; Check side effects:
         (check-false (unbox saved))
         (check-false (unbox deleted))
         ; Check database:
         (check-equal? (find-people) null))
       (clear-boxes!))))
  
  (test-suite "on-delete"
    
    (test-case "normal"
      (around
       (recreate-test-tables)
       (let* ([p1 (after (save! (make-person "Joe"))
                         (clear-boxes!))]
              [p2 (delete! p1)])
         (check snooze=? p2 p1)
         ; Check side effects:
         (check-false (unbox saved))
         (check-equal? (unbox deleted) p1)
         ; Check database:
         (check-equal? (find-people) null))
       (clear-boxes!)))
    
    (test-case "error"
      (around
       (recreate-test-tables)
       (let* ([p1 (after (save! (make-person "Freddy"))
                         (clear-boxes!))]
              [p2 (with-handlers ([exn? (lambda _ #f)])
                    (delete! p1))])
         (check-false p2)
         ; Check side effects:
         (check-false (unbox saved))
         (check-false (unbox deleted))
         ; Check database:
         (check-equal? (find-people) (list p1)))
       (clear-boxes!)))))
