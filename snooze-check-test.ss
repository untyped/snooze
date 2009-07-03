#lang scheme/base

(require "test-base.ss")

(require srfi/19
         "snooze-api.ss"
         "core/core.ss"
         (prefix-in real: "core/snooze-struct.ss")
         )

; Helpers ----------------------------------------

; These bypass the normal contract checks on make-foo/defaults:

(define create-course    (entity-defaults-constructor course))
(define create-tree-node (entity-defaults-constructor tree-node))

; Tests ------------------------------------------

; test-suite
(define snooze-check-tests
  (test-suite "snooze-check-tests"
    
    #:before
    recreate-test-tables
    
    (test-case "check-snooze-struct : okay"
      (let* ([struct  (create-course)]
             [results (check-snooze-struct struct)])
        (check-equal? (length results) 0)))
    
    (test-case "check-snooze-struct : invalid null attribute"
      (let* ([struct (create-course #:code #f)]
             [results (check-snooze-struct struct)])
        (check-equal? (length results) 1)
        (check-equal? (check-result-annotation (car results) ann:struct) struct)
        (check-equal? (check-result-annotation (car results) ann:attrs)
                      (list (attr course code)))))
    
    (test-case "check-snooze-struct : symbol too long"
      (let* ([struct  (create-course #:code 'abcdefghi)]
             [results (check-snooze-struct struct)])
        (check-equal? (length results) 1)
        (check-equal? (check-result-annotation (car results) ann:attrs)
                      (list (attr course code)))))
    
    (test-case "check-snooze-struct : string too long"
      (let* ([struct  (create-course #:name (make-string 129 #\a))]
             [results (check-snooze-struct struct)])
        (check-equal? (length results) 1)
        (check-equal? (check-result-annotation (car results) ann:attrs)
                      (list (attr course name)))))
    
    (test-case "check-snooze-struct : wrong enum value"
      (let* ([struct  (create-tree-node #:color 'white)]
             [results (check-snooze-struct struct)])
        (check-equal? (length results) 1)
        (check-equal? (check-result-annotation (car results) ann:attrs)
                      (list (attr tree-node color)))))
    
    (test-case "check-snooze-struct : unserializable binary value"
      (let* ([struct  (create-course #:notes (lambda (x) (add1 x)))]
             [results (check-snooze-struct struct)])
        (check-equal? (length results) 1)
        (check-equal? (check-result-annotation (car results) ann:attrs)
                      (list (attr course notes)))))
    
    (test-case "check-snooze-struct : custom check"
      (let ([temp (entity-save-check course)])
        (set-entity-save-check! course (lambda (course) null))
        (let* ([struct  (create-course #:code #f)]
               [results (check-snooze-struct struct)])
          (check-equal? (length results) 0))
        (set-entity-save-check! course temp)))
    
    (test-case "check-old-snooze-struct : symbol too long"
      (let* ([struct  (create-course #:name (make-string 129 #\a))]
             [results (check-old-snooze-struct struct)])
        (check-pred null? results)))
    
    (test-case "save! : okay"
      (check-not-exn (cut save! (create-course))))
    
    (test-case "save! : invalid null attribute"
      (check-exn exn:fail:snooze:check?
        (cut save! (create-course #:code #f))))
    
    (test-case "save! : symbol too long"
      (check-exn exn:fail:snooze:check?
        (cut save! (create-course #:code 'abcdefghi))))
    
    (test-case "save! : symbol too long"
      (check-exn exn:fail:snooze:check?
        (cut save! (create-course #:name (make-string 129 #\a)))))
    
    (test-case "save! : custom check"
      (let ([temp (entity-save-check course)])
        (set-entity-save-check! course (lambda (course) null))
        (with-handlers ([exn? (lambda (exn)
                                (check-false (exn:fail:snooze:check? exn)))])
          (save! (create-course #:code #f))
          (check-fail "no exception raised"))
        (set-entity-save-check! course temp)))))

; Provide statements -----------------------------

(provide snooze-check-tests)
