#lang scheme/base

(require "test-base.ss")

(require srfi/19
         "snooze-api.ss"
         "era/era.ss"
         (prefix-in real: "era/snooze-struct.ss")
         )

; Tests -------------------------------------------

; test-suite
(define snooze-check-tests
  (test-suite "snooze-check-tests"
    
    #:before
    recreate-test-tables
    
    (test-case "check-struct : okay"
      (let* ([struct  (make-course/defaults)]
             [results (check-snooze-struct struct)])
        (check-equal? (length results) 0)))
    
    (test-case "check-struct : invalid null attribute"
      (let* ([struct  (make-course/defaults #:code #f)]
             [results (check-snooze-struct struct)])
        (check-equal? (length results) 1)
        (check-equal? (check-result-annotation (car results) ann:struct) struct)
        (check-equal? (check-result-annotation (car results) ann:attrs)
                      (list (attr course code)))))
    
    (test-case "check-struct : symbol too long"
      (let* ([struct  (make-course/defaults  #:code 'abcdefghi)]
             [results (check-snooze-struct struct)])
        (check-equal? (length results) 1)
        (check-equal? (check-result-annotation (car results) ann:attrs)
                      (list (attr course code)))))
    
    (test-case "check-struct : symbol too long"
      (let* ([struct  (make-course/defaults #:name (make-string 129 #\a))]
             [results (check-snooze-struct struct)])
        (check-equal? (length results) 1)
        (check-equal? (check-result-annotation (car results) ann:attrs)
                      (list (attr course name)))))
    
    (test-case "check-struct : custom check"
      (let ([temp (entity-save-check course)])
        (set-entity-save-check! course (lambda (course) null))
        (let* ([struct  (make-course/defaults #:code #f)]
               [results (check-snooze-struct struct)])
          (check-equal? (length results) 0))
        (set-entity-save-check! course temp)))
    
    (test-case "check-old-struct : symbol too long"
      (let* ([struct  (make-course/defaults #:name (make-string 129 #\a))]
             [results (check-old-snooze-struct struct)])
        (check-pred null? results)))
    
    (test-case "save! : okay"
      (check-not-exn (cut save! (make-course/defaults))))
    
    (test-case "save! : invalid null attribute"
      (check-exn exn:fail:snooze:check?
        (cut save! (make-course/defaults #:code #f))))
    
    (test-case "save! : symbol too long"
      (check-exn exn:fail:snooze:check?
        (cut save! (make-course/defaults #:code 'abcdefghi))))
    
    (test-case "save! : symbol too long"
      (check-exn exn:fail:snooze:check?
        (cut save! (make-course/defaults #:name (make-string 129 #\a)))))
    
    (test-case "save! : custom check"
      (let ([temp (entity-save-check course)])
        (set-entity-save-check! course (lambda (course) null))
        (with-handlers ([exn? (lambda (exn)
                                (check-false (exn:fail:snooze:check? exn)))])
          (save! (make-course/defaults #:code #f))
          (check-fail "no exception raised"))
        (set-entity-save-check! course temp)))))

; Provide statements -----------------------------

(provide snooze-check-tests)