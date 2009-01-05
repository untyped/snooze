#lang scheme/base

(require "../test-base.ss"
         "annotation.ss"
         "check-combinator.ss"
         "check-combinator-syntax.ss"
         "result.ss")

; Helpers ----------------------------------------

; annotation
(define-annotation ann:num 
  (lambda (result) 0)
  (lambda (result old new) (+ old new)))

; annotation
(define-annotation ann:str
  (lambda (result) "")
  (lambda (result old new) (string-append old new)))

; Tests ------------------------------------------

(define check-combinator-tests
  (test-suite "check-combinator.ss"
    
    (test-case "pass, warn and fail"
      (check-equal? (check-pass)           (list (make-check-success "Okay")) "pass")
      (check-equal? (check-pass "Success") (list (make-check-success "Success")) "pass with argument")
      (check-equal? (check-warn "Warning") (list (make-check-warning "Warning")) "warn")
      (check-equal? (check-fail "Failure") (list (make-check-failure "Failure")) "fail"))
    
    (test-equal? "check-all"
      (check-all (check-pass "Success")
                 (check-warn  "Warning")
                 (check-fail  "Failure"))
      (list (make-check-success "Success")
            (make-check-warning "Warning")
            (make-check-failure "Failure")))
    
    (test-case "check-successes, check-problems, check-warnings, check-errors, check-failures, check-fatals"
      (let* ([exn (make-exn "Dang" (current-continuation-marks))]
             [all (check-all (check-pass)
                             (check-warn  "w1")
                             (check-fail "f1")
                             (list (make-check-fatal "e1" exn))
                             (check-pass)
                             (check-warn  "w2")
                             (check-fail "f2")
                             (list (make-check-fatal "e2" exn)))])
        (check-equal? (check-successes all)
                      (check-all (check-pass)
                                 (check-pass)))
        (check-equal? (check-problems all)
                      (check-all (check-warn  "w1")
                                 (check-fail "f1")
                                 (list (make-check-fatal "e1" exn))
                                 (check-warn  "w2")
                                 (check-fail "f2")
                                 (list (make-check-fatal "e2" exn))))
        (check-equal? (check-warnings all)
                      (check-all (check-warn  "w1")
                                 (check-warn  "w2")))
        (check-equal? (check-errors all)
                      (check-all (check-fail "f1")
                                 (list (make-check-fatal "e1" exn))
                                 (check-fail "f2")
                                 (list (make-check-fatal "e2" exn))))
        (check-equal? (check-failures all)
                      (check-all (check-fail "f1")
                                 (check-fail "f2")))
        (check-equal? (check-fatals all)
                      (check-all (list (make-check-fatal "e1" exn))
                                 (list (make-check-fatal "e2" exn))))))
    
    (test-case "check-warnings+failures+fatals"
      (let*-values ([(exn) (make-exn "Dang" (current-continuation-marks))]
                    [(warnings failures fatals)
                     (check-warnings+failures+fatals 
                      (check-all (check-pass)
                                 (check-warn  "w1")
                                 (check-fail "f1")
                                 (list (make-check-fatal "e1" exn))
                                 (check-pass)
                                 (check-warn  "w2")
                                 (check-fail "f2")
                                 (list (make-check-fatal "e2" exn))))])
        (check-equal? warnings 
                      (check-all (check-warn  "w1")
                                 (check-warn  "w2"))
                      "check 1")
        (check-equal? failures
                      (check-all (check-fail "f1")
                                 (check-fail "f2"))
                      "check 2")
        (check-equal? fatals
                      (check-all (list (make-check-fatal "e1" exn))
                                 (list (make-check-fatal "e2" exn)))
                      "check 3")))
    
    (test-case "check-with-handlers"
      (let ([exn (make-exn:fail "Oops!" (current-continuation-marks))])
        (check-equal? (check-with-handlers 
                       (lambda ()
                         (check-all (check-pass "Success")
                                    (check-warn  "Warning")
                                    (check-fail "Failure"))))
                      (list (make-check-success "Success")
                            (make-check-warning "Warning")
                            (make-check-failure "Failure"))
                      "no exn")
        (check-equal? (check-with-handlers
                       (lambda ()
                         (raise exn)))
                      (list (make-check-fatal "Exception raised" exn))
                      "exn")))
    
    (test-case "check-with-annotations"
      (let* ([results1 (check-all 
                        (check-pass "Success")
                        (check-warn  "Warning"))]
             [results2 (check-with-annotations 
                        (list (cons ann:num 123))
                        (lambda () results1))]
             [results3 (check-with-annotations
                        (list (cons ann:num 133) (cons ann:str "str"))
                        (lambda () results2))])
        (check-equal? results1
                      (list (make-check-success "Success")
                            (make-check-warning "Warning"))
                      "results1")
        (check-equal? results2
                      (list (make-check-success "Success" #:annotations (make-immutable-hasheq (list (cons ann:num 123))))
                            (make-check-warning "Warning" #:annotations (make-immutable-hasheq (list (cons ann:num 123)))))
                      "results2")
        (check-equal? results3
                      (list (make-check-success "Success" #:annotations (make-immutable-hasheq (list (cons ann:num 256) (cons ann:str "str"))))
                            (make-check-warning "Warning" #:annotations (make-immutable-hasheq (list (cons ann:num 256) (cons ann:str "str")))))
                      "results3")))
    
    (test-case "check/annotations"
      (let* ([results1 (check-all 
                        (check-pass "Success")
                        (check-warn  "Warning"))]
             [results2 (check/annotate ([ann:num 123]) results1)]
             [results3 (check/annotate ([ann:num 133] [ann:str "str"]) results2)])
        (check-equal? results1
                      (list (make-check-success "Success")
                            (make-check-warning "Warning"))
                      "results1")
        (check-equal? results2
                      (list (make-check-success 
                             "Success" #:annotations (make-immutable-hasheq (list (cons ann:num 123))))
                            (make-check-warning 
                             "Warning" #:annotations (make-immutable-hasheq (list (cons ann:num 123)))))
                      "results2")
        (check-equal? results3
                      (list (make-check-success 
                             "Success" #:annotations (make-immutable-hasheq (list (cons ann:num 256) (cons ann:str "str"))))
                            (make-check-warning
                             "Warning" #:annotations (make-immutable-hasheq (list (cons ann:num 256) (cons ann:str "str")))))
                      "results3")))
    
    (test-case "check-until-problems"
      (let ([stage-reached #f])
        (check-until-problems (lambda () (set! stage-reached 0) (check-pass))
                              (lambda () (set! stage-reached 1) (check-pass))
                              (lambda () (set! stage-reached 2) (check-pass)))
        (check-equal? stage-reached 2 "check 1"))
      (let ([stage-reached #f])
        (check-until-problems (lambda () (set! stage-reached 0) (check-pass))
                              (lambda () (set! stage-reached 1) (check-warn  "Dang"))
                              (lambda () (set! stage-reached 2) (check-pass)))
        (check-equal? stage-reached 1 "check 2")))))

; Provide statements -----------------------------

(provide check-combinator-tests)
