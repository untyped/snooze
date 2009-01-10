#lang scheme/base

(require "../test-base.ss"
         "check-combinator.ss"
         "result.ss"
         "result-combinator.ss")

; Tests ------------------------------------------

(define result-combinator-tests
  (test-suite "result-combinator.ss"
    
    (test-case "check-successes?"
      (check-true  (check-successes? (check-pass) (check-warn "Dang")))
      (check-false (check-successes? (check-warn "Dang") (check-warn "Dang"))))
    
    (test-case "check-problems?"
      (check-true  (check-problems? (check-pass) (check-warn "Dang")))
      (check-true  (check-problems? (check-pass) (check-fail "Dang")))
      (check-false (check-problems? (check-pass) (check-pass))))
    
    (test-case "check-warnings?"
      (check-true  (check-warnings? (check-pass) (check-warn "Dang")))
      (check-false (check-warnings? (check-pass) (check-fail "Dang")))
      (check-false (check-warnings? (check-pass) (check-pass))))
    
    (test-case "check-errors?"
      (check-true  (check-errors? (check-with-handlers (cut raise-exn exn:fail "Dang"))))
      (check-true  (check-errors? (check-pass) (check-fail "Dang")))
      (check-false (check-errors? (check-pass) (check-warn "Dang")))
      (check-false (check-errors? (check-pass) (check-pass))))
    
    (test-case "check-failures?"
      (check-false (check-failures? (check-with-handlers (cut raise-exn exn:fail "Dang"))))
      (check-true  (check-failures? (check-pass) (check-fail "Dang")))
      (check-false (check-failures? (check-pass) (check-warn "Dang")))
      (check-false (check-failures? (check-pass) (check-pass))))
    
    (test-case "check-fatals?"
      (check-true  (check-fatals? (check-with-handlers (cut raise-exn exn:fail "Dang"))))
      (check-false (check-fatals? (check-pass) (check-fail "Dang")))
      (check-false (check-fatals? (check-pass) (check-warn "Dang")))
      (check-false (check-fatals? (check-pass) (check-pass))))))

; Provide statements -----------------------------

(provide result-combinator-tests)
