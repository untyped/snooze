#lang scheme/base

(require "../test-base.ss"
         "check-combinator.ss"
         "result.ss"
         "result-combinator.ss")

; Tests ------------------------------------------

(define result-combinator-tests
  (test-suite "result-combinator.ss"
    
    (test-case "check-problems? works as expected"
      (check-true  (check-problems? (check-all (check-pass) (check-warn "Dang"))))
      (check-false (check-problems? (check-all (check-pass) (check-pass)))))
    
    (test-case "check-errors? works as expected"
      (check-true  (check-fatals? (check-with-handlers (cut raise-exn exn:fail "Dang"))))
      (check-true  (check-errors? (check-all (check-pass) (check-fail "Dang"))))
      (check-false (check-errors? (check-all (check-pass) (check-warn "Dang"))))
      (check-false (check-errors? (check-all (check-pass) (check-pass)))))
    
    (test-case "check-fatals? works as expected"
      (check-true  (check-fatals? (check-with-handlers (cut raise-exn exn:fail "Dang"))))
      (check-false (check-fatals? (check-all (check-pass) (check-fail "Dang"))))
      (check-false (check-fatals? (check-all (check-pass) (check-warn "Dang"))))
      (check-false (check-fatals? (check-all (check-pass) (check-pass)))))))

; Provide statements -----------------------------

(provide result-combinator-tests)
