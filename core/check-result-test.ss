#lang scheme/base

(require "../test-base.ss"
         (prefix-in snooze: "check-result.ss"))

; Tests ------------------------------------------

(define check-result-tests
  (test-suite "check-result.ss"
    
    (test-case "check-result-exn"
      (let ([exn (make-exn:fail "Oops!" (current-continuation-marks))])
        (check-false (snooze:check-result-exn (snooze:make-check-success "Success")) "pass")
        (check-false (snooze:check-result-exn (snooze:make-check-warning "Warning")) "warn")
        (check-false (snooze:check-result-exn (snooze:make-check-failure "Failure")) "fail")
        (check-equal? exn (snooze:check-result-exn (snooze:make-check-fatal "Exception" exn)) "exn")))))

; Provide statements -----------------------------

(provide check-result-tests)
