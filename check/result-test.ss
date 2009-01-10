#lang scheme/base

(require "../test-base.ss"
         (prefix-in c: "result.ss"))

; Tests ------------------------------------------

(define result-tests
  (test-suite "result.ss"
    
    (test-case "check-result-exn"
      (let ([exn (make-exn:fail "Oops!" (current-continuation-marks))])
        (check-false (c:check-result-exn (c:make-check-success "Success")) "pass")
        (check-false (c:check-result-exn (c:make-check-warning "Warning")) "warn")
        (check-false (c:check-result-exn (c:make-check-failure "Failure")) "fail")
        (check-equal? exn (c:check-result-exn (c:make-check-fatal "Exception" exn)) "exn")))))

; Provide statements -----------------------------

(provide result-tests)
