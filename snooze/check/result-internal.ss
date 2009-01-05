#lang scheme/base

(require scheme/contract
         scheme/serialize)

; Result structures ----------------------------

; (struct string (hasheqof symbol any))
(define-serializable-struct check-result (message annotations) #:transparent)
(define-serializable-struct (check-success check-result) () #:transparent)

; problems
(define-serializable-struct (check-problem check-result) () #:transparent)

; non-fatal problems
(define-serializable-struct (check-warning check-problem) () #:transparent)

; fatal problems
(define-serializable-struct (check-error   check-problem) () #:transparent)
(define-serializable-struct (check-failure check-error) () #:transparent)

; (struct string (hasheqof symbol any) exn)
(define-serializable-struct (check-fatal check-error) (exn) #:transparent)

; Provide statements ---------------------------

; contract
(define annotations/c
  (and/c hash? hash-eq?))

(provide annotations/c)

(provide/contract
 [struct check-result                  ([message string?] [annotations annotations/c])]
 [struct (check-success check-result)  ([message string?] [annotations annotations/c])]
 [struct (check-problem check-result)  ([message string?] [annotations annotations/c])]
 [struct (check-warning check-problem) ([message string?] [annotations annotations/c])]
 [struct (check-error   check-problem) ([message string?] [annotations annotations/c])]
 [struct (check-failure check-error)   ([message string?] [annotations annotations/c])]
 [struct (check-fatal   check-error)   ([message string?] [annotations annotations/c] [exn exn?])])
