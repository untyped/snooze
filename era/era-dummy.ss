#lang scheme/base
  
(require "../base.ss")

; Procedures -----------------------------------

; symbol -> procedure
(define (make-dummy-constructor struct-name)
  (lambda args
    (error (format "The ~a structure type wasn't correctly bootstrapped." struct-name))))

; symbol -> procedure
(define (make-dummy-predicate struct-name)
  (lambda (value)
    (error (format "The ~a structure type wasn't correctly bootstrapped." struct-name))))

; symbol -> procedure
(define (make-dummy-accessor struct-name)
  (lambda (struct index)
    (error (format "The ~a structure type wasn't correctly bootstrapped." struct-name))))

; symbol -> procedure
(define (make-dummy-mutator struct-name)
  (lambda (struct index value)
    (error (format "The ~a structure type wasn't correctly bootstrapped." struct-name))))

; Provide statements ---------------------------

(provide/contract
 [make-dummy-constructor (-> symbol? procedure?)]
 [make-dummy-predicate   (-> symbol? procedure?)]
 [make-dummy-accessor    (-> symbol? procedure?)]
 [make-dummy-mutator     (-> symbol? procedure?)])
