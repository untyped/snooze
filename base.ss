#lang scheme/base

(require scheme/contract 
         scheme/pretty
         scheme/serialize
         srfi/26
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/exn)
         (planet untyped/unlib:3/pipeline)
         (planet untyped/unlib:3/time))

; Exception types ------------------------------

(define-struct (exn:snooze exn)
  ()
  #:transparent)

(define-struct (exn:fail:snooze exn:fail)
  ()
  #:transparent)

(define-struct (exn:fail:snooze:query exn:fail:snooze)
  (backtrace)
  #:transparent)

; Raised when Snooze tries to save out-of-date data to the database.
(define-struct (exn:fail:snooze:revision exn:fail:snooze)
  (struct)
  #:transparent)

; Raised when Snooze tries to roll back a non-existant transaction.
(define-struct (exn:fail:snooze:transaction exn:fail:snooze)
  ()
  #:transparent)

; Raised when Snooze tries to roll back a non-existant transaction.
(define-struct (exn:fail:snooze:connection-count exn:fail:snooze)
  ()
  #:transparent)

; Provide statements --------------------------- 

(provide (all-from-out scheme/contract
                       scheme/pretty
                       scheme/serialize
                       srfi/26
                       (planet untyped/unlib:3/debug)
                       (planet untyped/unlib:3/exn)
                       (planet untyped/unlib:3/pipeline)
                       (planet untyped/unlib:3/time))
         (all-defined-out))
