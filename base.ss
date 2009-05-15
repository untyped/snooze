#lang scheme/base

(require (planet untyped/unlib:3/require))

(define-library-aliases schemeunit (planet schematics/schemeunit:3) #:provide)
(define-library-aliases unlib      (planet untyped/unlib:3)         #:provide)

(require scheme/class
         scheme/contract 
         scheme/match
         scheme/pretty
         scheme/serialize
         srfi/26
         (unlib-in debug exn time))

; (parameter boolean)
(define in-cache-code? (make-parameter #f))

; Logging --------------------------------------

; logger
(define cache-logger
  (make-logger 'cache (current-logger)))

; string [any] [log-level-symbol] -> void
(define (log-cache message [data (void)] [level 'info])
  #;(printf "log-cache ~a: ~a~n" level message)
  (void))

; Exception types ------------------------------

; Raised when Snooze encounters a problem that is likely caused by mistakes in user code.
(define-struct (exn:fail:snooze exn:fail) () #:transparent)

; Raised when Snooze could not parse a query.
(define-struct (exn:fail:snooze:query exn:fail:snooze) (backtrace) #:transparent)

; Raised when Snooze tries to save out-of-date data to the database.
(define-struct (exn:fail:snooze:revision exn:fail:snooze) (struct) #:transparent)

; Raised when Snooze tries to roll back a non-existant transaction.
(define-struct (exn:fail:snooze:transaction exn:fail:snooze) () #:transparent)

; Raised when Snooze cannot retrieve data from the cache.
(define-struct (exn:fail:snooze:cache exn:fail:snooze) () #:transparent)

; Provide statements --------------------------- 

(provide (all-from-out scheme/class
                       scheme/contract
                       scheme/match
                       scheme/pretty
                       scheme/serialize
                       srfi/26)
         (unlib-out debug exn time)
         (all-defined-out))
