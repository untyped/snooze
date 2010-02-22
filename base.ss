#lang scheme/base

(require (for-syntax scheme/base)
         (planet untyped/unlib:3/require))

(define-library-aliases cce-scheme (planet cce/scheme:4:1)          #:provide)
(define-library-aliases schemeunit (planet schematics/schemeunit:3) #:provide)
(define-library-aliases sqlite     (planet jaymccarthy/sqlite:4:1)  #:provide)
(define-library-aliases unlib      (planet untyped/unlib:3:20)      #:provide)

(require scheme/class
         scheme/contract 
         scheme/match
         scheme/pretty
         scheme/serialize
         srfi/26
         (unlib-in debug exn time))

; Logging --------------------------------------

; Exception types ------------------------------

; Raised when Snooze encounters a problem that is likely caused by mistakes in user code.
(define-struct (exn:fail:snooze exn:fail) () #:transparent)

; Raised when Snooze could not parse a query.
(define-struct (exn:fail:snooze:query exn:fail:snooze) (backtrace) #:transparent)

; Raised when Snooze tries to save out-of-date data to the database.
(define-struct (exn:fail:snooze:revision exn:fail:snooze) (struct) #:transparent)

; Raised when Snooze tries to roll back a non-existant transaction.
(define-struct (exn:fail:snooze:transaction exn:fail:snooze) () #:transparent)

; Raised when a struct cannot be saved/deleted because of failed checks.
(define-struct (exn:fail:snooze:check exn:fail:snooze) (struct results) #:transparent)
(define-struct (exn:fail:snooze:check:save exn:fail:snooze:check) () #:transparent)
(define-struct (exn:fail:snooze:check:delete exn:fail:snooze:check) () #:transparent)

; Raised when the database connection limit is exceeded.
(define-struct (exn:fail:snooze:connection-count exn:fail:snooze) () #:transparent)

; Provide statements --------------------------- 

(provide (all-from-out scheme/class
                       scheme/contract
                       scheme/match
                       scheme/pretty
                       scheme/serialize
                       srfi/26)
         (unlib-out debug exn time)
         (all-defined-out))
