#lang scheme/base

(require scheme/class
         scheme/file
         "../snooze.ss"
         "../sqlite3/sqlite3.ss")

; DB interface (for documentation labels) ------

(define-snooze-interface 
  (make-snooze (make-database ':memory:)))

; Provide statements --------------------------- 

(provide (all-from-out scheme/base
                       scheme/class
                       "../snooze.ss"
                       "../sqlite3/sqlite3.ss")
         (snooze-interface-out))
