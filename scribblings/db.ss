#lang scheme/base

(require scheme/class
         scheme/file
         (planet untyped/snooze:2/snooze)
         (planet untyped/snooze:2/sqlite3/sqlite3))

; DB interface (for documentation labels) ------

(define-snooze-interface 
  (make-snooze (make-database (make-temporary-file "temp~a.sqlite"))))

; Provide statements --------------------------- 

(provide (all-from-out scheme/base
                       scheme/class
                       (planet untyped/snooze:2/snooze)
                       (planet untyped/snooze:2/sqlite3/sqlite3))
         (snooze-interface-out))
