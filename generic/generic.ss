#lang scheme/base

(require "../base.ss")

(require scheme/class
         "connection.ss"
         "extract.ss"
         "interface.ss"
         "snooze-reraise.ss"
         "sql-query.ss")

(define generic-database%
  (generic-extract-mixin
   (class* object% (generic-database<%>)
     
     ; Fields -------------------------------------
     
     ; (U snooze<%> #f)
     (field [snooze #f])
     
     ; Constructor --------------------------------
     
     (super-new)
     
     ; Methods ------------------------------------
     
     ; -> snooze<%>
     (define/public (get-snooze)
       snooze)
     
     ; snooze<%> -> void
     (define/public (set-snooze! the-snooze)
       (set! snooze the-snooze)))))

; Provide statements -----------------------------

(provide generic-database%
         (all-from-out "connection.ss"
                       "interface.ss"
                       "snooze-reraise.ss"
                       "sql-query.ss"))

