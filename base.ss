(module base mzscheme

  (require (planet "exn.ss" ("untyped" "unlib.plt" 2)))
  
  ; Exception types ------------------------------
  
  (define-struct (exn:snooze exn) ())
  (define-struct (exn:fail:snooze exn:fail) ())
  
  ;; exn:fail:snooze:revision : exn
  ;;
  ;; Raised when Snooze tries to save out-of-date data to the database.
  (define-struct (exn:fail:snooze:revision exn:fail:snooze) (struct))
  
  ;; exn:fail:snooze:transaction : exn
  ;;
  ;; Raised when Snooze tries to roll back a non-existant transaction.
  (define-struct (exn:fail:snooze:transaction exn:fail:snooze) ())

  ; Provide statements --------------------------- 
  
  (provide (all-from (planet "exn.ss" ("untyped" "unlib.plt" 2)))
           (all-defined))

  )