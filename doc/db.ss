(module db mzscheme

  (require (file "../snooze.ss")
           (prefix db: (file "../sqlite3/sqlite3.ss")))
  
  ; DB interface (for documentation labels) ------

  (define-snooze-interface db:db@)
  
  ; Provide statements --------------------------- 
  
  (provide (all-from (file "../snooze.ss"))
           (all-defined))
  
  )
