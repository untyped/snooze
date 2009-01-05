(module util mzscheme
  
  (require-for-syntax (file "../base.ss"))
  
  (require (file "../base.ss"))
  
  (provide with-snooze-reraise)
  
  (define-syntax (with-snooze-reraise stx)
    (syntax-case stx ()
      [(_ (exn-pred message) expr ...)
       #'(with-handlers
             ([exn-pred (lambda (exn)
                          (reraise-exn exn exn:fail:snooze message))])
           expr ...)]))
  
  )


