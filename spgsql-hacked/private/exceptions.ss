;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module exceptions mzscheme
  (require "p3-msg.ss")
  (provide (struct exn:spgsql:backend ())
           raise-backend-error)
  
  ;; exn:spgsql:backend
  ;; Represents an ErrorResponse sent by the backend.
  (define-struct (exn:spgsql:backend exn:fail:user) (properties))
  
  ;; raise-backend-error : symbol ErrorResponse -> raises exn
  (define (raise-backend-error function r)
    (define code (cdr (assq 'code (ErrorResponse-properties r))))
    (define message (cdr (assq 'message (ErrorResponse-properties r))))
    (raise 
     (make-exn:spgsql:backend
      (string-append (if function
                         (string-append (symbol->string function) ": ")
                         "")
                     message
                     " (SQL code " code ")")
      (current-continuation-marks)
      (ErrorResponse-properties r))))
  )
