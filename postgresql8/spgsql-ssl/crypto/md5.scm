;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module md5 mzscheme
  (require (lib "md5.ss"))
  
  (define (md5password/bytes user password salt)
    (let* [(s (md5 (bytes-append password user)))
           (t (md5 (bytes-append s salt)))]
      (bytes-append #"md5" t)))
  
  ;; md5password : string string bytes -> string
  ;; Compute the MD5 hash of a password in the form expected by the PostgreSQL 
  ;; backend.
  (define (md5password user password salt)
    (bytes->string/utf-8
     (md5password/bytes (string->bytes/utf-8 user)
                        (string->bytes/utf-8 password)
                        salt)))

  (provide md5password)
  )
