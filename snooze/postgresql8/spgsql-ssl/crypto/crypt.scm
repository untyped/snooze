;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module crypt mzscheme
  (require (lib "foreign.ss"))
  (unsafe!)
  (provide crypt)

  (define libcrypt 
    (with-handlers ([void (lambda (e) #f)])
      (ffi-lib "libcrypt")))

  (define primitive-crypt
    (and libcrypt
         (get-ffi-obj "crypt" libcrypt (_fun _bytes _bytes -> _bytes)
                      (lambda () #f))))
  
  (define crypt
    (and primitive-crypt
         (lambda (passwd salt)
           (bytes->string/utf-8
            (primitive-crypt (string->bytes/utf-8 passwd)
                             salt)))))
  )
