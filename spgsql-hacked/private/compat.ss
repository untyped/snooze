;; Copyright 2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module compat mzscheme
  (require (planet "version-case.ss" ("dyoo" "version-case.plt" 1 4)))
  (require (prefix kw: (lib "kw.ss")))

  (provide lambda/kw)

  (version-case
   ((version< (version) "3.99")
    ;; Only one keyword protocol
    (define-syntax lambda/kw
      (make-rename-transformer #'kw:lambda/kw)))

   (else
    ;; For 3.99:
    ;; Need to support both old and new keyword protocols
    (require (only scheme make-keyword-procedure))
    (define-syntax lambda/kw
      (syntax-rules ()
        [(lambda/kw formals . body)
         (let ([inner-proc (kw:lambda/kw formals . body)])
           (make-keyword-procedure
            (lambda (kws kwargs . rest)
              (apply inner-proc
                     (append (apply append (map list kws kwargs)) rest)))))])))))
