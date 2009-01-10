;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module config mzscheme
  (require "../spgsql.ss"
           (lib "class.ss")
           (lib "match.ss"))

  (provide HOST PORT USER DATABASE PASSWORD SSL SSL-ENCRYPT
           connect-for-test
           connect-and-setup
           call-with-connection
           
           set-equal?
           pg-error?
           spgsql-error?
           )
  
  (define-values (HOST PORT USER DATABASE PASSWORD SSL SSL-ENCRYPT)
    (values "localhost" 5432 "test" "test" "test" 'yes 'sslv2-or-v3))
  
  (define (connect-for-test)
    (connect HOST PORT DATABASE USER PASSWORD
             #:ssl SSL #:ssl-encrypt SSL-ENCRYPT))

  (define (connect-and-setup)
    (let [(cx (connect-for-test))]
      (send* cx 
        (set-notice-handler void)
        (set-notification-handler void)
        (exec "create temporary table the_numbers 
               (N integer primary key, description varchar(80))")
        (exec "insert into the_numbers values (1, 'unity')")
        (exec "insert into the_numbers (description, N)
               values ('the loneliest number since the number one', 2)")
        (exec "insert into the_numbers values (0, 'naught')")
        (exec "insert into the_numbers values (4, 'four');
               insert into the_numbers values (5, 'five');
               insert into the_numbers values (6, 'seven less 1')")
        (exec "create temporary table constrained
               (id integer check (id > 0),
                bigger_id integer constraint big_id check (bigger_id > id))"))
      cx))

  ;; set-equal? : ('a list) ('a list) -> boolean
  (define (set-equal? a b)
    (and (andmap (lambda (xa) (member xa b)) a)
         (andmap (lambda (xb) (member xb a)) b)
         #t))

  ;; pg-error? : string -> exn -> boolean
  ;; Create a predicate to test for an exn:spgsql which contains a particular
  ;; string in its message.
  (define (pg-error? str)
    (lambda (exn)
      (and (exn:spgsql? exn) 
           (regexp-match str (exn-message exn))
           #t)))

  ;; SYNTAX spgsql-error?
  ;; Creates a predicate to test for an exn:spgsql with a given type
  ;; and extra symbol.
  ;; (spgsql-error? exn-type sym) : exn -> boolean
  ;; (spgsql-error? exn-type) : exn -> boolean
  (define-syntax (spgsql-error? stx)
    (define (get-predicate type)
      (list-ref (syntax-local-value type) 2))
    (define (get-accessor type n)
      (list-ref (list-ref (syntax-local-value type) 3) n))
    (syntax-case stx ()
      [(_ exn-type sym)
       #`(lambda (x) 
           (and (#,(get-predicate #'exn-type) x)
                (eq? sym (#,(get-accessor #'exn-type 0) x))))]
      [(_ exn-type)
       #`(lambda (x)
           (#,(get-predicate #'exn-type) x))]))
  
  #;(define-syntax spgsql-error?
    (syntax-rules ()
      [(_ exn-type sym)
       (lambda (exn)
         (match exn
           [($ exn-type _ _ sym-2)
            (eq? sym sym-2)]
           [_ #f]))]
      [(_ exn-type)
       (lambda (exn)
         (match exn 
           [($ exn-type _ _)
            #t]
           [_ #f]))]))

  (define (call-with-connection f)
    (let [(c (connect-and-setup))]
      (dynamic-wind void
                    (lambda () (f c))
                    (lambda () (send c disconnect)))))
  
  )
