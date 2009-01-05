;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

;; This file provides all the necessary structures, objects, and functions
;; for using spgsql to open a connection to a PostgreSQL database.

(module spgsql mzscheme
  (require "private/connection-structures.ss"
           "private/connection.ss"
           "private/exceptions.ss"
           "private/sql-types.ss"
           "crypto/md5.scm"
           "crypto/crypt.scm"
           (lib "class.ss")
           (lib "kw.ss"))
  
  (provide connection<%>
           (all-from "private/sql-types.ss")
           ;; exceptions
           (struct exn:spgsql ())
           (struct exn:spgsql:fatal ())
           (struct exn:spgsql:nonfatal ())
           (struct exn:spgsql:auth (type))
           (struct exn:spgsql:communication (type))
           (struct exn:spgsql:internal (location))
           (struct exn:spgsql:query ())
           (struct exn:spgsql:constraint ())
           (struct exn:spgsql:user (type)))
  
  (provide connect)
  
  ; [DJG] changed
  ;(define connect
  ;  (case-lambda
  ;    [(server port dbname username)
  ;     (connect/fixed server port dbname username #f)]
  ;    [(server port dbname username password)
  ;     (connect/fixed server port dbname username password)]))
  ; [DJG] to
  ;; connect 
  ;;     : string 
  ;;       integer 
  ;;       string
  ;;       string
  ;;       [(U string #f)]
  ;;       #:ssl      (U 'yes 'no 'optional)
  ;;       #:ssl-encrypt (U 'sslv2-or-v3 'sslv2 'sslv3 'tls)
  ;;    -> ConnectionResult
  (define connect
    (lambda/kw (server 
                port
                dbname
                username
                #:optional
                [password #f] 
                #:key
                [ssl 'no] 
                [ssl-encrypt 'sslv2-or-v3])
       (connect/fixed server port dbname username password ssl ssl-encrypt)))
  ; [DJG] /changed

  ; [DJG] changed
  ; (define (connect/fixed server port dbname username password ssl ssl-encrypt)
  ; [DJG] to
  ;; connect/fixed
  ;;     : string
  ;;       integer
  ;;       string
  ;;       string
  ;;       string 
  ;;       (U 'yes 'no 'optional)
  ;;       (U 'sslv2-or-v3 'sslv2 'sslv3 'tls)
  ;;    -> ConnectionResult
  (define (connect/fixed server port dbname username password ssl ssl-encrypt)
    ; [DJG] /changed
    (let* [(port (or port 5432))
           (dbname (or dbname "template1"))
           (cx (make-object connection%))
           ; [DJG] changed
           (auth (send cx connect server port dbname username ssl ssl-encrypt))]
      ; [DJG] /changed
      (let loop [(auth auth)]
        (cond [(OkConnection? auth)
               cx]
              [(EncryptedPasswordResult? auth)
               (when (not (string? password))
                 (raise-auth-error 
                  'password "You must supply a string as password"))
               (when (not crypt)
                 (raise-internal-error 
                  'crypt-not-supported
                  (string-append 
                   "Backend requested a crypt-encrypted password, which is not "
                   "available on this system.")))
               (loop ((EncryptedPasswordResult-callback auth)
                      (crypt password (EncryptedPasswordResult-salt auth))))]
              [(UnencryptedPasswordResult? auth)
               (when (not (string? password))
                 (raise-auth-error 
                  'password "You must supply a string as password"))
               (loop ((UnencryptedPasswordResult-callback auth) password))]
              [(MD5PasswordResult? auth)
               (when (not (string? password))
                 (raise-sp-user-error 
                  'password "You must supply a string as password"))
               (loop ((MD5PasswordResult-callback auth)
                      (md5password username password 
                                   (MD5PasswordResult-salt auth))))]
              [else
               (send cx disconnect)
               (raise-auth-error 
                'unsupported 
                (string-append 
                 "Authentication failed: the backend sent an "
                 "authentication request which spgsql cannot handle"))]))))

  ;; list-databases : connection% -> (listof string)
  (define (list-databases c)
    (send c query-list "select datname from pg_database"))
  
  ;; list-tables : connection% boolean -> (listof string)
  (define (list-tables c include-sys-catalogs?)
    (if include-sys-catalogs?
        (send c query-list "select relname from pg_class where relkind = 'r'")
        (send c query-list 
              (sql-format "select pg_class.relname "
                          "from pg_class left join pg_namespace "
                          "on pg_namespace.oid = pg_class.relnamespace"
                          "where pg_namespace.nspname !~~ '^pg_'"))))

  ;; list-fields : connection% string -> (listof string)
  (define (list-fields c table)
    (send c query-list
          (sql-format "select A.attname from pg_attribute A, pg_class T "
                      "where A.attrelid = T.relfilenode "
                      "and T.relname = '~a' and A.attnum > 0"
                      `(sql ,table))))

  ;; create-database : connection string -> void
  (define (create-database c dbname)
    (send c exec
          (sql-format "create database ~a"
                      `(sql ,dbname))))
  
  ;; SYNTAX: with-transaction
  (define-syntax with-transaction
    (syntax-rules ()
      [(_ connection body ...)
       (let [(c connection)]
         (unless (is-a? c connection%)
           (error 'with-transaction 
                  "expected a connection% object, given ~v" c))
         (begin 
           (send c exec "begin transaction")
           (begin0
             (begin body ...)
             (send c exec "commit transaction"))))]))
  )
