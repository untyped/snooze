#lang mzscheme

(require mzlib/kw
         scheme/class
         scheme/contract)

(require "../generic/connection.ss"
         "../generic/database.ss"
         "database.ss")

;  [#:server      string]
;  [#:port        integer]
;   #:database    string
;   #:username    string
;  [#:password    (U string #f)]
;  [#:ssl         (U 'yes 'no 'optional)]
;  [#:ssl-encrypt (U 'sslv2-or-v3 'sslv2 'sslv3 'tls)]
; ->
;  database%
(define/kw (make-database 
            #:key
            [server "localhost"]
            [port 5432]
            database
            username
            [password #f]
            [ssl 'optional]
            [ssl-encrypt 'sslv2-or-v3])
  (new database%
       [server      server]
       [port        port]
       [database    database]
       [username    username]
       [password    password]
       [ssl         ssl]
       [ssl-encrypt ssl-encrypt]))

; Provide statements -----------------------------

(provide (struct connection (back-end in-transaction?))
         database%
         make-database)
