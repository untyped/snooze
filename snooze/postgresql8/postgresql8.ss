#lang scheme/base
  
(require scheme/class
         scheme/contract)

(require (file "../generic/connection.ss")
         (file "../generic/database.ss")
         (file "database.ss"))
  
;  [#:server      string]
;  [#:port        integer]
;   #:database    string
;   #:username    string
;  [#:password    (U string #f)]
;  [#:ssl         (U 'yes 'no 'optional)]
;  [#:ssl-encrypt (U 'sslv2-or-v3 'sslv2 'sslv3 'tls)]
; ->
;  database%
(define (make-database 
         #:server      [server "localhost"]
         #:port        [port 5432]
         #:database    database
         #:username    username
         #:password    [password #f]
         #:ssl         [ssl 'optional]
         #:ssl-encrypt [ssl-encrypt 'sslv2-or-v3])
  (new database%
       [server      server]
       [port        port]
       [database    database]
       [username    username]
       [password    password]
       [ssl         ssl]
       [ssl-encrypt ssl-encrypt]))

; Provide statements -----------------------------

; (contract symbol)
(define ssl/c
  (symbols 'yes 'no 'optional))
  
; (contract symbol)
(define ssl-encrypt/c
  (symbols 'sslv2-or-v3 'sslv2 'sslv3 'tls))

(provide (struct-out connection)
         database%)

(provide/contract
 [make-database (->* (#:database string? #:username string?)
                      (#:server string? #:port integer? #:password (or/c string? false/c) #:ssl ssl/c #:ssl-encrypt ssl-encrypt/c)
                      (is-a?/c database<%>))])
