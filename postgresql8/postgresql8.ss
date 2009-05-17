#lang scheme/base
  
(require "../base.ss")

(require (prefix-in postgresql: (spgsql-in spgsql))
         (unlib-in gen symbol)
         "../base.ss"
         "../era/era.ss"
         "../generic/generic.ss"
         "../sql/sql-struct.ss"
         "sql.ss")
  
;  [#:server      string]
;  [#:port        integer]
;   #:database    string
;   #:username    string
;  [#:password    (U string #f)]
;  [#:ssl         (U 'yes 'no 'optional)]
;  [#:ssl-encrypt (U 'sslv2-or-v3 'sslv2 'sslv3 'tls)]
; ->
;  database%
(define (make-postgresql8-database 
         #:server      [server "localhost"]
         #:port        [port 5432]
         #:database    database
         #:username    username
         #:password    [password #f]
         #:ssl         [ssl 'optional]
         #:ssl-encrypt [ssl-encrypt 'sslv2-or-v3])
  (new postgresql8-database%
       [server      server]
       [port        port]
       [database    database]
       [username    username]
       [password    password]
       [ssl         ssl]
       [ssl-encrypt ssl-encrypt]))

; Database class ---------------------------------

(define postgresql8-database%
  (class* (generic-sql-query-mixin (postgresql8-sql-mixin generic-database%)) (database<%>)
    
    (inspect #f)
    
    (inherit escape-sql-name
             escape-sql-value
             get-snooze
             create-table-sql
             drop-table-sql
             insert-sql
             update-sql
             delete-sql
             direct-find-sql
             query-sql
             parse-value
             make-parser
             make-query-extractor
             make-single-item-extractor
             make-multiple-item-extractor)
    
    ; Fields -------------------------------------
    
    (init-field server ; string
      port             ; natural
      database         ; string
      username         ; string
      password         ; (U string #f)
      ssl              ; (U 'yes 'no 'optional)
      ssl-encrypt)     ; (U 'sslv2-or-v3 'sslv2 'sslv3 'tls)]
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; -> connection
    (define/public (connect)
      (with-snooze-reraise (exn:fail? "Could not connect to database")
        (define conn (postgresql:connect '#:server      server
                                         '#:port        port
                                         '#:database    database
                                         '#:user        username
                                         '#:password    password
                                         '#:ssl         ssl
                                         '#:ssl-encrypt ssl-encrypt))
        (send conn exec "SET client_min_messages TO warning;")
        (send conn exec "SET datestyle TO iso;")
        (send conn exec "SET regex_flavor TO extended;")
        ;(send conn exec "SET standard_conforming_strings TO on;")
        (make-connection conn #f)))
    
    ; connection -> void
    (define/public (disconnect conn)
      (with-snooze-reraise (exn:fail? "Could not disconnect from database")
        (send (connection-back-end conn) disconnect)))
    
    ; connection entity -> void
    (define/public (create-table conn entity)
      (with-snooze-reraise (exn:fail? (format "Could not create table for ~a" entity))
        (for-each (cut send (connection-back-end conn) exec <>)
                  (map (cut string-append <> ";")
                       (regexp-split #px";" (create-table-sql entity))))))
    
    ; connection entity -> void
    (define/public (drop-table conn entity)
      (with-snooze-reraise (exn:fail? (format "Could not drop table for ~a" entity))
        (for-each (cut send (connection-back-end conn) exec <>)
                  (map (cut string-append <> ";")
                       (regexp-split #px";" (drop-table-sql entity))))))
    
    ; connection guid -> ineteger
    ;
    ; Inserts a new database record for the struct and returns its ID.
    (define/public (insert-record conn guid)
      ; symbol
      (let ([sequence-name (symbol-append (entity-table-name (guid-entity guid)) '_seq)])
        ; integer
        (with-snooze-reraise (exn:fail? (format "Could not insert database record for ~a" guid))
          ; The two lines below work as follows:
          ;   - the first line inserts the record, using the SEQUENCE "entity_seq" to determine the new ID
          ;   - the second line reads the current value of "entity_seq", retrieving the ID of the new struct
          ; Note that there is no transaction around this: the PostgreSQL function "currval" returns
          ; a session-local value. The INSERT SQL from insert-sql lets PostgreSQL assign a new ID from the 
          ; sequence "entity_seq". Returns the new value of the ID sequence (and thus the ID of the 
          ; just-inserted element):
          (send (connection-back-end conn) exec (insert-sql (guid-ref guid)))
          (parse-value
           type:integer
           (send (connection-back-end conn) query-value
                 (string-append "SELECT currval('" (escape-sql-name sequence-name) "');"))))))
    
    ; connection guid -> void
    (define/public (update-record conn guid)
      (with-snooze-reraise (exn:fail? (format "Could not update database record for ~a" guid))
        (send (connection-back-end conn) exec (update-sql (guid-ref guid)))
        (void)))
    
    ; connection guid -> void
    (define/public (delete-record conn guid)
      (with-snooze-reraise (exn:fail? (format "Could not delete database record for ~a" guid))
        (send (connection-back-end conn) exec (delete-sql guid))
        (void)))
    
    ; (listof vanilla-guid) -> (listof interned-vanilla-guid)
    (define/public (direct-find conn guids)
      (if (null? guids)
          null
          (let ([sql    (direct-find-sql guids)]
                [entity (guid-entity (car guids))])
            (with-snooze-reraise (exn:fail? (format "Could not execute SELECT query:~n~a" sql))
              (g:collect
               (g:map (make-single-item-extractor entity)
                      (g:map (make-parser (map attribute-type (entity-attributes entity)))
                             (g:list (send (connection-back-end conn) map sql list)))))))))
    
    ; connection query -> result-generator
    ;
    ; TODO : This procedure is memory-inefficient, because it retrieves the entire result
    ; set as a list. We really want to fold over the results, sending individual results to
    ; the generator as we go. Here's some pseudocode:
    ;
    ; (send connection
    ;       for-each
    ;       sql
    ;       (lambda (result)
    ;         ; get result
    ;         ; capture continuation
    ;         ; send continuation and result to generator
    ;         ...))
    ; 
    ; (g:map process-data
    ;        (lambda ()
    ;          ; call continuation in iterator
    ;          ; get result and next continuation from iterator
    ;          ; store next continuation
    ;          ; emit result
    ;          ...))
    (define/public (g:find conn query)
      (let ([sql (query-sql query)])
        (with-snooze-reraise (exn:fail? (format "Could not execute SELECT query:~n~a" sql))
          (g:map (make-query-extractor query)
                 (g:map (make-parser (map expression-type (query-what query)))
                        (g:list (send (connection-back-end conn) map sql list)))))))
    
    ; connection -> boolean
    (define/public (transaction-allowed? conn)
      #t)
    
    ; connection thunk -> any
    (define/public (call-with-transaction conn body)
      ; symbol
      (define savepoint (gensym 'savepoint))
      ; (listof symbol)
      (define outermost? (not (connection-in-transaction? conn)))
      ; string
      (define escaped-savepoint (escape-sql-name savepoint))
      ; boolean
      (define complete? #f)
      ; Main body:
      (dynamic-wind
       (lambda ()
         ; If this is the outermost call to call-with-transaction, start a TRANSACTION:
         (when outermost?
           (send (connection-back-end conn) exec "BEGIN;")
           (set-connection-in-transaction?! conn #t))
         ; The actual COMMIT / ROLLBACK process is governed by SAVEPOINTS:
         (send (connection-back-end conn) exec (string-append "SAVEPOINT " escaped-savepoint ";")))
       (lambda ()
         (begin0 (body)
                 (set! complete? #t)))
       (lambda ()
         ; Commit or roll back:
         (if complete?
             (send (connection-back-end conn) exec (string-append "RELEASE SAVEPOINT " escaped-savepoint ";"))
             (send (connection-back-end conn) exec (string-append "ROLLBACK TO SAVEPOINT " escaped-savepoint ";")))
         ; If this is the outermost call to call-with-transaction, exit the TRANSACTION:
         (when outermost?
           (set-connection-in-transaction?! conn #f)
           (send (connection-back-end conn) exec "COMMIT;")))))
    
    ; connection -> (listof symbol)
    (define/public (table-names conn)
      (map (cut parse-value type:symbol <>)
           (send (connection-back-end conn) query-list 
                 "SELECT tablename FROM pg_tables WHERE schemaname = 'public' ORDER BY tablename;")))
    
    ; connection (U symbol entity) -> boolean
    (define/public (table-exists? conn table)
      ; string
      (define sql
        (format "SELECT relname FROM pg_class WHERE relname=~a;"
                (cond [(entity? table) (escape-sql-value type:symbol (entity-table-name table))]
                      [(symbol? table) (escape-sql-value type:symbol table)]
                      [else            (raise-exn exn:fail:snooze
                                         (format "Expected (U entity symbol), recevied ~s" table))])))
      ; connection -> list
      (define result (send (connection-back-end conn) query-list sql))
      ; boolean
      (not (null? result)))
    
    ; query output-port string -> query
    ;
    ; Prints an SQL string to stdout as a side effect.
    (define/public (debug-sql query [output-port (current-output-port)] [format "~a"])
      (fprintf output-port format (query-sql query))
      query)))

; Provide statements -----------------------------

; (contract symbol)
(define ssl/c
  (symbols 'yes 'no 'optional))
  
; (contract symbol)
(define ssl-encrypt/c
  (symbols 'sslv2-or-v3 'sslv2 'sslv3 'tls))

(provide (struct-out connection)
         postgresql8-database%)

(provide/contract
 [make-postgresql8-database
  (->* (#:database string? #:username string?)
       (#:server string? #:port integer? #:password (or/c string? false/c) #:ssl ssl/c #:ssl-encrypt ssl-encrypt/c)
       (is-a?/c database<%>))])
