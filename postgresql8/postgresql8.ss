#lang scheme/base

(require "../base.ss")

(require (prefix-in postgresql: (spgsql-in spgsql))
         (unlib-in gen symbol)
         "../base.ss"
         "../era/core.ss"
         "../era/snooze-struct.ss"
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
      (with-snooze-reraise (exn:fail? "could not connect to database")
        (define conn (postgresql:connect '#:server      server
                                         '#:port        port
                                         '#:database    database
                                         '#:user        username
                                         '#:password    password
                                         '#:ssl         ssl
                                         '#:ssl-encrypt ssl-encrypt))
        (send conn exec "SET client_min_messages = 'ERROR';")
        (send conn exec "SET datestyle = iso;")
        (send conn exec "SET regex_flavor = extended;")
        ;(send conn exec "SET standard_conforming_strings = on;")
        (make-connection conn #f)))
    
    ; connection -> void
    (define/public (disconnect conn)
      (with-snooze-reraise (exn:fail? "could not disconnect from database")
        (send (connection-back-end conn) disconnect)))
    
    ; connection entity -> void
    (define/public (create-table conn entity)
      (with-snooze-reraise (exn:fail? (format "could not create table for ~a" entity))
        (for-each (cut send (connection-back-end conn) exec <>)
                  (map (cut string-append <> ";")
                       (regexp-split #px";" (create-table-sql entity))))))
    
    ; connection entity -> void
    (define/public (drop-table conn entity)
      (with-snooze-reraise (exn:fail? (format "could not drop table for ~a" entity))
        (for-each (cut send (connection-back-end conn) exec <>)
                  (map (cut string-append <> ";")
                       (regexp-split #px";" (drop-table-sql entity))))))
    
    ; connection snooze-struct -> snooze-struct
    ; Inserts a new database record for the supplied struct.
    (define/public (insert-struct conn old-struct)
      (with-snooze-reraise (exn:fail? (format "could not insert database record for ~a" old-struct))
        (let* ([cache        (send (get-snooze) get-current-cache)]
               [entity       (snooze-struct-entity old-struct)]
               [guid-type    (attribute-type (car (entity-attributes entity)))]
               [seq-name     (symbol-append (entity-table-name entity) '_seq)]
               [guid         (snooze-struct-guid old-struct)]
               [revision     (snooze-struct-revision old-struct)]
               [new-struct   (apply (entity-private-constructor entity)
                                    guid
                                    (or revision 0)
                                    (for/list ([val (in-list (cddr (snooze-struct-ref* old-struct)))])
                                      (if (guid? val)
                                          (send cache get-saveable-guid val)
                                          val)))])
          (send (connection-back-end conn) exec (insert-sql new-struct))
          (let ([id (send (connection-back-end conn) query-value
                          (string-append "SELECT currval('" (escape-sql-name seq-name) "');"))])
            (apply (entity-private-constructor entity)
                   (parse-value guid-type id)
                   (cdr (snooze-struct-ref* new-struct)))))))
    
    ; connection snooze-struct [boolean] -> snooze-struct
    ; Updates the existing database record for the supplied struct.
    (define/public (update-struct conn old-struct [check-revision? #t])
      (with-snooze-reraise (exn:fail? (format "could not update database record for ~a" old-struct))
        (let ([cache    (send (get-snooze) get-current-cache)]
              [entity   (snooze-struct-entity old-struct)]
              [guid     (snooze-struct-guid old-struct)]
              [revision (snooze-struct-revision old-struct)])
          (when check-revision?
            (check-revision conn entity guid revision))
          (let ([new-struct (apply (entity-private-constructor entity)
                                   guid
                                   (add1 revision)
                                   (for/list ([val (in-list (cddr (snooze-struct-ref* old-struct)))])
                                     (if (guid? val)
                                         (send cache get-saveable-guid val)
                                         val)))])
            (send (connection-back-end conn) exec (update-sql new-struct))
            new-struct))))
    
    ; connection snooze-struct [boolean] -> snooze-struct
    ; Deletes the database record for the supplied struct.
    (define/public (delete-struct conn old-struct [check-revision? #t])
      (with-snooze-reraise (exn:fail? (format "could not insert database record for ~a" old-struct))
        (let ([cache    (send (get-snooze) get-current-cache)]
              [entity   (snooze-struct-entity old-struct)]
              [guid     (snooze-struct-guid old-struct)]
              [revision (snooze-struct-revision old-struct)])
          (when check-revision?
            (check-revision conn entity guid revision))
          (let ([new-struct (apply (entity-private-constructor entity)
                                   #f
                                   #f
                                   (for/list ([val (in-list (cddr (snooze-struct-ref* old-struct)))])
                                     (if (guid? val)
                                         (send cache get-saveable-guid val)
                                         val)))])
            (send (connection-back-end conn) exec (delete-sql (snooze-struct-guid old-struct)))
            new-struct))))
    
    ; connection vanilla-guid -> void
    ; Deletes the database record for the supplied guid.
    (define/public (delete-guid conn guid)
      (with-snooze-reraise (exn:fail? (format "could not insert database record for ~a" guid))
        (send (connection-back-end conn) exec (delete-sql guid))
        (void)))
    
    ; connection entity vanilla-guid natural -> void
    (define (check-revision conn entity guid expected)
      (let ([actual (send (connection-back-end conn) query-value
                          (format "SELECT revision FROM ~a WHERE guid = ~a;"
                                  (escape-sql-name (entity-table-name entity))
                                  (guid-id guid)))])
        (unless (equal? actual expected)
          (error (format "revision mismatch: database ~a, struct ~a" actual expected)))))
    
    ; (listof vanilla-guid) -> (listof local-guid)
    (define/public (direct-find conn guids)
      (if (null? guids)
          null
          (let ([sql    (direct-find-sql guids)]
                [entity (guid-entity (car guids))])
            (with-snooze-reraise (exn:fail? (format "could not execute SELECT query:~n~a" sql))
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
        (with-snooze-reraise (exn:fail? (format "could not execute SELECT query:~n~a" sql))
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
