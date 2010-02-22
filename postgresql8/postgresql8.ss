#lang scheme/base

(require "../base.ss")

(require (for-syntax scheme/base)
         (only-in srfi/13 string-contains)
         (prefix-in postgresql: "../spgsql-hacked/spgsql.ss")
         (only-in "../spgsql-hacked/private/exceptions.ss" exn:spgsql:backend?)
         (unlib-in gen symbol)
         "../base.ss"
         "../core/struct.ss"
         "../core/snooze-struct.ss"
         "../common/common.ss"
         "../common/connection-pool.ss"
         "../sql/sql-struct.ss"
         "sql.ss")

(define-syntax-rule (debug-sql* fn arg ...)
  (let ([sql (fn arg ...)])
    ;(printf "~a~n" sql)
    sql))

;  [#:server                 string]
;  [#:port                   integer]
;   #:database               string
;   #:username               string
;  [#:password               (U string #f)]
;  [#:ssl                    (U 'yes 'no 'optional)]
;  [#:ssl-encrypt            (U 'sslv2-or-v3 'sslv2 'sslv3 'tls)]
;  [#:pool-connections?      boolean]
;  [#:keepalive-milliseconds natural]
; ->
;  database%
(define (make-postgresql8-database 
         #:server                  [server "localhost"]
         #:port                    [port 5432]
         #:database                database
         #:username                username
         #:password                [password #f]
         #:ssl                     [ssl 'optional]
         #:ssl-encrypt             [ssl-encrypt 'sslv2-or-v3]
         #:pool-connections?       [pool-connections? #t]
         #:keepalive-milliseconds  [keepalive-milliseconds 5000])
  (if pool-connections?
      (new (connection-pool-mixin postgresql8-database%)
           [server                 server]
           [port                   port]
           [database               database]
           [username               username]
           [password               password]
           [ssl                    ssl]
           [ssl-encrypt            ssl-encrypt]
           [keepalive-milliseconds keepalive-milliseconds])
      (new postgresql8-database%
           [server                 server]
           [port                   port]
           [database               database]
           [username               username]
           [password               password]
           [ssl                    ssl]
           [ssl-encrypt            ssl-encrypt])))

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
             make-multiple-item-extractor
             make-query-cross-referencer)
    
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
      (with-handlers ([exn:spgsql:backend? reraise-connect])
        (define conn (postgresql:connect '#:server      server
                                         '#:port        port
                                         '#:database    database
                                         '#:user        username
                                         '#:password    password
                                         '#:ssl         ssl
                                         '#:ssl-encrypt ssl-encrypt))
        (send conn exec
              "SET client_min_messages = 'ERROR';"
              "SET datestyle = iso;"
              "SET regex_flavor = extended;"
              "SET standard_conforming_strings = on;")
        (make-connection conn #f)))
    
    ; connection -> void
    (define/public (disconnect conn)
      (with-handlers ([exn:spgsql:backend? (cut reraise <> "could not disconnect from database")])
        (send (connection-back-end conn) disconnect)))
    
    ; connection entity -> void
    (define/public (create-table conn entity)
      (with-handlers ([exn:spgsql:backend? (cut reraise <> (format "could not create table: ~a" entity))])
        (for-each (cut send (connection-back-end conn) exec <>)
                  (map (cut string-append <> ";")
                       (regexp-split #px";" (debug-sql* create-table-sql entity))))))
    
    ; connection entity -> void
    (define/public (drop-table conn entity)
      (with-handlers ([exn:spgsql:backend? (cut reraise <> (format "could not drop table: ~a" entity))])
        (for-each (cut send (connection-back-end conn) exec <>)
                  (map (cut string-append <> ";")
                       (regexp-split #px";" (debug-sql* drop-table-sql entity))))))
    
    ; connection snooze-struct -> snooze-struct
    ; Inserts a new database record for the supplied struct.
    (define/public (insert-struct conn old-struct)
      (with-handlers ([exn:spgsql:backend? (cut reraise <> (format "could not insert row: ~a" old-struct))])
        (let* ([entity       (snooze-struct-entity old-struct)]
               [seq-name     (symbol-append (entity-table-name entity) '_seq)]
               [guid         (snooze-struct-guid old-struct)]
               [revision     (snooze-struct-revision old-struct)])
          (set-guid-id! guid (send (connection-back-end conn)
                                   query-value
                                   (debug-sql* format "SELECT nextval('~a');" (escape-sql-name seq-name))))
          (let ([new-struct (apply (entity-private-constructor entity)
                                   guid
                                   (or revision 0)
                                   (cddr (snooze-struct-raw-ref* old-struct)))])
            (send (connection-back-end conn) exec (debug-sql* insert-sql new-struct))
            new-struct))))
    
    ; connection snooze-struct [boolean] -> snooze-struct
    ; Updates the existing database record for the supplied struct.
    (define/public (update-struct conn old-struct [check-revision? #t])
      (with-handlers ([exn:spgsql:backend? (cut reraise <> (format "could not update row: ~a" old-struct))])
        (let ([entity   (snooze-struct-entity   old-struct)]
              [guid     (snooze-struct-guid     old-struct)]
              [revision (snooze-struct-revision old-struct)])
          (when check-revision? (check-revision conn entity guid revision))
          (let ([ans (apply (entity-private-constructor entity)
                            guid
                            (add1 revision)
                            (cddr (snooze-struct-raw-ref* old-struct)))])
            (send (connection-back-end conn) exec (debug-sql* update-sql ans))
            ans))))
    
    ; connection snooze-struct [boolean] -> snooze-struct
    ; Deletes the database record for the supplied struct.
    (define/public (delete-struct conn old-struct [check-revision? #t])
      (with-handlers ([exn:spgsql:backend? (cut reraise <> (format "could not delete row: ~a" old-struct))])
        (let ([entity   (snooze-struct-entity old-struct)]
              [guid     (snooze-struct-guid old-struct)]
              [revision (snooze-struct-revision old-struct)])
          (when check-revision?
            (check-revision conn entity guid revision))
          (send (connection-back-end conn) exec (debug-sql* delete-sql (snooze-struct-guid old-struct)))
          (set-guid-temporary-id! guid)
          (apply (entity-private-constructor entity)
                 guid
                 #f
                 (cddr (snooze-struct-raw-ref* old-struct))))))
    
    ; connection database-guid -> void
    ; Deletes the database record for the supplied guid.
    (define/public (delete-guid conn guid)
      (with-handlers ([exn:spgsql:backend? (cut reraise <> (format "could not delete row for guid: ~a" guid))])
        (send (connection-back-end conn) exec (debug-sql* delete-sql guid))
        (void)))
    
    ; connection entity database-guid natural -> void
    (define (check-revision conn entity guid expected)
      (let ([actual (send (connection-back-end conn) query-value
                          (debug-sql* format "SELECT revision FROM ~a WHERE guid = ~a;"
                                      (escape-sql-name (entity-table-name entity))
                                      (guid-id guid)))])
        (unless (equal? actual expected)
          (raise-exn exn:fail:snooze:revision
            (format "revision mismatch: database ~a, struct ~a" actual expected)
            guid))))
    
    ; connection (listof database-guid) transaction-frame -> (gen-> snooze-struct)
    (define/public (direct-find conn guids frame)
      (if (null? guids)
          null
          (let* ([sql     (debug-sql* direct-find-sql guids)]
                 [entity  (guid-entity (car guids))]
                 [extract (make-single-item-extractor entity)])
            (with-handlers ([exn:spgsql:backend? (cut reraise <> (format "could not execute query: ~a" sql))])
              (g:map (cut extract <> frame)
                     (g:map (make-parser (map attribute-type (entity-attributes entity)))
                            (g:list (send (connection-back-end conn) map sql list))))))))
    
    ; connection query transaction-frame -> result-generator
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
    (define/public (g:find conn query frame)
      (let ([sql     (debug-sql* query-sql query)]
            [extract (make-query-extractor query)]
            [xref    (make-query-cross-referencer query)])
        (with-handlers ([exn:spgsql:backend? (cut reraise <> (format "could not execute query: ~a" sql))])
          (g:map (lambda (item)
                   (xref (extract item frame) frame))
                 (g:map (make-parser (map expression-type (query-what query)))
                        (g:list (send (connection-back-end conn) map sql list)))))))
    
    ; -> boolean
    (define/public (supports-nested-transactions?)
      #t)
    
    ; connection -> boolean
    (define/public (transaction-allowed? conn)
      #t)
    
    ; connection thunk -> any
    (define/public (call-with-transaction conn body)
      ; symbol
      (define savepoint (gensym 'savepoint))
      ; boolean
      (define started-once? #f)
      ; (listof symbol)
      (define outermost? (not (connection-in-transaction? conn)))
      ; string
      (define escaped-savepoint (escape-sql-name savepoint))
      ; boolean
      (define complete? #f)
      ; Main body:
      (dynamic-wind
       (lambda ()
         ; Can't jump out of and back into a transaction block:
         (if started-once?
             (error "attempt to re-enter a terminated transaction block")
             (set! started-once? #t))
         ; If this is the outermost call to call-with-transaction, start a TRANSACTION:
         (when outermost?
           (send (connection-back-end conn) exec (debug-sql* string-append "BEGIN;"))
           (set-connection-in-transaction?! conn #t))
         ; The actual COMMIT / ROLLBACK process is governed by SAVEPOINTS:
         (send (connection-back-end conn) exec (debug-sql* string-append "SAVEPOINT " escaped-savepoint ";")))
       (lambda ()
         (begin0 (body)
                 (set! complete? #t)))
       (lambda ()
         ; Commit or roll back:
         (if complete?
             (send (connection-back-end conn) exec (debug-sql* string-append "RELEASE SAVEPOINT " escaped-savepoint ";"))
             (send (connection-back-end conn) exec (debug-sql* string-append "ROLLBACK TO SAVEPOINT " escaped-savepoint ";")))
         ; If this is the outermost call to call-with-transaction, exit the TRANSACTION:
         (when outermost?
           (set-connection-in-transaction?! conn #f)
           (send (connection-back-end conn) exec (debug-sql* string-append "COMMIT;"))))))
    
    ; connection -> (listof symbol)
    (define/public (table-names conn)
      (map (cut parse-value type:symbol <>)
           (send (connection-back-end conn) query-list 
                 "SELECT tablename FROM pg_tables WHERE schemaname = 'public' ORDER BY tablename;")))
    
    ; connection (U symbol entity) -> boolean
    (define/public (table-exists? conn table)
      ; string
      (define sql
        (debug-sql* format "SELECT relname FROM pg_class WHERE relname=~a;"
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

; Helpers ----------------------------------------

; exn -> void
(define (reraise-connect exn)
  (cond [(string-contains (exn-message exn) "connection limit exceeded")
         (raise-exn exn:fail:snooze:connection-count
           (format "could not connect to database: ~a" (exn-message exn)))]
        [else (reraise exn "could not connect to database")]))

; exn -> void
(define (reraise exn msg)
  (raise-exn exn:fail:snooze
    (format "~a: ~a" msg (exn-message exn))))

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
       (#:server string?
                 #:port                   integer?
                 #:password               (or/c string? #f)
                 #:ssl                    ssl/c
                 #:ssl-encrypt            ssl-encrypt/c
                 #:pool-connections?      boolean?
                 #:keepalive-milliseconds natural-number/c)
       (is-a?/c database<%>))])
