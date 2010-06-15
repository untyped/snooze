;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

;; Implementation of connections, which communicate with a backend through
;; structured messages.
(module connection mzscheme
  (require (lib "etc.ss")
           (lib "class.ss")
           (lib "plt-match.ss")
           (lib "md5.ss")
           (lib "mzssl.ss" "openssl")
           "interfaces.ss"
           "p3.ss"
           "p3-msg.ss"
           "exceptions.ss"
           "sql-data.ss")
  (provide pure-connection%
           connection%)

  ;; Debugging
  (define DEBUG-RESPONSES #f)
  (define DEBUG-SENT-MESSAGES #f)

  ;; A PreparedStatement is:
  ;;   (make-PreparedStatement string (list-of number) number (weak-box connection<%>))
  (define-struct PreparedStatement (name param-types results wcx))

  ;; A Statement is one of:
  ;;   - string
  ;;   - (make-StatementBinding PreparedStatement (list-of string))
  (define-struct StatementBinding (pst params) #f)

  ;; base%
  (define base%
    (class* object% (base<%>)
      ;; protocol : protocol3
      (init-field [protocol #f]
                  [process-id #f]
                  [secret-key #f])

      (super-new)

      ;; with-disconnect-on-error
      ;; Specialized to use direct method call rather than 'send'
      (define-syntax with-disconnect-on-error
        (syntax-rules ()
          [(with-disconnect-on-error expr)
           (with-handlers ([exn:fail?
                            (lambda (e)
                              (disconnect #f)
                              (raise e))])
             expr)]))

      ;; Communication Methods

      ;; new-exchange : -> stream
      (define/public-final (new-exchange)
        (unless protocol
          (raise-user-error 'connection<%> "not connected"))
        (protocol3:new-exchange protocol))

      ;; end-exchange : -> void
      (define/public-final (end-exchange)
        (unless protocol
          (raise-user-error 'connection<%> "not connected"))
        (protocol3:end-exchange protocol))

      ;; recv : symbol/#f stream -> message stream
      ;; Automatically handles asynchronous messages
      (define/public-final (recv behalf mg)
        (unless protocol
          (raise-user-error 'connection<%> "not connected"))
        (let-values ([(r mg) (with-disconnect-on-error (stream:current+next mg))])
          (when DEBUG-RESPONSES
            (fprintf (current-error-port) "  << ~s\n" r))
          (match r
            [(? ErrorResponse?)
             (raise-backend-error behalf r)]
            [(struct NoticeResponse (properties))
             (handle-notice properties)
             (recv behalf mg)]
            [(struct NotificationResponse (process-id condition info))
             (handle-notification process-id condition info)
             (recv behalf mg)]
            [(struct ParameterStatus (name value))
             (handle-parameter-status name value)
             (recv behalf mg)]
            [else
             (values r mg)])))

      ;; buffer-message : message -> void
      (define/public-final (buffer-message msg)
        (unless protocol
          (raise-user-error 'connection<%> "not connected"))
        (when DEBUG-SENT-MESSAGES
          (fprintf (current-error-port) "  >> ~s\n" msg))
        (with-disconnect-on-error
         (protocol3:encode protocol msg)))

      ;; flush-message-buffer : -> void
      (define/public-final (flush-message-buffer)
        (with-disconnect-on-error
         (protocol3:flush protocol)))

      ;; send-message : message -> void
      (define/public-final (send-message msg)
        (buffer-message msg)
        (flush-message-buffer))

      ;; Connection management

      ;; disconnect : [boolean] -> (void)
      (define/public disconnect
        (case-lambda
          [() (disconnect #t)]
          [(politely?)
           (when protocol
             (when politely?
               (let ([_ (new-exchange)])
                 (send-message (make-Terminate))
                 (end-exchange)))
             (protocol3:close protocol))
           (set! protocol #f)]))

      ;; connected? : -> boolean
      (define/public (connected?)
        (and protocol #t))

      ;; Asynchronous message hooks

      ;; handle-parameter-status : string string -> void
      (define/public (handle-parameter-status name value)
        (when (equal? name "client_encoding")
          (unless (equal? value "UTF8")
            (disconnect)
            (error 'connection "client encoding must be UTF8, changed to: ~e" value)))
        (void))

      ;; handle-notice : (listof (cons symbol string)) -> void
      (define/public (handle-notice properties)
        (fprintf (current-error-port)
                 "notice: ~a (SQL code ~a)\n" 
                 (cdr (assq 'message properties))
                 (cdr (assq 'code properties))))

      ;; handle-notification : number string string -> void
      (define/public (handle-notification process-id condition info)
        (fprintf (current-error-port)
                 "notification ~a: ~a\n"
                 condition
                 info))

      ;; Initialization

      (define/public (after-connect)
        (void))
      ))

  (define connector-mixin
    (mixin (base<%>) (connector<%>)
      (init-field [allow-cleartext-password? #f])

      (inherit-field protocol
                     process-id
                     secret-key)
      (inherit recv
               new-exchange
               end-exchange
               buffer-message
               send-message
               disconnect
               after-connect)
      (super-new)

      ;; with-disconnect-on-error
      ;; Specialized to use direct method call rather than 'send'
      (define-syntax with-disconnect-on-error
        (syntax-rules ()
          [(with-disconnect-on-error expr)
           (with-handlers ([exn:fail?
                            (lambda (e)
                              (disconnect #f)
                              (raise e))])
             expr)]))

      ;; attach-to-ports : input-port output-port -> void
      (define/public (attach-to-ports in out)
        (set! protocol (protocol3:new in out)))

      ;; start-connection-protocol : string string string/#f -> void
      (define/public (start-connection-protocol dbname username password)
        (with-disconnect-on-error
         (let [(mg (new-exchange))]
           (send-message
            (make-StartupMessage
             (list (cons "user" username)
                   (cons "database" dbname)
                   (cons "client_encoding" "UTF8")
                   (cons "DateStyle" "ISO, MDY"))))
           (expect-auth mg username password))))

      ;; expect-auth : stream string/#f -> ConnectionResult
      (define/private (expect-auth mg username password)
        (let-values [((r mg) (recv 'connect mg))]
          (match r
            [(struct AuthenticationOk ())
             (expect-ready-for-query mg)]
            [(struct AuthenticationCleartextPassword ())
             (handle-cleartext-password-authentication password)
             (expect-auth mg username password)]
            [(struct AuthenticationCryptPassword (salt))
             (handle-crypt-password-authentication password salt)
             (expect-auth mg username password)]
            [(struct AuthenticationMD5Password (salt))
             (handle-md5-password-authentication username password salt)
             (expect-auth mg username password)]
            [(struct AuthenticationKerberosV5 ())
             (handle-kerberos5-authentication)
             (expect-auth mg username password)]
            [(struct AuthenticationSCMCredential ())
             (handle-scm-credential-authentication)
             (expect-auth mg username password)]
            [_
             (error 'connect
                    "authentication failed (backend sent unexpected message)")])))

      ;; expect-ready-for-query : stream -> void
      (define/private (expect-ready-for-query mg)
        (let-values [((r mg) (recv 'connect mg))]
          (match r
            [(struct ReadyForQuery (status))
             (end-exchange)
             (after-connect)]
            [(struct BackendKeyData (pid secret))
             (set! process-id pid)
             (set! secret-key secret)
             (expect-ready-for-query mg)]
            [_
             (error 'connect
                    (string-append "connection failed after authentication "
                                   "(backend sent unexpected message)"))])))

      ;; Authentication hooks
      ;; The authentication hooks serve two purposes:
      ;;   - to handle unsupported mechanisms (eg kerberos)
      ;;   - to prevent undesirable authentication methods
      ;;     (such as sending passwords in cleartext)
      ;; An authentication hook should take any necessary action (eg send one or more
      ;; messages to the protocol) and then return to continue the authentication 
      ;; process, or raise an error to abort the connection.

      ;; handle-cleartext-password-authentication : string -> void
      (define/private (handle-cleartext-password-authentication password)
        (unless (string? password)
          (raise-user-error 'connect "password needed but not supplied"))
        (send-message (make-PasswordMessage (compute-cleartext-password password))))

      ;; compute-cleartext-password : string -> string
      (define/public (compute-cleartext-password password)
        (unless allow-cleartext-password?
          (raise-user-error 'connect (nosupport "cleartext password")))
        password)

      ;; handle-crypt-password-authentication : string bytes -> void
      (define/private (handle-crypt-password-authentication password salt)
        (send-message (make-PasswordMessage (compute-crypt-password password salt))))

      ;; compute-crypt-password : string bytes -> void
      (define/public (compute-crypt-password password salt)
        (raise-user-error 'connect (nosupport "crypt()-encrypted password")))

      ;; handle-md5-password-authentication : string string bytes -> void
      (define/private (handle-md5-password-authentication user password salt)
        (send-message (make-PasswordMessage (compute-md5-password user password salt))))

      ;; compute-md5-password : strin string bytes -> bytes
      (define/public (compute-md5-password user password salt)
        (unless (string? password)
          (raise-user-error 'connect "password needed but not supplied"))
        (md5password user password salt))

      ;; handle-kerberos5-authentication : -> void
      (define/public (handle-kerberos5-authentication)
        (raise-user-error 'connect (nosupport "KerberosV5 authentication")))

      ;; handle-scm-credential-authentication : -> void
      (define/public (handle-scm-credential-authentication)
        (raise-user-error 'connect (nosupport "SCM authentication")))

      ))

  ;; ssl-connector-mixin
  ;; Adds SSL connection support.
  (define ssl-connector-mixin
    (mixin (connector<%> base<%>) (ssl-connector<%>)
      (field [ssl 'no]
             [ssl-encrypt 'sslv2-or-v3])
      (super-new)

      ;; set-ssl-options : YesNoOptional/#f SSLMode/#f -> void
      (define/public (set-ssl-options -ssl -ssl-encrypt)
        (unless (memq -ssl '(yes no optional))
          (raise-user-error 'set-ssl-options
                            "bad ssl option: expected 'yes, 'no, or 'optional, got: ~e"
                            -ssl))
        (when -ssl (set! ssl -ssl))
        (when -ssl-encrypt (set! ssl-encrypt -ssl-encrypt)))

      ;; attach-to-ports : input-port output-port -> void
      (define/override (attach-to-ports in out)
        (with-handlers ([(lambda _ #t)
                         (lambda (e)
                           (close-input-port in)
                           (close-output-port out)
                           (raise e))])
          (case ssl
            ((yes optional)
             ;; Try negotiating SSL connection
             (write-message (make-SSLRequest) out)
             (flush-output out)
             (let ([response (peek-byte in)])
               (case (integer->char response)
                 ((#\S)
                  (void (read-byte in))
                  (let-values ([(sin sout)
                                (ports->ssl-ports in out
                                                  #:mode 'connect
                                                  #:encrypt ssl-encrypt 
                                                  #:close-original? #t)])
                    (super attach-to-ports sin sout)))
                 ((#\N)
                  ;; Backend gracefully declined
                  (void (read-byte in))
                  (unless (eq? ssl 'optional)
                    (raise-user-error 'connect "backend does not support SSL"))
                  (super attach-to-ports in out))
                 ((#\E)
                  (let ([r (parse-server-message in)])
                    (raise-backend-error 'connect r)))
                 (else
                  (error 'connect "backend returned invalid response to SSL request")))))
            ((no)
             (super attach-to-ports in out)))))))

  ;; nosupport : string -> string
  (define (nosupport str)
    (string-append "spgsql does not support " str))

  ;; md5password : string string bytes -> string
  ;; Compute the MD5 hash of a password in the form expected by the PostgreSQL 
  ;; backend.
  (define (md5password user password salt)
    (bytes->string/latin-1
     (md5password/bytes (string->bytes/latin-1 user)
                        (string->bytes/latin-1 password)
                        salt)))
  (define (md5password/bytes user password salt)
    (let* [(s (md5 (bytes-append password user)))
           (t (md5 (bytes-append s salt)))]
      (bytes-append #"md5" t)))

  (define (list-FieldInfo-type-oid fi) (list-ref fi 3))
  (define (list-FieldInfo-field-name fi) (car fi))

  ;; primitive-query-mixin
  ;; Handles the mechanics of connection creations, queries, etc.
  ;; Provides functionality, not usability. See connection% for friendly 
  ;; interface.
  (define primitive-query-mixin
    (mixin (base<%>) (primitive-query<%>)
      (inherit-field protocol
                     process-id
                     secret-key)
      (inherit recv
               send-message
               buffer-message
               flush-message-buffer
               new-exchange
               end-exchange)
      (super-new)

      ;; name-counter : number
      (define name-counter 0)

      (define-syntax with-final-end-exchange
        (syntax-rules ()
          [(with-final-end-exchange . b)
           (dynamic-wind
               void
               (lambda () . b)
               (lambda () (end-exchange)))]))

      ;; query* : symbol (list-of Statement) Collector -> (list-of QueryResult)
      ;; The single point of control for the query engine
      (define/public (query* fsym stmts collector)
        (for-each (lambda (stmt) (check-statement fsym stmt)) stmts)
        (let ([mg (new-exchange)])
          (with-final-end-exchange
            (for-each (lambda (stmt) (query1:enqueue stmt)) stmts)
            (send-message (make-Sync)))
          (let loop ([stmts stmts] [mg mg])
            (if (null? stmts)
                (begin (check-ready-for-query mg)
                       null)
                (let-values ([(result mg) (query1:collect mg (car stmts) collector)])
                  (cons result (loop (cdr stmts) mg)))))))

      ;; check-ready-for-query : stream -> void
      (define/private (check-ready-for-query mg)
        (let-values ([(r mg) (recv 'query* mg)])
          (unless (ReadyForQuery? r)
            (error 'query* "backend sent unexpected message after query results"))))

      ;; check-statement : symbol any -> void
      (define/private (check-statement fsym stmt)
        (unless (or (string? stmt) (StatementBinding? stmt))
          (raise-type-error fsym "string or StatementBinding" stmt))
        (when (StatementBinding? stmt)
          (let ([pst (StatementBinding-pst stmt)])
            (unless (PreparedStatement? pst)
              (raise-type-error 
               fsym
               "StatementBinding containing prepared statement" stmt))
            (unless (eq? this (weak-box-value (PreparedStatement-wcx pst)))
              (raise-mismatch-error 
               fsym
               "prepared statement owned by another connection" stmt)))))

      ;; query1:enqueue : Statement -> void
      (define/private (query1:enqueue stmt)
        (if (string? stmt)
            (begin (buffer-message (make-Parse "" stmt null))
                   (buffer-message (make-Bind "" "" null null null)))
            (let* ([pst (StatementBinding-pst stmt)]
                   [pst-name (PreparedStatement-name pst)]
                   [params (StatementBinding-params stmt)])
              (buffer-message (make-Bind "" pst-name null params null))))
        (buffer-message (begin-lifted (make-Describe 'portal "")))
        (buffer-message (begin-lifted (make-Execute "" 0)))
        (buffer-message (begin-lifted (make-Close 'portal ""))))

      ;; query1:collect : stream Statement Collector -> QueryResult stream
      (define/private (query1:collect mg stmt collector)
        (if (string? stmt)
            (query1:expect-parse-complete mg collector)
            (query1:expect-bind-complete mg collector)))
      (define/private (query1:expect-parse-complete mg collector)
        (let-values ([(r mg) (recv 'query* mg)])
          (match r
            [(struct ParseComplete ())
             (query1:expect-bind-complete mg collector)]
            [_ (query1:error-recovery r mg)])))
      (define/private (query1:expect-bind-complete mg collector)
        (let-values ([(r mg) (recv 'query* mg)])
          (match r
            [(struct BindComplete ())
             (query1:expect-portal-description mg collector)]
            [_ (query1:error-recovery r mg)])))
      (define/private (query1:expect-portal-description mg collector)
        (let-values ([(r mg) (recv 'query* mg)])
          (match r
            [(struct RowDescription (rows))
             (let-values ([(init combine finalize info) (collector rows)])
               (query1:data-loop mg init combine finalize info))]
            [(struct NoData ())
             (query1:expect-completion mg)]
            [_ (query1:error-recovery r mg)])))
      (define/private (query1:data-loop mg init combine finalize info)
        (let-values ([(r mg) (recv 'query* mg)])
          (match r
            [(struct DataRow (value))
             (query1:data-loop mg 
                               (apply combine init value)
                               combine
                               finalize
                               info)]
            [(struct CommandComplete (command))
             (query1:finalize mg (make-Recordset info (finalize init)))]
            [_ (query1:error-recovery r mg)])))
      (define/private (query1:expect-completion mg)
        (let-values ([(r mg) (recv 'query* mg)])
          (match r
            [(struct CommandComplete (command))
             (query1:finalize mg (make-SimpleResult command))]
            [(struct EmptyQueryResponse ())
             (query1:finalize mg (make-SimpleResult #f))]
            [_ (query1:error-recovery r mg)])))
      (define/private (query1:finalize mg result)
        (let-values ([(r mg) (recv 'query* mg)])
          (match r
            [(struct CloseComplete ())
             (values result mg)]
            [_ (query1:error-recovery r mg)])))
      (define/private (query1:error-recovery r mg)
        (match r
          [(struct CopyInResponse (format column-formats))
           (raise-user-error 'query*
                             "COPY IN statements not allowed in this query mode")]
          [(struct CopyOutResponse (format column-formats))
           (raise-user-error 'query*
                             "COPY OUT statements not allowed in this query mode")]
          [_ (error 'query "unexpected message")]))

      ;; generate-name : -> string
      (define/private (generate-name)
        (let ([n name-counter])
          (set! name-counter (add1 name-counter))
          (format "λmz_~a_~a" process-id n)))

      ;; prepare-multiple : (list-of string) -> (list-of PreparedStatement)
      (define/public (prepare-multiple stmts)
        (for-each (lambda (stmt)
                    (unless (string? stmt)
                      (raise-type-error 'prepare* "string" stmt)))
                  stmts)
        (let* ([mg (new-exchange)]
               ;; name generation within exchange: synchronized
               [names (map (lambda (_) (generate-name)) stmts)])
          (with-final-end-exchange
            (for-each (lambda (name stmt) (prepare1:enqueue name stmt))
                      names
                      stmts)
            (send-message (make-Sync)))
          (let loop ([names names] [stmts stmts] [mg mg])
            (if (null? stmts)
                (begin (check-ready-for-query mg)
                       null)
                (let-values ([(result mg) (prepare1:collect mg (car names) (car stmts))])
                  (cons result (loop (cdr names) (cdr stmts) mg)))))))
      
      ;; prepare1:enqueue : string string -> void
      (define/private (prepare1:enqueue name stmt)
        (buffer-message (make-Parse name stmt null))
        (buffer-message (make-Describe 'statement name)))
      
      ;; prepare1:collect : stream string string -> PreparedStatement stream
      (define/private (prepare1:collect mg name stmt)
        (let-values ([(r mg) (recv 'prepare* mg)])
          (match r
            [(struct ParseComplete ())
             (prepare1:describe-params mg name stmt)]
            [else (prepare1:error mg r stmt)])))
      (define/private (prepare1:describe-params mg name stmt)
        (let-values ([(r mg) (recv 'prepare* mg)])
          (match r
            [(struct ParameterDescription (param-types))
             (prepare1:describe-result mg name stmt param-types)]
            [else (prepare1:error mg r stmt)])))
      (define/private (prepare1:describe-result mg name stmt param-types)
        (let-values ([(r mg) (recv 'prepare* mg)])
          (match r
            [(struct RowDescription (field-records))
             (prepare1:finish mg name stmt param-types (length field-records))]
            [(struct NoData ())
             (prepare1:finish mg name stmt param-types #f)]
            [else (prepare1:error mg r stmt)])))
      (define/private (prepare1:error mg r stmt)
        (error 'prepare* "unexpected message processing ~s: ~s" stmt r))
      (define/private (prepare1:finish mg name stmt param-types result-fields)
        (values 
         (make-PreparedStatement name param-types result-fields (make-weak-box this))
         mg))
      
      ;; bind-prepared-statement : PreparedStatement (list-of value) -> StatementBinding
      (define/public (bind-prepared-statement pst params)
        (unless (PreparedStatement? pst)
          (raise-type-error 'bind-prepared-statement "prepared statement" pst))
        (unless (eq? this (weak-box-value (PreparedStatement-wcx pst)))
          (raise-mismatch-error 'bind-prepared-statement
                                "prepared statement is owned by another connection"
                                pst))
        (unless (list? params)
          (raise-type-error 'bind-prepared-statement "list" params))
        (match pst
          [(struct PreparedStatement (pst-name param-types result? pst-wcx))
           (check-params params param-types)
           (let ([params
                  (map (lambda (p t)
                         (if (sql-null? p)
                             sql-null
                             (datum->external-representation t p)))
                       params 
                       param-types)])
             (make-StatementBinding pst params))]))
      
      (define/private (check-params params param-types)
        (define len (length params))
        (define tlen (length param-types))
        (when (not (= len tlen))
          (raise-user-error
           'bind-prepared-statement
           "prepared statement requires ~s parameters, given ~s" tlen len)))

      ;; datum->external-representation : number datum -> string
      (define/public (datum->external-representation typeoid datum)
        (unless (string? datum)
          (raise-user-error 'datum->external-representation
                            "cannot convert datum: ~s" datum))
        datum)
      ))

  ;; Standard recordset collectors

  (define (standard-info field-records)
    (map (lambda (fr) (make-FieldInfo (list-FieldInfo-field-name fr)))
         field-records))

  (define vectorlist-collector
    (lambda (fields)
      (values null
              (lambda (b . fields) (cons (apply vector fields) b))
              reverse
              #f)))

  (define vectorlist-collector/fieldinfo
    (lambda (fields)
      (values null
              (lambda (b . fields) (cons (apply vector fields) b))
              reverse
              (standard-info fields))))

  (define void-collector
    (lambda (fields)
      (values #f void void #f)))

  (define (mk-single-column-collector function sql)
    (lambda (fields)
      (unless (= 1 (length fields))
        (raise-mismatch-error function 
                              "query did not return exactly one column: "
                              sql))
      (values null
              (lambda (b a) (cons a b))
              reverse
              #f)))

  (define (recordset->one-row function rs sql)
    (define rows (Recordset-data rs))
    (cond [(and (pair? rows) (null? (cdr rows)))
           (car rows)]
          [else (raise-mismatch-error 
                 function
                 "query did not return exactly one row: "
                 sql)]))

  (define (recordset->maybe-row function rs sql)
    (define rows (Recordset-data rs))
    (cond [(null? rows) #f]
          [(and (pair? rows) (null? (cdr rows)))
           (car rows)]
          [else (raise-mismatch-error 
                 function
                 "query did not return zero or one rows: "
                 sql)]))

  ;; FIXME!!! 
  ;; Change to use typoutput properly; needs coordination with sql-types.ss
  (define TYPE-QUERY 
    "select oid, typinput, typoutput from pg_type where typtype = 'b' and typelem = 0")

  (define default-converter%
    (class* object% (converter<%>)
      (define/public (get-external->datum typeinput)
        (hash-table-get external=>datum typeinput #f))
      (define/public (get-datum->external typeoutput)
        (hash-table-get datum=>external typeoutput #f))
      (super-new)))
  (define default-converter (new default-converter%))
  
  ;; conversion-mixin
  ;; Adds automatic conversions from SQL external representations to Scheme data
  (define conversion-mixin
    (mixin (base<%> primitive-query<%>) ()
      (super-new)

      (init-field [converter default-converter])
      (define typeinput-mapping #f)
      (define typeoutput-mapping #f)

      ;; query*/no-conversion : symbol (list-of Statement) Collector
      ;;                     -> (list-of QueryResult)
      (define/public-final (query*/no-conversion fsym stmts collector)
        (super query* fsym stmts collector))

      ;; query* : symbol (list-of Statement) Collector -> (list-of QueryResult)
      ;; Overridden to automatically use type conversion
      (define/override (query* fsym stmts collector)
        (super query* fsym stmts (compose-with-converters collector)))

      ;; compose-with-converters : (FieldInfo -> 'a ('a field ... -> 'a) ('a -> 'b))
      ;;                        -> (FieldInfo -> 'a ('a field ... -> 'a) ('a -> 'b))
      (define/private (compose-with-converters f)
        (lambda (field-records)
          (let* ([fieldtypeoids (map list-FieldInfo-type-oid field-records)]
                 [type-functions
                  (map (lambda (oid) (get-type-converter oid)) fieldtypeoids)]
                 [convert
                  (lambda (args)
                    (map (lambda (convert arg)
                           (if (sql-null? arg) sql-null (convert arg)))
                         type-functions
                         args))])
            (let-values ([(base combine finish info) (f field-records)])
              (values base 
                      (lambda (b . args) (apply combine b (convert args)))
                      finish
                      info)))))

      ;; get-type-converter : number -> (string -> datum)
      (define/private (get-type-converter typeoid)
        (let ([typeinput (hash-table-get typeinput-mapping typeoid #f)])
          (if typeinput
              (or (send converter get-external->datum typeinput)
                  values)
              values)))

      ;; datum->external-representation : number datum -> string
      (define/override (datum->external-representation typeoid datum)
        (let ([typeoutput (hash-table-get typeoutput-mapping typeoid #f)])
          (let ([convert 
                 (and typeoutput
                      (send converter get-datum->external typeoutput))])
            (cond [convert
                   (convert datum)]
                  [(string? datum)
                   datum]
                  [else
                   (raise-user-error
                    'datum->external-representation
                    "cannot convert datum: ~s" datum)]))))

      ;; create-typeoid-mapping : -> void
      (define/private (create-typeoid-mappings)
        (let ([input-ht (make-hash-table)]
              [output-ht (make-hash-table)]
              [qrs (query*/no-conversion 'internal
                                         (list TYPE-QUERY)
                                         vectorlist-collector)])
          (for-each
           (lambda (v)
             (let ([oid (string->number (vector-ref v 0))]
                   [typeinput (string->symbol (vector-ref v 1))]
                   [typeoutput (string->symbol (vector-ref v 2))])
               (hash-table-put! input-ht oid typeinput)
               (hash-table-put! output-ht oid typeoutput)))
           (Recordset-data (car qrs)))
          (set! typeinput-mapping input-ht)
          (set! typeoutput-mapping output-ht)))

      ;; after-connect : -> void
      (define/override (after-connect)
        (super after-connect)
        (create-typeoid-mappings))))

  ;; query-mixin
  ;; Provides high-level query methods in terms of low-level ones
  (define query-mixin
    (mixin (primitive-query<%>) (query<%>)
      (inherit query*)
      (super-new)

      ;; query1 : symbol Statement Collector -> QueryResult
      (define/private (query1 fsym stmt collector)
        (car (query* fsym (list stmt) collector)))

      ;; query : Statement -> QueryResult
      ;; Uses the default 'vectorlist' collector
      (define/public-final (query sql)
        (car (query* 'query (list sql) vectorlist-collector/fieldinfo)))

      ;; query-multiple : (list-of Statement) -> (list-of QueryResult)
      (define/public-final (query-multiple stmts)
        (query* 'query-multiple stmts vectorlist-collector/fieldinfo))

      ;; fold : Statement ('a field ... -> 'a) 'a -> 'a
      (define/public-final (fold sql f base)
        (-fold 'fold sql f base))

      ;; -fold : symbol Statement ('a field ... -> 'a) 'a -> 'a
      (define/private (-fold function sql f base)
        (Recordset-data
         (query/recordset function
                          sql
                          (lambda (fields) (values base f values #f)))))

      ;; query-list : Statement -> (listof 'a)
      ;; Expects to get back a recordset with one field per row.
      (define/public-final (query-list sql)
        (Recordset-data
         (query/recordset 'query-list
                          sql (mk-single-column-collector 'query-list sql))))

      ;; query-maybe-row : Statement -> (vector-of 'a) or #f
      ;; Expects to get back a recordset of zero or one rows.
      (define/public-final (query-maybe-row sql)
        (recordset->maybe-row 
         'query-maybe-row
         (query/recordset 'query-maybe-row sql vectorlist-collector)
         sql))

      ;; query-row : Statement -> (vector-of 'a)
      ;; Expects to get back a recordset of zero or one rows.
      (define/public-final (query-row sql)
        (recordset->one-row 
         'query-row
         (query/recordset 'query-row sql vectorlist-collector)
         sql))

      ;; query-value : string -> value | raises error
      ;; Expects to get back a recordset of exactly one row, exactly one column.
      (define/public-final (query-value sql)
        (recordset->one-row
         'query-value
         (query/recordset 'query-value
                          sql (mk-single-column-collector 'query-value sql))
         sql))

      ;; query-maybe-value : Statement -> value/#f
      ;; Expects to get back a recordset of zero or one rows, exactly one column.
      (define/public-final (query-maybe-value sql)
        (recordset->maybe-row
         'query-maybe-value
         (query/recordset 
          'query-maybe-value sql
          (mk-single-column-collector 'query-maybe-value sql))
         sql))

      ;; exec : Statement ... -> void
      (define/public-final (exec . sqls)
        (query* 'exec sqls void-collector)
        (void))

      ;; mapfilter : Statement (field... -> 'a) (field... -> boolean) -> (listof 'a)
      (define/public-final (mapfilter sql f keep?)
        (unless (procedure? keep?)
          (raise-type-error 'mapfilter "procedure" keep?))
        (unless (procedure? f)
          (raise-type-error 'mapfilter "procedure" f))
        (reverse (-fold 'mapfilter
                        sql
                        (lambda (b . fields)
                          (if (apply keep? fields)
                              (cons (apply f fields) b)
                              b))
                        null)))

      ;; -map : Statement (field ... -> 'a) -> (listof 'a)
      (public (-map map))
      (define (-map sql f)
        (unless (procedure? f)
          (raise-type-error 'map "procedure" f))
        (reverse
         (-fold 'map
                sql (lambda (b . fields) (cons (apply f fields) b)) null)))

      ;; -for-each : Statement (field ... -> unspecified) -> unspecified
      (public (-for-each for-each))
      (define (-for-each sql f)
        (unless (procedure? f)
          (raise-type-error 'for-each "procedure" f))
        (-fold 'for-each sql (lambda (_ . fields) (apply f fields)) #f))

      ;; query/recordset : symbol Statement collector -> void
      (define/private (query/recordset fsym sql collector)
        (let [(result (query1 fsym sql collector))]
          (cond [(Recordset? result) result]
                [else
                 (raise-mismatch-error
                  fsym
                  "query did not return recordset: " sql)])))
      ))

  ;; prepare-query-mixin
  ;; Provides closure-producing versions of high-level query methods
  (define prepare-query-mixin
    (mixin (primitive-query<%> query<%>) (prepare-query<%>)
      (inherit exec
               query-list
               query-row
               query-maybe-row
               query-value
               query-maybe-value
               map
               for-each
               mapfilter
               fold)
      (inherit prepare-multiple
               bind-prepared-statement)
      (super-new)

      ;; prepare : string -> PreparedStatement
      (define/public-final (prepare stmt)
        (car (prepare-multiple (list stmt))))

      (define-syntax prepare-query-method
        (syntax-rules ()
          [(prepare-query-method name method)
           (prepare-query-method name method [#:check])]
          [(prepare-query-method name method [#:check check ...])
           (prepare-query-method name method [#:check check ...] [#:arg])]
          [(prepare-query-method name method [#:check check ...] [#:arg arg ...])
           (define/public (name sql arg ...)
             (let ([pst (prepare sql)])
               (check 'name pst sql) ...
               (lambda args (method (bind-prepared-statement pst args) arg ...))))]))

      (prepare-query-method prepare-exec exec)
      (prepare-query-method prepare-query-list query-list
        [#:check check-results/one-column])
      (prepare-query-method prepare-query-row query-row
        [#:check check-results])
      (prepare-query-method prepare-query-maybe-row query-maybe-row
        [#:check check-results])
      (prepare-query-method prepare-query-value query-value
        [#:check check-results/one-column])
      (prepare-query-method prepare-query-maybe-value query-maybe-value
        [#:check check-results/one-column])

      (prepare-query-method prepare-map map
        [#:check check-results]
        [#:arg proc])
      (prepare-query-method prepare-for-each for-each
        [#:check check-results]
        [#:arg proc])
      (prepare-query-method prepare-mapfilter mapfilter
        [#:check check-results]
        [#:arg map-proc filter-proc])
      (prepare-query-method prepare-fold fold
        [#:check check-results]
        [#:arg combine base])
      ))

  (define (check-results name pst stmt)
    (unless (PreparedStatement-results pst)
      (raise-user-error name "query does not return records")))
  (define (check-results/one-column name pst stmt)
    (check-results name pst stmt)
    (unless (equal? (PreparedStatement-results pst) 1)
      (raise-user-error name
                        "query does not return a single column (returns ~a columns)"
                        (PreparedStatement-results pst))))

  ;; pure-connection%
  (define pure-connection% 
    (class (prepare-query-mixin
            (query-mixin
             (conversion-mixin
              (primitive-query-mixin
               (connector-mixin
                base%)))))
      (super-new)))

  ;; connection%
  (define connection%
    (class (ssl-connector-mixin pure-connection%)
      (super-new)))
  )
