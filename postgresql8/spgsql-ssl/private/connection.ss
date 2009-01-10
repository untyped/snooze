;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

;; Implementation of connections, which communicate with a backend through
;; structured messages.

(module connection mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "class.ss")
           "protocol-structures.ss"
           "protocol2.ss"
           "connection-structures.ss"
           "exceptions.ss"
           "sql-types.ss")
  
  (provide connection<%>
           connection%)
  
  (define connection<%>
    (interface ()
      disconnect
      disconnected?
      set-notification-handler
      set-notice-handler
      use-type-conversions
      
      fold
      fold-right
      query-list
      query-tuple
      query-value
      map
      for-each
      mapfilter
      exec
      ))
  
  
  (define DEBUG-RESPONSES #f)
  (define DEBUG-SENT-MESSAGES #f)
  
  ;; Connection helpers
  (define (query:next-f Fs f0 fields)
    (let-values [((f Fs)
                  (cond [(null? Fs) (values f0 Fs)]
                        [(pair? Fs) (values (car Fs) (cdr Fs))]))]
      (query:check-f f)
      (let-values [((base combine finish) (f fields))]
        (query:check-combine/finish combine finish (length fields))
        (values base combine finish Fs f0))))
  (define (query:check-f f)
    (unless (procedure-arity-includes? f 1)
      (raise-sp-user-error 
       'arity "arity of (field-info -> base combine finish) must be 1")))
  (define (query:check-combine/finish combine finish field-length)
    (unless (procedure? combine)
      (raise-sp-user-error 
       'expected-procedure
       "procedure expected for fold combine argument: got ~s" combine))
    (unless (procedure-arity-includes? combine (add1 field-length))
      (raise-sp-user-error
       'arity
       "fold combine argument must have arity one greater than fields/row"))
    (unless (procedure? finish)
      (raise-sp-user-error
       'expected-procedure 
       "procedure expected for fold finish argument: got ~s" finish))
    (unless (procedure-arity-includes? finish 1)
      (raise-sp-user-error 'arity "fold finish argument must have arity 1")))
  
  ;; create-fieldinfo : RowDescription -> (listof 'A)
  (define (create-fieldinfo rd)
    (if rd
        (let [(fields (RowDescription-fields rd))]
          (map (lambda (ri)
                 (list (FieldInfo-name ri) 
                       (FieldInfo-oid ri)))
               fields))
        null))
  
  ;; connection-base%
  ;; Handles the mechanics of connection creations, queries, etc.
  ;; Provides functionality, not usability. See connection% for friendly 
  ;; interface.
  (define connection-base%
    (class object%
      (super-instantiate ())
      (public query-fold)
      (public-final connect
                    disconnect
                    disconnected?)
      
      ;; protocol : protocol
      (define protocol #f)
      
      ;; Information needed for CancelRequest (currently unimplemented)
      (define server-process-id #f)
      (define server-secret-key #f)
      
      ;; fatal
      ;; Disconnects before raising an exception.
      (define-syntax fatal
        (syntax-rules ()
          [(_ raise-expression)
           (begin (disconnect)
                  raise-expression)]))
      
      ;; lock! : symbol -> integer
      (define (lock! sym) (protocol2:lock protocol sym))
      
      ;; lock/key! : symbol -> integer
      ;; Unique state lock (eg, a particular COPY or authentication callback)
      (define (lock/key! sym) (protocol2:lock/key protocol sym))
      
      ;; check-lock! : symbol [number] -> void or error
      (define (check-lock! . args) 
        (apply protocol2:unlock protocol args))
      
      ; [DJG] changed
      ;; connect
      ;;     : string
      ;;       integer
      ;;       string
      ;;       string
      ;;       (U 'yes 'no 'optional)
      ;;       (U 'sslv2-or-v3 'sslv2 'sslv3 'tls)
      ;;    -> ConnectionResult
      (define (connect server port dbname username ssl ssl-encrypt)
        ; [DJG] /changed
        (check-lock! protocol:lock:disconnected)
        (set! protocol #f)
        (with-handlers 
            [(exn:fail:network?
              (lambda (e) 
                (fatal (raise-communication-error 
                        'connect 
                        "Could not connect to server ~a:~a: ~a"
                        server port (exn-message e)))))]
          ; [DJG] changed
          (let-values [((in out) (protocol2:negotiate-ssl server port ssl ssl-encrypt))]
            ; [DJG] /changed
            (set! protocol (protocol2:new in out))))
        (let [(mg (protocol2:reset protocol))]
          (send-message (make-StartupPacket '(2 . 0) dbname username "" "" ""))
          (connect:expect-auth mg)))
      
      (define (connect:expect-auth mg)
        (let-values [((r mg) (get-response mg))]
          (cond 
            [(FatalErrorResponse? r)
             (fatal (raise-auth-error 'authentication-error
                                      "Authentication failed. Backend says ~s"
                                      (MessageResponse-message r)))]
            [(not (Authentication? r))
             (fatal (raise-internal-error 
                     'connect:expect-auth
                     "Expected Authentication message from backend, got ~s" r))]
            [(symbol=? (Authentication-method r) 'ok)
             (connect:expect-backend-key-data mg)]
            [(symbol=? (Authentication-method r) 
                       'unencrypted-password)
             (let [(locknum (lock/key! protocol:lock:auth-required))]
               (make-UnencryptedPasswordResult
                (lambda (n)
                  (check-lock! protocol:lock:auth-required locknum)
                  (send-message (make-PasswordPacket n))
                  (connect:expect-auth mg))))]
            [(symbol=? (Authentication-method r) 
                       'encrypted-password)
             (let [(locknum (lock/key! protocol:lock:auth-required))]
               (make-EncryptedPasswordResult
                (AuthenticationEncryptedPassword-salt r)
                (lambda (n)
                  (check-lock! protocol:lock:auth-required locknum)
                  (send-message (make-PasswordPacket n))
                  (connect:expect-auth mg))))]
            [(symbol=? (Authentication-method r)
                       'md5-password)
             (let [(locknum (lock/key! protocol:lock:auth-required))]
               (make-MD5PasswordResult
                (AuthenticationMD5Password-salt r)
                (lambda (n)
                  (check-lock! protocol:lock:auth-required locknum)
                  (send-message (make-PasswordPacket n))
                  (connect:expect-auth mg))))]
            [(or (symbol=? (Authentication-method r) 'kerberos4)
                 (symbol=? (Authentication-method r) 'kerberos5)
                 (symbol=? (Authentication-method r) 'scm))
             (fatal (raise-auth-error 'authentication-unsupported 
                                      "Authentication method ~s known but not supported"
                                      (Authentication-method r)))]
            [else 
             (fatal (raise-auth-error 'authentication-unknown
                                      "Authentication method ~s unknown"
                                      (Authentication-method r)))])))
      
      (define (connect:expect-backend-key-data mg)
        (let-values [((r mg) (get-response mg))]
          (cond
            [(BackendKeyData? r)
             (set! server-process-id (BackendKeyData-process-id r))
             (set! server-secret-key (BackendKeyData-secret-key r))
             (connect:expect-ready-for-query mg)]
            [(FatalErrorResponse? r)
             (fatal (raise-internal-error 'connect:expect-backend-key-data
                                          "Error after authentication: ~s"
                                          (MessageResponse-message r)))]
            [else
             (fatal 
              (raise-internal-error 
               'connect:expect-backend-key-data
               "Authentication successful, but backend information missing: got ~s" r))])))
      
      (define (connect:expect-ready-for-query mg)
        (let-values [((r mg) (get-response mg))]
          (cond
            [(ReadyForQuery? r)
             (lock! protocol:lock:ready)
             ; [DJG] added
             ; This is a bit of a hack: exec is defined in the subclass connection%. The call
             ; works with the code as it stands, but it won't if someone subclasses 
             ; connection-base% without implementing this method.
             (send this exec "SET CLIENT_ENCODING TO 'UTF8';")
             ; [DJG] /added
             (make-OkConnection)]
            [else 
             (fatal (raise-internal-error 
                     'connect:expect-ready-for-query
                     "Connection complete, but backend not ready for query: got ~s" r))])))
      
      ;; query-fold : string list-of-F F -> (listof QueryResult)
      ;; Where F is list-of-FieldInfo -> (values b (b a1 a2 ... -> b) (b -> c)
      ;; and combines the result of the previous rows (starting with the first 
      ;; value when no rows have been received yet) with the current fields in 
      ;; the row.
      (define (query-fold sql Fs f0)
        (check-lock! protocol:lock:ready)
        (unless (string? sql)
          (raise-sp-user-error 'expected-sql-string
                               "expected string for query SQL: got ~s" sql))
        (let [(mg (protocol2:reset protocol))]
          (send-message (make-Query sql))
          (query:init mg null Fs f0)))
      
      (define (query:init mg rsets Fs f0)
        (let-values [((r mg) (get-response mg))]
          (cond
            [(CopyInResponse? r)
             (let [(lock (lock/key! protocol:lock:copy-in))]
               (lambda (copy-list)
                 (check-lock! protocol:lock:copy-in lock)
                 (lock! protocol:lock:ready)
                 (send-message (make-CopyDataRows copy-list))
                 (query:copy-in mg rsets Fs f0)))]
            [(CopyOutResponse? r)
             (query:copy-out mg rsets Fs f0 (CopyOutResponse-rows r))]
            [(CursorResponse? r) 
             (query:cursor mg
                           rsets 
                           Fs
                           f0
                           (CursorResponse-name r))]
            [(ReadyForQuery? r) (query:return rsets)]
            [(CompletedResponse? r)
             (query:init mg
                         (cons (make-SimpleQueryResult 
                                (CompletedResponse-command r)) 
                               rsets) 
                         Fs f0)]
            [(EmptyQueryResponse? r)
             (query:init mg rsets Fs f0)]
            [else (query:error-recovery mg rsets Fs f0 r)])))
      
      (define (query:copy-in mg rsets Fs f0)
        (let-values [((r mg) (get-response mg))]
          (cond
            [(CompletedResponse? r)
             (query:init mg
                         (cons (make-SimpleQueryResult
                                (CompletedResponse-command r))
                               rsets)
                         Fs f0)]
            [else (query:error-recovery mg rsets Fs f0 r)])))
      (define (query:copy-out mg rsets Fs f0 rows)
        (let-values [((r mg) (get-response mg))]
          (cond
            [(CompletedResponse? r)
             (query:init mg 
                         (cons (make-Copyset (CompletedResponse-command r)
                                             (reverse rows))
                               rsets)
                         Fs f0)]
            [else (query:error-recovery mg rsets Fs f0 r)])))
      (define (query:cursor mg rsets Fs f0 cursor-name)
        (let-values [((r mg) (get-response mg))]
          (cond
            [(CompletedResponse? r)
             (query:init mg
                         (cons 
                          (make-CursorResult (CompletedResponse-command r)
                                             cursor-name)
                          rsets)
                         Fs f0)]
            [(RowDescription? r)
             (let-values [((base combine finish Fs f0)
                           (query:next-f Fs f0 (RowDescription-fields r)))]
               (query:cursor-recordset mg
                                       rsets Fs f0 cursor-name #f (create-fieldinfo r)
                                       base
                                       combine
                                       finish))]
            [else (query:error-recovery mg rsets Fs f0 r)])))
      (define (query:cursor-recordset mg rsets Fs f0 cursor-name binary? 
                                      field-info accum combine finish)
        (let-values [((r mg) (get-response mg))]
          (cond
            [(CompletedResponse? r)
             (query:init mg
                         (cons
                          (make-Recordset (CompletedResponse-command r)
                                          cursor-name
                                          binary? 
                                          field-info
                                          (finish accum))
                          rsets)
                         Fs f0)]
            [(AsciiRow? r)
             (query:cursor-recordset mg rsets Fs f0 cursor-name binary? field-info
                                     (apply combine accum (AsciiRow-fields r))
                                     combine finish)]
            [(BinaryRow? r)
             (query:cursor-recordset mg rsets Fs f0 cursor-name #t field-info
                                     (apply combine accum (BinaryRow-fields r))
                                     combine finish)]
            [else (query:error-recovery mg rsets Fs f0 r)])))
      (define (query:error-recovery mg rsets Fs f0 msg)
        (cond
          [(FatalErrorResponse? msg)
           (fatal (raise-internal-error 'query:error-recovery
                                        "Fatal error reported by backend: ~s"
                                        (MessageResponse-message msg)))]
          [(ErrorResponse? msg)
           ;(printf "Handling non-fatal error, adding to results: ~v~n" msg)
           (query:init mg
                       (cons (make-ErrorResult (MessageResponse-type msg)
                                               (MessageResponse-message msg)
                                               (ErrorResponse-code msg))
                             rsets)
                       Fs f0)]
          [(NotificationResponse? msg)
           ;(printf "Handling notification, adding to results: ~v~n" msg)
           (query:init mg
                       (cons (make-NotificationResult 
                              (NotificationResponse-condition msg)
                              (NotificationResponse-process-id msg))
                             rsets)
                       Fs f0)]
          [(NoticeResponse? msg)
           ;(printf "Handling notice, adding to results: ~v~n" msg)
           (query:init mg 
                       (cons (make-NoticeResult (MessageResponse-type msg)
                                                (MessageResponse-message msg))
                             rsets)
                       Fs f0)]
          [else
           (fatal (raise-internal-error 
                   'query:error-recovery
                   "Error receiving messages from backend: got ~s" msg))]))
      (define (query:return rsets)
        (reverse rsets))
      
      
      ;; disconnect : -> (void)
      (define (disconnect)
        (when protocol
          ; [DJG] added
          (send-message (make-Terminate))
          ; [DJG] /added
          (protocol2:close protocol))
        (set! protocol #f))
      
      ;; get-response : -> msg
      (define (get-response mg)
        (let-values [((current next) (message-generator:current/next mg))]
          (when DEBUG-RESPONSES
            (fprintf (current-error-port) "  << ~s~n" current))
          (values current next)))
      
      ;; send-message : msg -> (void)
      (define (send-message msg)
        (when DEBUG-SENT-MESSAGES
          (fprintf (current-error-port) "  >> ~s~n" msg))
        (protocol2:encode protocol msg))
      
      ;; disconnected? : -> boolean
      (define (disconnected?)
        (not (and protocol #t)))
      ))
  
  ;; Standard recordset collectors
  ;; Used by query-fold
  (define vectorlist-collector
    (lambda (fields)
      (values null
              (lambda (b . fields) (cons (apply vector fields) b))
              reverse)))
  (define void-collector
    (lambda (fields)
      (values #f void void)))
  
  ;; connection%
  ;; Class intended as a programming interface between application and database.
  (define connection%
    (class* connection-base% (connection<%>)
      (super-instantiate ())
      
      (public-final (-map map)
                    (-for-each for-each))
      
      (define typeoids #f)
      
      ;; fatal
      ;; Disconnects before raising an exception.
      (define-syntax fatal
        (syntax-rules ()
          [(_ raise-expression)
           (begin (super disconnect)
                  raise-expression)]))
      
      (define handle-notification void)
      (define handle-notice 
        (lambda (n)
          (fprintf (current-error-port)
                   "~a: ~a~n"
                   (NoticeResult-type n)
                   (NoticeResult-message n))))
      
      (define/public-final (set-notification-handler h)
        (set! handle-notification h))
      (define/public-final (set-notice-handler h)
        (set! handle-notice h))
      
      ;; query-fold : ...
      ;; Overridden to automatically use type conversion when able
      (define/override (query-fold sql Fs f0)
        (super query-fold 
               sql
               (map (lambda (f) (compose-with-type-function f)) Fs)
               (compose-with-type-function f0)))
      
      ;; query-general : string -> proc | 
      ;;        (listof QueryResult,ErrorResult,NoticeResult,NotificationResult)
      ;; query-general raises an error only if a Fatal Error is sent by the 
      ;; backend, or if there is an internal error in the connection object
      ;; query-general passes all types of message through to caller, including
      ;; Notifications, Notices, and Copy callbacks
      (define/public-final (query-general sql)
        (query-fold sql null vectorlist-collector))
      
      ;; query/versatile : string collector boolean (list-of-QueryResult -> 'a)
      ;;                                 -> 'a | ErrorResult | raises error
      (define/private (query/versatile sql collector)
        (let* [(r (query-fold sql (list collector) void-collector))
               (sr (and (list? r) (filter QueryResult? r)))
               (nr (and (list? r) (filter NoticeResult? r)))
               (nfr (and (list? r) (filter NotificationResult? r)))
               (er (and (list? r) (filter ErrorResult? r)))]
          (when nfr (for-each handle-notification nfr))
          (when nr (for-each handle-notice nr))
          (values r sr er)))
      
      ;; query/versatile-nocopy : string collector boolean (listof Query -> 'a)
      ;;                        -> 'a | ErrorResult | raises error
      (define/private (query/versatile-nocopy sql collector raise-on-error? results-f)
        (let-values [((r sr er) (query/versatile sql collector))]
          (cond [(procedure? r)
                 ;; NOTE: when a COPY IN procedure is
                 ;; returned, we give it nothing, and
                 ;; continue processing, and then we signal
                 ;; an error.
                 (r null)
                 (raise-sp-user-error
                  'copy-in
                  "COPY IN not allowed with this query method")]
                [(and raise-on-error? (pair? er))
                 (let ([error (car er)])
                   (if (constraint-error? error)
                       (raise-constraint-error (ErrorResult-message error))
                       (raise-query-error (ErrorResult-message error))))]
                [(and (pair? er) (not raise-on-error?))
                 (car er)]
                [else (results-f sr)])))
      
      (define/private (query/single-result sql collector raise-on-error?)
        (query/versatile-nocopy 
         sql collector raise-on-error?
         (lambda (results)
           (cond [(and (pair? results) (null? (cdr results)))
                  (car results)]
                 [(or (pair? results) (null? results))
                  (raise-sp-user-error 'expected-single-result
                                       "single result expected; got ~s"
                                       results)]))))
      (define/private (query/single-recordset sql collector raise-on-error?)
        (let [(result (query/single-result sql collector raise-on-error?))]
          (cond [(Recordset? result)
                 result]
                [else
                 (raise-sp-user-error 'expected-single-recordset
                                      "single recordset expected: got ~s" result)])))
      
      ;; query : string -> QueryResult | ErrorResult | raises error
      ;; Query expects to get back a single QueryResult (and zero or more 
      ;; Notifications and Notices, which are handled and not returned).
      ;; Query raises an error only on a Fatal Error sent by the server, or a 
      ;; Copy In request, multiple returned results, or an internal error.
      ;; If the backend sends any nonfatal error messages, this method returns
      ;; one of them.
      (define/public-final (query sql)
        (query/single-result sql vectorlist-collector #f))
      
      ;; fold : string ('a field ... -> 'a) 'a -> 'a
      (define/public-final (fold sql f base)
        (Recordset-rows (query/single-recordset 
                         sql
                         (lambda (fields)
                           (values base f (lambda (x) x)))
                         #t)))
      
      ;; fold-right : string 'a ('a field ... -> 'a) -> 'a
      ;; Uses two left-folds to make a right-fold
      (define/public-final (fold-right sql f base)
        (foldl (lambda (row b) (apply f b row))
               base
               (Recordset-rows (query/single-recordset
                                sql
                                (lambda (fields)
                                  (values null 
                                          (lambda (b . fields) (cons fields b)) 
                                          (lambda (x) x)))
                                #t))))
      
      ;; query-list : string -> (listof value) | raises error
      ;; Query-list expects to get back a single QueryResult with exactly 
      ;; one field per row.
      (define/public-final (query-list sql)
        (let [(r (query/single-recordset sql vectorlist-collector #t))]
          (cond [(= 1 (length (Recordset-fields r)))
                 (map (lambda (row) (vector-ref row 0)) (Recordset-rows r))]
                [else 
                 (raise-sp-user-error 'expected-single-field
                                      "single field per row expected: got ~s"
                                      (length (Recordset-fields r)))])))
      
      ;; query-tuple : string -> (tuple-of value) | raises error
      ;; Query-tuple expects a single QueryResult with exactly one row.
      (define/public-final (query-tuple sql)
        (let [(r (query/single-recordset sql vectorlist-collector #t))]
          (cond [(= 1 (length (Recordset-rows r)))
                 (car (Recordset-rows r))]
                [else 
                 (raise-sp-user-error 'expected-single-row
                                      "single row expected: got ~s"
                                      (length (Recordset-rows r)))])))
      
      ;; query-value : string -> value | raises error
      ;; Query-value expects a single QueryResult with exactly one row 
      ;; having exactly one field.
      (define/public-final (query-value sql)
        (let [(r (query/single-recordset sql vectorlist-collector #t))]
          (cond [(= 1 (length (Recordset-rows r)) (length (Recordset-fields r)))
                 (vector-ref (car (Recordset-rows r)) 0)]
                [(not (= 1 (length (Recordset-rows r))))
                 (raise-sp-user-error 'expected-single-row
                                      "single row expected: got ~s"
                                      (length (Recordset-rows r)))]
                [else
                 (raise-sp-user-error 'expected-single-field
                                      "single field per row expected: got ~s"
                                      (length (Recordset-fields r)))])))
      
      ;; exec : string -> #t | raises error
      ;; exec raises a fatal error on a Fatal Error from backend, internal error,
      ;; or COPY IN statement; exec raises non-fatal error on non-fatal error from
      ;; backend: only the first error is raised
      (define/public-final (exec sql)
        (query/versatile-nocopy sql void-collector #t (lambda _ #t)))
      
      ;; mapfilter : string (field... -> 'a) (field... -> boolean) -> (listof 'a)
      (define/public-final (mapfilter sql f keep?)
        (unless (procedure? keep?)
          (raise-sp-user-error 'expected-procedure
                               "expected procedure for keep? argument to mapfilter"))
        (unless (procedure? f)
          (raise-sp-user-error 'expected-procedure
                               "expected procedure for f argument to mapfilter"))
        (reverse (fold sql
                       (lambda (b . fields)
                         (if (apply keep? fields)
                             (cons (apply f fields) b)
                             b))
                       null)))
      
      ;; -map : string (field ... -> 'a) -> (listof 'a)
      (define (-map sql f)
        (unless (procedure? f)
          (raise-sp-user-error 'expected-procedure
                               "expected procedure for f argument to map"))
        (reverse (fold sql (lambda (b . fields) (cons (apply f fields) b)) null)))
      
      ;; -for-each : string (field ... -> unspecified) -> unspecified
      (define (-for-each sql f)
        (unless (procedure? f)
          (raise-sp-user-error 'expected-procedure
                               "expected procedure for f argument to for-each"))
        (fold sql (lambda (_ . fields) (apply f fields)) #f))
      
      ;; compose-with-type-function : (FieldInfo -> 'a ('a field ... -> 'a) ('a -> 'b)) -> same
      (define/private (compose-with-type-function f)
        (query:check-f f)
        (lambda (fields)
          (let* [(fieldtypeoids (map FieldInfo-oid fields))
                 (fieldcount (length fields))
                 (type-info (map (lambda (t) (sql-type-info t)) fieldtypeoids))
                 (type-functions (map cadr type-info))
                 (type-map (lambda (args)
                             (let loop [(args args) (type-functions type-functions)]
                               (cond [(null? args) null]
                                     [(pair? args)
                                      (cons 
                                       (let [(val (car args))]
                                         (if (sql-null? val)
                                             val
                                             ((car type-functions) val)))
                                       (loop (cdr args) (cdr type-functions)))]))))]
            (let-values [((base combine finish) (f fields))]
              (query:check-combine/finish combine finish (length fields))
              (values base 
                      (lambda (b . args) (apply combine b (type-map args)))
                      finish)))))
      
      (define/private (sql-type-info typeoid)
        (if typeoids
            (let [(typename (hash-table-get typeoids typeoid (lambda _ #f)))]
              (if typename
                  (list typename
                        (hash-table-get sql-parsers typename 
                                        (lambda () identity)))
                  (list typeoid identity)))
            (list typeoid identity)))
      
      (define/public-final use-type-conversions
	(case-lambda
          (()
           (and typeoids #t))
          ((use?)
           (if use?
               (begin (ensure-typeoids-available)
                      (set! typeoids typeoids-cache))
               (set! typeoids #f)))))
      
      (define typeoids-cache #f)
      (define/private (ensure-typeoids-available)
        (when (not typeoids-cache)
          (let [(ht (make-hash-table))]
            (-for-each 
             "select typinput, oid from pg_type"               
             (lambda (typeinput oid)
               (let [(typeinput (string->symbol
                                 (bytes->string/utf-8 typeinput)))
                     (oid (string->number (bytes->string/utf-8 oid)))]
                 (when (hash-table-get sql-parsers typeinput (lambda _ #f))
                   (hash-table-put! ht oid typeinput)))))
            (set! typeoids-cache ht))))
      ))
  )
 
