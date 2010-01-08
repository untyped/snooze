;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module p3-msg mzscheme
  (require (lib "port.ss")
           (lib "list.ss")
           "p3-msg-util.ss"
           "io.ss"
           "sql-data.ss")
  (provide (all-defined))
  
  ;; The strange structures
  
  (define-msg AuthenticationOk
    (header #\R)
    (literal #:int32 0))
  (define-msg AuthenticationKerberosV5
    (header #\R)
    (literal #:int32 2))
  (define-msg AuthenticationCleartextPassword
    (header #\R)
    (literal #:int32 3))
  (define-msg AuthenticationCryptPassword
    (header #\R)
    (literal #:int32 4)
    (field #:bytes2 salt))
  (define-msg AuthenticationMD5Password
    (header #\R)
    (literal #:int32 5)
    (field #:bytes4 salt))
  (define-msg AuthenticationSCMCredential
    (header #\R)
    (literal #:int32 6))

  (define (parse:Authentication p)
    (let* ([len (io:read-int32 p)]
           [tag (io:read-int32 p)])
      (case tag
        ((0) (make-AuthenticationOk))
        ((2) (make-AuthenticationKerberosV5))
        ((3) (make-AuthenticationCleartextPassword))
        ((4) (let ([salt (io:read-bytes-as-bytes p 2)])
               (make-AuthenticationCleartextPassword salt)))
        ((5) (let ([salt (io:read-bytes-as-bytes p 4)])
               (make-AuthenticationMD5Password salt)))
        ((6) (make-AuthenticationSCMCredential))
        (else (error 'parse:Authentication
                     "unknown authentication method requested (~s)" tag)))))
  
  (define (parse:StartupMessage p)
    (let* ([len (io:read-int32 p)]
           [p (make-limited-input-port len p)]
           [version (io:read-int32 p)])
      (make-StartupMessage
       (let loop ([fields null])
         (let ([next (peek-byte p)])
           (cond [(zero? next)
                  (begin (read-byte p) null)]
                 [else
                  (let* ([tag (io:read-null-terminated-string p)]
                         [value (io:read-null-terminated-string p)])
                    (cons (cons tag value) (loop)))]))))))
  (define write:StartupMessage
    (case-lambda
      [(v) (write:StartupMessage (current-output-port) v)]
      [(p v) (let ([out (open-output-bytes)])
               (io:write-int32 out 196608)
               (for-each (lambda (param)
                           (io:write-null-terminated-string out (car param))
                           (io:write-null-terminated-string out (cdr param)))
                         (StartupMessage-parameters v))
               (io:write-byte out 0)
               (let ([bs (get-output-bytes out)])
                 (io:write-int32 p (+ 4 (bytes-length bs)))
                 (io:write-bytes p bs)))]))
  (define-msg-struct StartupMessage (parameters))
  
  (define-msg SSLRequest
    (literal #:int32 8)
    (literal #:int32 80877103))
  
  (define-msg CancelRequest
    (literal #:int32 16)
    (literal #:int32 80877102)
    (field #:int32 process-id)
    (field #:int32 secret-key))
  
  (define (write:ErrorResponse . _)
    (error 'write:ErrorResponse "never called"))
  (define (parse:ErrorResponse p)
    (let* ([len (io:read-int32 p)]
           [in (make-limited-input-port p (- len 4))]
           [fields (parse-field-list in)])
      (make-ErrorResponse fields)))
  (define-msg-struct ErrorResponse (properties) #\E)
  ;; where properties is (list-of (cons symbol string))
  
  (define (write:NoticeResponse . _)
    (error 'write:NoticeResponse "never called"))
  (define (parse:NoticeResponse p)
    (let* ([len (io:read-int32 p)]
           [in (make-limited-input-port p (- len 4))]
           [fields (parse-field-list in)])
      (make-NoticeResponse fields)))
  (define-msg-struct NoticeResponse (properties) #\N)
  ;; where properties is (list-of (cons symbol string))
  
  (define (parse-field-list p)
    (let loop ()
      (let ([next (peek-byte p)])
        (cond [(zero? next)
               (begin (read-byte p) null)]
              [else
               (let* ([tag (integer->char (io:read-byte p))]
                      [value (io:read-null-terminated-string p)])
                 (cons (cons (char->message-tag tag) value)
                       (loop)))]))))
  
  ;; Begin message struct definitions
  ;; --------------------------------
  
  (define-msg BackendKeyData (header #\K)
    (field #:int32 process-id)
    (field #:int32 secret-key))
  
  (define-msg Bind (header #\B)
    (field #:string portal)
    (field #:string statement)
    (sequence param-formats (#:int16 param-format))
    (sequence values
              (#:length+string value
                              string/f->string/sql-null
                              string/sql-null->string/f))
    (sequence result-formats (#:int16 result-format)))
  
  (define-msg BindComplete (header #\2))
  
  (define-msg Close (header #\C)
    (field #:byte/char type 
           char->statement/portal
           statement/portal->char)
    (field #:string name))
  
  (define-msg CloseComplete (header #\3))
  
  (define-msg CommandComplete (header #\C)
    (field #:string command))
  
  (define-msg CopyData (header #\d)
    (field #:length+bytes data))
  
  (define-msg CopyDone (header #\c))
  
  (define-msg CopyFail (header #\f)
    (field #:string cause))
  
  (define-msg CopyInResponse (header #\G)
    (field #:byte format)
    (sequence column-formats
              (#:int16 column-format)))
  
  (define-msg CopyOutResponse (header #\H)
    (field #:byte format)
    (sequence column-formats
              (#:int16 column-format)))
  
  (define-msg DataRow (header #\D)
    (sequence values
              (#:length+string value
                              string/f->string/sql-null
                              string/sql-null->string/f)))
  
  (define-msg Describe (header #\D)
    (field #:byte/char type 
           char->statement/portal 
           statement/portal->char)
    (field #:string name))
  
  (define-msg EmptyQueryResponse (header #\I))
  
  (define-msg Execute (header #\E)
    (field #:string portal)
    (field #:int32 row-limit))
  
  (define-msg Flush (header #\H))
  
  (define-msg NoData (header #\n))
  
  (define-msg NotificationResponse (header #\A)
    (field #:int32 process-id)
    (field #:string condition)
    (field #:string info))
  
  (define-msg ParameterDescription (header #\t)
    (sequence type-oids
              (#:int32 type-oid)))
  
  (define-msg ParameterStatus (header #\S)
    (field #:string name)
    (field #:string value))
  
  (define-msg Parse (header #\P)
    (field #:string name)
    (field #:string query)
    (sequence type-oids
              (#:int32 type-oid)))
  
  (define-msg ParseComplete (header #\1))
  
  (define-msg PasswordMessage (header #\p)
    (field #:string password))
  
  (define-msg PortalSuspended (header #\s))
  
  (define-msg Query (header #\Q)
    (field #:string query))
  
  (define-msg ReadyForQuery (header #\Z)
    (field #:byte/char transaction-status 
           char->transaction-status
           transaction-status->char))
  
  (define-msg RowDescription (header #\T)
    (sequence rows
              (#:string name)
              (#:int32 table-oid)
              (#:int16 column-attid)
              (#:int32 type-oid)
              (#:int16 type-size)
              (#:int32 type-mod)
              (#:int16 format-code)))
  
  (define-msg Sync (header #\S))
  (define-msg Terminate (header #\X))
  
  
  ;; -----------
  ;;   HELPERS
  ;; -----------

  (define (string/f->string/sql-null b)
    (if b b sql-null))

  (define (string/sql-null->string/f b)
    (if (sql-null? b) #f b))
  
  (define (char->message-tag c)
    (case c
      [(#\S) 'severity]
      [(#\C) 'code]
      [(#\M) 'message]
      [(#\D) 'detail]
      [(#\H) 'hint]
      [(#\P) 'position]
      [(#\p) 'internal-position]
      [(#\q) 'internal-query]
      [(#\W) 'where]
      [(#\F) 'file]
      [(#\L) 'line]
      [(#\R) 'routine]))
  
  (define (char->statement/portal c)
    (case c
      [(#\S) 'statement]
      [(#\P) 'portal]))
  (define (statement/portal->char sp)
    (case sp
      [(statement) #\S]
      [(portal) #\P]))
  
  (define (char->transaction-status c)
    (case c
      [(#\I) 'idle]
      [(#\T) 'transaction]
      [(#\E) 'failed]))
  (define (transaction-status->char ts)
    (case ts
      [(idle) #\I]
      [(transaction) #\T]
      [(failed) #\E]))
  
  (define (string->command s)
    (cond [(regexp-match #rx"^SELECT *$" s)
           => (lambda (m) (list 'select))]
          [(regexp-match #rx"^INSERT ([0-9]*) ([0-9]*) *$" s)
           => (lambda (m)
                (list 'insert 
                      (string->number (cadr m))
                      (string->number (caddr m))))]
          [(regexp-match #rx"^DELETE ([0-9]* *$)" s)
           => (lambda (m)
                (list 'delete (string->number (cadr m))))]
          [(regexp-match #rx"^UPDATE ([0-9]*) *$" s)
           => (lambda (m)
                (list 'update (string->number (cadr m))))]
          [(regexp-match #rx"^MOVE ([0-9]*) *$" s)
           => (lambda (m)
                (list 'move (string->number (cadr m))))]
          [(regexp-match #rx"^FETCH ([0-9]*) *$" s)
           => (lambda (m)
                (list 'fetch (string->number (cadr m))))]
          [(regexp-match #rx"^(CREATE|ALTER|DROP) ([A-Z]*) *$" s)
           => (lambda (m)
                (list (string->symbol (string-downcase (cadr m)))
                      (string->symbol (string-downcase (caddr m)))))]
          [else s]))
  
  (define (command->string s)
    (if (list? s)
        (apply string-append
               (case (car s)
                 [(insert) "INSERT"]
                 [(delete) "DELETE"]
                 [(update) "UPDATE"]
                 [(move) "MOVE"]
                 [(fetch) "FETCH"]
                 [else s])
               (map (lambda (n) (format " ~a" n))
                    (cdr s)))
        s))
  
  (define (write-message msg port)
    ((message-writer-value msg) port msg))
  
  ;; parse-server-message : input-port -> msg/eof
  (define-parser parse-server-message
    [(#\R parse:Authentication)
     BackendKeyData
     BindComplete
     CloseComplete
     CommandComplete
     CopyData
     CopyDone
     CopyInResponse
     CopyOutResponse
     DataRow
     EmptyQueryResponse
     (#\E parse:ErrorResponse)
     ;; FunctionCallResponse
     NoData
     (#\N parse:NoticeResponse)
     NotificationResponse
     ParameterDescription
     ParameterStatus
     ParseComplete
     PortalSuspended
     ReadyForQuery
     RowDescription])
  
  )
