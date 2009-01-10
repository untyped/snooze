;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

;; Implements the frontend/backend interface specified in PostgreSQL 
;; documentation.  Defines many structure types to encode messages 
;; from the Postgres backend, and provides methods to write messages
;; back to the backend.

(module protocol2 mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "mzssl.ss" "openssl")
           "bitbang.ss"
           "protocol-structures.ss"
           "exceptions.ss"
           "sql-types.ss")
  
  (provide protocol2:negotiate-ssl
           protocol2:new
           protocol2:reset
           protocol2:close
           protocol2:lock
           protocol2:lock/key
           protocol2:unlock
           protocol2:encode
           
           message-generator:current
           message-generator:next
           message-generator:current/next
           message-generator:done?
           
           protocol:lock:disconnected
           protocol:lock:auth-required
           protocol:lock:copy-in
           protocol:lock:ready

           constraint-error?)
  
  ;; lock-counter : number
  (define lock-counter 0)
  
  (define protocol:lock:disconnected 'disconnected)
  (define protocol:lock:auth-required 'auth-required)
  (define protocol:lock:copy-in 'copy-in)
  (define protocol:lock:ready 'ready)
  
  (define-struct protocol2 
    (inport outport last-field-count message-generator status key))
  (define-struct message-generator (protocol promise done?))
  
  (define (end-of-exchange-message? msg)
    (or (ReadyForQuery? msg)
        (FatalErrorResponse? msg)))
  
  ; [DJG] added
  ;; protocol2:negotiate-ssl
  ;;     : string 
  ;;       integer
  ;;       (U 'yes 'no 'optional)
  ;;       (U 'sslv2-or-v3 'sslv2 'sslv3 'tls)
  ;;    -> (values input-port output-port)
  (define (protocol2:negotiate-ssl server port ssl ssl-encrypt)
    (let-values ([(in out) (tcp-connect server port)])
      (if (eq? ssl 'no)
          (values in out)
          (begin
            ; Write SSL request message:
            (write-bytes (integer->integer-bytes        8  4 #f #t) out)
            (write-bytes (integer->integer-bytes 80877103  4 #f #t) out)
            (flush-output out)
            ;(write-bytes (integer->integer-bytes        0  4 #f #t) out)
            ; Don't need to use get-response to parse this:
            ; it's just a single byte (#\S means "yes", #\N means "no", #\E is returned
            ; by older versions of PostgreSQL that don't support SSL):
            (let ([ssl-response (read-char in)])
              (cond
                ; SSL was accepted
                [(eq? ssl-response #\S)
                 (ports->ssl-ports in out #:mode 'connect #:encrypt ssl-encrypt #:close-original? #t)]
                ; SSL was denied: either go with non-SSL or raise an exception
                [(eq? ssl-response #\N)
                 (if (eq? ssl 'optional)
                     (values in out)
                     (raise-communication-error
                      'connect
                      "Could not connect to ~a:~a (SSL request denied)"
                      server port))]
                [(eq? ssl-response #\E)
                 (let* ([message-length (integer->integer-bytes (read-bytes 4 in) #f #t)]
                        [field-type     (read-byte in)]
                        [message        (if (= field-type 0)
                                            "No message given."
                                            (bytes->string/utf-8 (read-bytes (- message-length 5))))])
                   (raise-communication-error
                    'connect
                    "Could not connect to ~a:~a (SSL request unsupported: ~a)"
                    server port message))]
                [else
                 (raise-communication-error
                  'connect
                  "Could not connect to ~a:~a (unrecognised response from SSL request: ~a)"
                  server port ssl-response)]))))))
  
  ;; protocol2:new : input-port output-port -> protocol
  (define (protocol2:new inport outport)
    (make-protocol2 inport outport 0 #f protocol:lock:disconnected #f))
  
  ;; protocol2:reset : protocol2 -> message-generator
  (define (protocol2:reset protocol)
    (let [(mg (protocol2-message-generator protocol))
          (inport (protocol2-inport protocol))]
      (let loop [(mg mg)]
        (when (and mg (not (message-generator:done? mg)))
          (loop (message-generator:next mg))))
      (let [(new-mg (message-generator:new protocol))]
        (set-protocol2-message-generator! protocol new-mg)
        new-mg)))
  
  (define (protocol2:close protocol)
    (close-output-port (protocol2-outport protocol))
    (close-input-port (protocol2-inport protocol)))
  
  (define (protocol2:lock protocol status)
    (set-protocol2-status! protocol status)
    (set-protocol2-key! protocol 0)
    0)
  
  (define (protocol2:lock/key protocol status)
    (set-protocol2-status! protocol status)
    (let [(key lock-counter)]
      (set-protocol2-key! protocol key)
      (set! lock-counter (add1 lock-counter))
      key))
  
  (define protocol2:unlock 
    (case-lambda 
      [(protocol status)
       (protocol2:unlock protocol status 0)]
      [(protocol status key)
       (let ([protocol-status
              (if protocol
                  (protocol2-status protocol)
                  protocol:lock:disconnected)]
             [protocol-key (if protocol (protocol2-key protocol) 0)])
         (unless (and (eq? protocol-status status)
                      (= protocol-key key))
           (raise-sp-user-error 'lock "backend link is locked on state ~a (~a)"
                                protocol-status
                                protocol)))]))
  
  (define (protocol2:encode protocol message)
    (encode-message message (protocol2-outport protocol)))
  
  
  (define (message-generator:current/next mg)
    (force (message-generator-promise mg)))
  
  (define (message-generator:current mg)
    (let-values [((current next) (force (message-generator-promise mg)))]
      current))
  
  (define (message-generator:next mg)
    (let-values [((current next) (force (message-generator-promise mg)))]
      next))
  
  (define (message-generator:done? mg)
    (message-generator-done? mg))
  
  (define (message-generator:new protocol)
    (make-message-generator 
     protocol
     (delay
       (let [(next-message (parse-message protocol))]
         (values next-message
                 (if (end-of-exchange-message? next-message)
                     (make-message-generator protocol #f #t)
                     (message-generator:new protocol)))))
     #f))
  
  ;; parse-message : protocol -> Response
  (define (parse-message protocol)
    (with-handlers [(exn:fail?
                     (lambda (e) 
                       (make-FatalErrorResponse 
                        "FATAL" 
                        (format "Error communicating with backend: ~a" e)
                        #f
                        0)))]
      (parse-response protocol)))
  
  ; Many of these message formats are described here: 
  ;     http://developer.postgresql.org/pgdocs/postgres/protocol-message-formats.html
  (define (encode-message msg outport)
    (cond
      ;; Messages sent by client
      [(CancelRequest? msg)
       (write-int32 outport 16)
       (write-int32 outport 80877102)
       (write-int32 outport (CancelRequest-process-id msg))
       (write-int32 outport (CancelRequest-secret-key msg))]
      ;      [(CopyDataRows? msg)
      ;       (for-each (lambda (row)
      ;                   (write-astring outport row)
      ;                   (write-char #\newline outport))
      ;                 (CopyDataRows-rows msg))
      ;       (write-astring outport "\\.")
      ;       (write-char #\newline outport)]
      [(PasswordPacket? msg)
       (let [(ep (PasswordPacket-password msg))]
         (write-int32 outport (+ 5 (string-length ep)))
         (write-tstring outport ep))]
      ;      [(FunctionCall? msg)
      ;       (write-char #\F outport)
      ;       (write-tstring outport "")
      ;       (write-int32 outport (FunctionCall-oid msg))
      ;       (write-int32 outport (length (FunctionCall-arglist msg)))
      ;       (for-each 
      ;        (lambda (arg) 
      ;          (write-int32 outport (string-length arg))
      ;          (write-bytes arg outport))
      ;        (FunctionCall-arglist msg))]
      [(Query? msg)
       (write-char #\Q outport)
       (write-tstring outport (Query-sql msg))]
      [(StartupPacket? msg)
       (write-int32 outport 296)
       (write-int16 outport (car (StartupPacket-ver msg)))
       (write-int16 outport (cdr (StartupPacket-ver msg)))
       (write-limstring outport 64 (StartupPacket-db msg))
       (write-limstring outport 32 (StartupPacket-user msg))
       (write-limstring outport 64 (StartupPacket-cmdline msg))
       (write-limstring outport 64 (StartupPacket-unused msg))
       (write-limstring outport 64 (StartupPacket-tty msg))]
      [(Terminate? msg)
       (write-char #\X outport)]
      
      ;; Structures only sent by backend
      [(NotificationResponse? msg)
       (write-char #\A outport)
       (write-int32 outport (NotificationResponse-process-id msg))
       (write-tstring outport (NotificationResponse-condition msg))]
      [(AsciiRow? msg)
       (write-char #\D outport)
       (let [(fields (AsciiRow-fields msg))]
         (encode-NullFields (map sql-null? fields) outport)
         (for-each (lambda (field)
                     (write-int32 outport (+ 4 (string-length field)))
                     (write-astring outport field))
                   (filter (lambda (f) (not (sql-null? f))) fields)))]
      [(BinaryRow? msg)
       (write-char #\B outport)
       (let [(fields (BinaryRow-fields msg))]
         (encode-NullFields (map sql-null? fields) outport)
         (for-each (lambda (field)
                     (write-int32 outport (+ 4 (string-length field)))
                     (write-bytes field outport))
                   (filter (lambda (f) (not (sql-null? f))) fields)))]
      [(CompletedResponse? msg)
       (write-char #\C outport)
       (write-tstring outport (CompletedResponse-command msg))]
      [(FatalErrorResponse? msg)
       (write-char #\E outport)
       (cond [(and (MessageResponse-type msg) (FatalErrorResponse-level msg))
              (write-tstring outport
                             (format "~a ~a: ~a"
                                     (MessageResponse-type msg)
                                     (MessageResponse-message msg)
                                     (FatalErrorResponse-level msg)))]
             [(MessageResponse-type msg)
              (write-tstring outport
                             (format "~a: ~a"
                                     (MessageResponse-type msg)
                                     (MessageResponse-message msg)))]
             [else
              (write-tstring outport (MessageResponse-message msg))])]
      [(ErrorResponse? msg) 
       (write-char #\E outport)
       (cond [(MessageResponse-type msg)
              (write-tstring outport
                             (format "~a: ~a"
                                     (MessageResponse-type msg)
                                     (MessageResponse-message msg)))]
             [else
              (write-tstring outport (MessageResponse-message msg))])]
      [(CopyInResponse? msg) 
       (write-char #\G outport)]
      [(CopyOutResponse? msg) 
       (write-char #\H outport)
       (encode-message (make-CopyDataRows (CopyOutResponse-rows msg)) outport)]
      [(EmptyQueryResponse? msg)
       (write-char #\I outport)
       (write-tstring outport (EmptyQueryResponse-unused msg))]
      [(BackendKeyData? msg)
       (write-char #\K outport)
       (write-int32 outport (BackendKeyData-process-id msg))
       (write-int32 outport (BackendKeyData-secret-key msg))]
      [(NoticeResponse? msg)
       (write-char #\N outport)
       (cond [(MessageResponse-type msg)
              (write-tstring outport
                             (format "~a: ~a"
                                     (MessageResponse-type msg)
                                     (MessageResponse-message msg)))]
             [else
              (write-tstring outport (MessageResponse-message msg))])]
      [(CursorResponse? msg)
       (write-char #\P outport)
       (write-tstring outport (CursorResponse-name msg))]
      [(AuthenticationEncryptedPassword? msg)
       (write-char #\R outport)
       (write-int32 outport 4)
       (write-bytes (AuthenticationEncryptedPassword-salt msg) outport)]
      [(AuthenticationMD5Password? msg)
       (write-char #\R outport)
       (write-int32 outport 5)
       (write-bytes (AuthenticationMD5Password-salt msg) outport)]
      [(AuthenticationSCM? msg)
       (write-char #\R outport)
       (write-int32 outport 6)
       (write-bytes (AuthenticationSCM-data msg) outport)]
      [(Authentication? msg)
       (write-char #\R outport)
       (write-int32 outport 
                    (case (Authentication-method msg)
                      [(ok) 0]
                      [(kerberosV4) 1]
                      [(kerberosV5) 2]
                      [(unencrypted-password) 3]))]
      [(RowDescription? msg)
       (write-char #\T outport)
       (write-int16 outport (length (RowDescription-fields msg)))
       (for-each (lambda (fi)
                   (write-tstring outport (FieldInfo-name fi))
                   (write-int32 outport (FieldInfo-oid fi))
                   (write-int16 outport (FieldInfo-tsize fi))
                   (write-int32 outport (FieldInfo-tmod fi)))
                 (RowDescription-fields msg))]
      [(ReadyForQuery? msg)
       (write-char #\Z outport)])
    (flush-output outport))
  
  (define (encode-NullFields null-fields outport)
    (let* [(fields-length (length null-fields))
           (bytes-needed (ceiling (/ fields-length 8)))
           (bytelist
            (let byteloop [(bytes-left bytes-needed) 
                           (bytes '())
                           (fields-left null-fields)]
              (if (zero? bytes-left)
                  (reverse bytes)
                  (let bitloop [(bit 7) (byte 0) (fields-left fields-left)]
                    (if (or (< bit 0) (null? fields-left))
                        (byteloop (sub1 bytes-needed) 
                                  (cons byte bytes)
                                  fields-left)
                        (bitloop (sub1 bit) 
                                 (if (car fields-left)
                                     byte
                                     (bitwise-ior byte (arithmetic-shift 1 bit)))
                                 (cdr fields-left)))))))]
      (write-bytes (apply bytes bytelist) outport)))
  
  ;; Message Parsing
  
  (define (parse-response protocol)
    (let* [(inport (protocol2-inport protocol))
           (c (read-char inport))]
      (cond 
        [(eq? c #\A)
         (make-NotificationResponse (read-int32 inport) 
                                    (read-tstring inport))]
        [(eq? c #\B)
         (parse-BinaryRow inport protocol)]
        [(eq? c #\C)
         (make-CompletedResponse (read-tstring inport))]
        [(eq? c #\D)
         (parse-AsciiRow inport protocol)]
        [(eq? c #\E)
         (parse-ErrorResponse inport)]
        ;        [(eq? c #\G)
        ;         (make-CopyInResponse)]
        ;        [(eq? c #\H)
        ;         (make-CopyOutResponse (parse-CopyDataRows inport))]
        
        [(eq? c #\I)
         (make-EmptyQueryResponse (read-tstring inport))]
        
        [(eq? c #\K)
         (make-BackendKeyData (read-int32 inport) (read-int32 inport))]
        [(eq? c #\N) 
         (parse-NoticeResponse inport)]
        [(eq? c #\P)
         (make-CursorResponse (read-tstring inport))]
        [(eq? c #\R)
         (parse-Authentication inport)]
        [(eq? c #\T)
         (parse-RowDescription inport protocol)]
        ;        [(eq? c #\V)
        ;         (parse-FunctionResult/VoidResponse inport)]
        [(eq? c #\Z) 
         (make-ReadyForQuery)]
        [(memq c '(#\G #\H #\V))
         (error 'protocol 
                "unsupported feature: copy in, copy out, or function call")]
        [else
         (error (format "unknown response code ~a" c))])))
  
  ;; parse-Authentication : input-port -> msg
  (define (parse-Authentication port)
    (let [(n (read-int32 port))]
      (cond [(= n 0)
             (make-Authentication 'ok)]
            [(= n 1)
             (make-Authentication 'kerberosV4)]
            [(= n 2)
             (make-Authentication 'kerberosV5)]
            [(= n 3)
             (make-Authentication 'unencrypted-password)]
            [(= n 4)
             (make-AuthenticationEncryptedPassword 
              'encrypted-password
              (read-bytes 2 port))]
            [(= n 5)
             (make-AuthenticationMD5Password 
              'md5-password
              (read-bytes 4 port))]
            [(= n 6)
             (make-AuthenticationSCM 
              'scm
              (read-bytes 6 port))]
            [else
             (make-Authentication 'unknown)])))
  
  ;  ;; parse-CopyDataRows : input-port -> list<string>
  ;  (define (parse-CopyDataRows port)
  ;    (let [(line (read-line port))]
  ;      (if (string=? line (string #\\ #\.))
  ;          null
  ;          (cons line (parse-CopyDataRows port)))))
  
  ;; parse-ErrorResponse : input-port -> ErrorResponse
  (define (parse-ErrorResponse port)
    (define (error-code rawmsg)
      (match (regexp-match #px"C([0-9]{5})" rawmsg)
        [`(,full ,code) (string->number code)]
        [else #f]))
    (let* ([rawmsg (read-tstring port)]
           [code (error-code rawmsg)]
           [fmt (regexp-match #rx"([A-Z0-9 ]*): (.*)" rawmsg)])
      ;;(printf "Parsing error: ~s~ngot ~s~n" rawmsg fmt)
      (match fmt
       [#f
        (make-ErrorResponse #f rawmsg code)]
       [`(,full "FATAL 1" ,msg) 
         (make-FatalErrorResponse "FATAL" msg code 1)]
       [`(,full "FATAL 2" ,msg)
         (make-FatalErrorResponse "FATAL 2" msg code 2)]
       [`(,full "FATAL" ,msg)
        (make-FatalErrorResponse "FATAL" msg code #f)]
       [`(,full ,type ,msg)
        (make-ErrorResponse type msg code)])))
  
  (define (parse-NoticeResponse port)
    (let* [(rawmsg (read-tstring port))
           (fmt (regexp-match "([A-Z0-9 ]*): (.*)" rawmsg))]
      (cond [(not fmt) (make-NoticeResponse "NOTICE" rawmsg)]
            [else (make-NoticeResponse (cadr fmt) (caddr fmt))])))
  
  ;; parse-RowDescription : input-port protocol -> RowDescription
  (define (parse-RowDescription port protocol)
    (let [(numfields (read-int16 port))]
      (set-protocol2-last-field-count! protocol numfields)
      (make-RowDescription
       (build-list numfields 
                   (lambda (n)
                     (make-FieldInfo
                      (read-tstring port)
                      (read-int32 port)
                      (read-int16 port)
                      (read-int32 port)))))))
  
  ;; parse-AsciiRow : input-port protocol -> AsciiRow
  (define (parse-AsciiRow port protocol)
    (let* [(last-field-count (protocol2-last-field-count protocol))
           (nonnullfields (decode-NullFields port last-field-count))]
      ;;(fprintf (current-error-port) "NonNullFields: ~a~n" nonnullfields)
      (make-AsciiRow
       (build-list ;;vector
        last-field-count
        (lambda (n)
          (if (select-bit nonnullfields n)
              (let* [(raw (read-int32 port))
                     (runlen (- raw 4))]
                ;;(printf " -- reading field ~a with size ~a - 4~n" n raw)
                (read-limstring port runlen))
              sql-null))))))
  
  ;; parse-BinaryRow : input-port protocol -> BinaryRow
  (define (parse-BinaryRow port protocol)
    (let* [(last-field-count (protocol2-last-field-count protocol))
           (nonnullfields (decode-NullFields port last-field-count))]
      (make-BinaryRow
       (build-list ;;vector
        last-field-count
        (lambda (n)
          (if (select-bit nonnullfields n)
              (let [(runlen (read-int32 port))]
                (read-limstring port runlen))
              sql-null))))))
  
  ; [DJG] changed
  ;; decode-NullFields : input-port number -> (vector-of [0..127])
  (define (decode-NullFields port numfields)
    (let [(bytes (read-limbytes port (ceiling (/ numfields 8))))]
      (build-vector 
       (bytes-length bytes)
       (lambda (i)
         (bytes-ref bytes i)))))
  ; [DJG] /changed
  
  ;; select-bit : (vector-of [0..127]) integer -> boolean
  (define (select-bit bits index)
    (let* [(offset (modulo index 8))
           (vindex (/ (- index offset) 8))]
      (not (zero? 
            (bitwise-and (arithmetic-shift 1 (- 7 offset))
                         (vector-ref bits vindex))))))
  
  (define (parse-FunctionResult/VoidResponse port)
    (let [(c (read-char port))]
      (cond [(eq? c #\G)
             (let [(size (read-int32 port))]
               (let [(answer
                      (make-FunctionResultResponse 
                       (read-limstring port size)))]
                 (if (not (eq? (read-byte port) #\nul))
                     (error "Expected null at end of FunctionResult"))
                 answer))]
            [(eq? c #\nul)
             (make-FunctionVoidResponse)])))

  ;; constraint-error? : any -> (U #t #f)
  (define (constraint-error? resp)
    (and (ErrorResponse? resp)
         (let ([code (ErrorResponse-code resp)])
           (and (>= code 23000) (<= code 23999)))))
  )
 