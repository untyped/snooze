;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module interfaces mzscheme
  (require (lib "class.ss"))
  (provide (struct SimpleResult (command))
           (struct Recordset (info data))
           (struct FieldInfo (name))

           converter<%>

           base<%>
           connector<%>
           ssl-connector<%>
           primitive-query<%>
           query<%>
           prepare-query<%>)

  ;; A YesNoOptional is one of 'yes, 'no, 'optional
  ;; An SSLMode is one of 'sslv2-or-v3, 'sslv2, 'sslv3, 'tls

  ;; A QueryResult is one of:
  ;;  - (make-SimpleResult string)
  ;;  - (make-Recordset Header value)
  (define-struct SimpleResult (command) #f)
  (define-struct Recordset (info data) #f)

  ;; A Header is one of
  ;;  - FieldInfo
  ;;  - list of FieldInfo
  ;;  - #f

  ;; A FieldInfo is (make-FieldInfo string)
  (define-struct FieldInfo (name) #f)

  ;; A Statement is one of
  ;;  - string
  ;;  - StatementBinding

  ;; A Preparable is one of
  ;;  - string

  ;; A Collector = RowDescription -> b (b a ... -> b) (b -> c) Header

  ;; base<%>
  ;; Manages communication
  (define base<%>
    (interface ()
      ;; recv : stream -> message stream
      recv

      ;; send-message : message -> void
      send-message

      ;; buffer-message : message -> void
      buffer-message

      ;; flush-message-buffer : -> void
      flush-message-buffer

      ;; new-exchange : -> stream
      new-exchange

      ;; end-exchange : -> void
      end-exchange

      ;; after-connect : -> void
      after-connect

      ;; disconnect : -> void
      ;; disconnect : boolean -> void
      disconnect

      ;; connected? : -> boolean
      connected?

      ;; handle-parameter-status : string string -> void
      handle-parameter-status

      ;; handle-notice : string string string (listof (cons string string)) => void
      handle-notice

      ;; handle-notification : string -> void
      handle-notification))

  ;; connector<%>
  ;; Manages making connections
  (define connector<%>
    (interface ()
      ;; attach-to-ports : input-port output-port -> void
      attach-to-ports

      ;; start-connection-protocol : string string string/#f -> void
      start-connection-protocol

      ;; compute-cleartext-password : string/#f -> bytes
      compute-cleartext-password

      ;; compute-crypt-password : string/#f bytes -> bytes
      compute-crypt-password

      ;; compute-md5-password : string string bytes -> bytes
      compute-md5-password

      ;; handle-kerberos5-authentication : -> void
      handle-kerberos5-authentication

      ;; handle-scm-credential-authentication : -> void
      handle-scm-credential-authentication
      ))

  ;; ssl-connector<%>
  (define ssl-connector<%>
    (interface (connector<%>)
      ;; set-ssl-options : YesNoOptional SSLMode -> void
      set-ssl-options))

  ;; primitive-query<%>
  (define primitive-query<%>
    (interface ()
      ;; query* : (listof Statement) Collector -> (listof QueryResult)
      query*

      ;; prepare-multiple : (listof Preparable) -> (listof PreparedStatement)
      prepare-multiple

      ;; bind-prepared-statement : PreparedStatement (list-of value) -> Statement
      bind-prepared-statement

      ;; datum->external-representation : number datum -> string
      datum->external-representation
      ))

  ;; query<%>
  (define query<%>
    (interface ()
      ;; query : Statement -> QueryResult
      query

      ;; query-multiple : (list-of Statement) -> (list-of QueryResult)
      query-multiple

      ;; exec : Statement ... -> void
      exec

      ;; query-list : Statement -> (list-of value)
      query-list

      ;; query-row : Statement -> (vector-of value)
      query-row

      ;; query-maybe-row : Statement -> (vector-of value) or #f
      query-maybe-row

      ;; query-value : Statement -> value
      query-value

      ;; query-maybe-value : Statement -> value or #f
      query-maybe-value

      ;; map : Statement (value ... -> a) -> (list-of a)
      map

      ;; for-each : Statement (value ... -> void) -> void
      for-each

      ;; mapfilter : Statement (value ... -> a) (value ... -> boolean)
      ;;           -> (list-of a)
      mapfilter

      ;; fold : Statement (a value ... -> a) a -> a
      fold))

  ;; prepare-query<%>
  (define prepare-query<%>
    (interface ()
      ;; prepare : Preparable -> PreparedStatement
      prepare

      ;; prepare-multiple : (list-of Preparable) -> (list-of PreparedStatement)
      prepare-multiple

      ;; bind-prepared-statement : PreparedStatement (list-of param) -> Statement
      bind-prepared-statement

      ;; prepare-exec : Preparable -> datum ... -> void
      prepare-exec

      ;; prepare-query-list : Preparable -> datum ... -> (list-of value)
      prepare-query-list

      ;; prepare-query-row : Preparable -> datum ... -> (vector-of value)
      prepare-query-row

      ;; prepare-query-maybe-row : Preparable -> datum ... -> (vector-of value) or #f
      prepare-query-maybe-row

      ;; prepare-query-value : Preparable -> datum ... -> value
      prepare-query-value

      ;; prepare-query-maybe-value : Preparable -> datum ... -> value or #f
      prepare-query-maybe-value

      ;; prepare-map : Preparable ('a ... -> 'b) -> datum ... -> (list-of 'b)
      prepare-map

      ;; prepare-for-each : Preparable ('a ... -> void) -> datum ... -> void
      prepare-for-each

      ;; prepare-mapfilter : Preparable ('a ... -> 'b) ('a ... -> boolean)
      ;;                  -> datum ... -> (list-of 'b)
      prepare-mapfilter

      ;; prepare-fold : Preparable ('b 'a ... -> 'b) 'b -> datum ... -> 'b
      prepare-fold))


  ;; converter<%>
  (define converter<%>
    (interface ()
      ;; get-external->datum : symbol -> (string -> datum)
      get-external->datum

      ;; get-datum->external : symbol -> (datum -> string)
      get-datum->external))

  )
