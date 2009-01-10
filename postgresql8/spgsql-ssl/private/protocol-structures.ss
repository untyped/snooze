;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module protocol-structures mzscheme
  (provide (struct msg ())
           (struct MessageResponse (type message))
           (struct AsciiRow (fields))
           (struct Authentication (method))
           (struct AuthenticationEncryptedPassword (salt))
           (struct AuthenticationMD5Password (salt))
           (struct AuthenticationSCM (data))
           (struct BackendKeyData (process-id secret-key))
           (struct BinaryRow (fields))
           (struct CancelRequest (process-id secret-key))
           (struct CompletedResponse (command))
           (struct CopyDataRows (rows))
           (struct CopyInResponse ())
           (struct CopyOutResponse (rows))
           (struct CursorResponse (name))
           (struct EmptyQueryResponse (unused))
           (struct ErrorResponse (code))
           (struct FatalErrorResponse (level))
           (struct FunctionCall (oid arglist))
           (struct FunctionResultResponse (result))
           (struct FunctionVoidResponse ())
           (struct NoticeResponse ())
           (struct NotificationResponse (process-id condition))
           (struct Query (sql))
           (struct PasswordPacket (password))
           (struct ReadyForQuery ())
           (struct RowDescription (fields))
           (struct FieldInfo (name oid tsize tmod))
           (struct StartupPacket (ver db user cmdline unused tty))
           (struct Terminate ())
           )

  ;; An astring is a string containing only Latin-1 characters.
  
  ;; inspector, so all these are transparent
  (define inspector (make-inspector))
  
  ;; Structures for messages
  (define-struct msg () inspector)
  
  ;; Superstruct for Error/Notice structs
  (define-struct (MessageResponse msg) (type message) inspector)
  ;;  where message is a astring
  
  ;; Begin message struct definitions
  
  (define-struct (AsciiRow msg) (fields) inspector)
  ;;  where fields is list<astring>
  
  (define-struct (Authentication msg) (method) inspector)
  ;;  where method is 'ok | 'kerberosV4 | 'kerberosV5 | 'unencrypted-password 
  ;;                  'encrypted-password
  (define-struct (AuthenticationEncryptedPassword Authentication)
    (salt) inspector)
  ;;  where salt is bytes of length 2
  (define-struct (AuthenticationMD5Password Authentication) (salt) inspector)
  ;; where salt is a bytes of length 4  
  (define-struct (AuthenticationSCM Authentication) (data) inspector)
  ;; where data is some sort of 6-byte bytes (purpose unknown)

  (define-struct (BackendKeyData msg) (process-id secret-key) inspector)
  ;;  where process-id is integer, secret-key is integer
  
  (define-struct (BinaryRow msg) (fields) inspector)
  ;;  where fields is vector<bytes>
  
  (define-struct (CancelRequest msg) (process-id secret-key) inspector)
  ;;  where process-id is integer, secret-key is integer
  
  (define-struct (CompletedResponse msg) (command) inspector)
  ;;  where command is astring
  
  (define-struct (CopyDataRows msg) (rows) inspector)
  ;;  where rows is list<astring>
  
  (define-struct (CopyInResponse msg) () inspector)
  (define-struct (CopyOutResponse msg) (rows) inspector)
  ;;  where rows is list<astring>
  
  (define-struct (CursorResponse msg) (name) inspector)
  ;;  where name is astring
  
  (define-struct (EmptyQueryResponse msg) (unused) inspector)
  ;;  where unused is astring

  ;; struct ErrorResponse : (U integer #f)
  (define-struct (ErrorResponse MessageResponse) (code) inspector)
  
  (define-struct (FatalErrorResponse ErrorResponse) (level) inspector)
  ;; where level is 1 or 2
  
  (define-struct (FunctionCall msg) (oid arglist) inspector)
  ;;  where oid is integer, arglist is list<bytes>
  
  (define-struct (FunctionResultResponse msg) (result) inspector)
  ;;  where result is bytes
  
  (define-struct (FunctionVoidResponse msg) () inspector)
  
  (define-struct (NoticeResponse MessageResponse) () inspector)
  
  (define-struct (NotificationResponse msg) (process-id condition) inspector)
  ;;  where process-id is integer, condition is a string
  
  (define-struct (PasswordPacket msg) (password) inspector)
  ;;  where password is string
  
  (define-struct (Query msg) (sql) inspector)
  ;;  where sql is astring
  
  (define-struct (ReadyForQuery msg) () inspector)
  
  (define-struct (RowDescription msg) (fields) inspector)
  ;;  where fields is list<FieldInfo>
  (define-struct FieldInfo (name oid tsize tmod) inspector)
  ;;  where name is astring, oid is integer, tsize is integer, tmod is integer
  
  (define-struct (StartupPacket msg) (ver db user cmdline unused tty) inspector)
  ;;  where version is pair<int,int>, db is astring, user is astring,
  ;;        cmdline is astring, unused is astring, tty is astring
  
  (define-struct (Terminate msg) () inspector)
  
  )
