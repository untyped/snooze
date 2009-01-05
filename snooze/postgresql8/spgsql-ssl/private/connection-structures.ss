;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module connection-structures mzscheme
  (provide (struct OkConnection ())
           (struct EncryptedPasswordResult (salt callback))
           (struct UnencryptedPasswordResult (callback))
           (struct MD5PasswordResult (salt callback))
           
           QueryResult?
           (struct SimpleQueryResult (command))
           (struct CursorResult (command cursor))
           (struct Recordset (command cursor binary? fields rows))
           (struct Copyset (command rows))
           
           (struct ErrorResult (type message))
           (struct NoticeResult (type message))
           (struct NotificationResult (event))
           
           )
  
  ;; inspector; use so all of these are transparent
  (define inspector (make-inspector))
  
  ;; ============== Connection results
  ;; Returned by the connect method, and also returned by their own callbacks.

  ;; A ConnectionResult is one of 
  ;; - OkConnection
  ;; - EncryptedPasswordResult
  ;; - UnencryptedPasswordResult
  ;; - MD5PasswordResult
  
  (define-struct OkConnection () inspector)
  ;; with result:symbol
  ;; The connection process has been successfully completed.
  
  (define-struct EncryptedPasswordResult (salt callback) inspector)
  ;; with salt:string, callback:(astring->ConnectionResult)
  ;; The backend requests a password encrypted (crypt()) with the given salt.
  
  (define-struct MD5PasswordResult (salt callback) inspector)
  ;; with salt:string, callback:(astring->ConnectionResult)
  ;; The backend requests a password md5-hashed with the given salt.
  
  (define-struct UnencryptedPasswordResult (callback) inspector)
  ;; with callback:(astring->ConnectionResult)
  ;; The backend requests an unencrypted password.  Respond by calling the 
  ;; callback with a single argument: the password as astring.
  
  ;; ============== Query results
  
  ;; The following structures are returned by the query method of connection%, 
  ;; and also by callbacks associated with this method.

  ;; A QueryResult is one of
  ;; - SimpleQueryResult
  ;; - CursorQueryResult
  ;; - Recordset
  ;; - Copyset
  
  (define-struct SimpleQueryResult (command) inspector)
  ;;  where command:astring
  ;; Base structure.  command is a string identifying what type of SQL statement 
  ;; was completed, plus other information (eg, "SELECT", "INSERT ...", etc)
  
  (define-struct CursorResult (command cursor) inspector)
  ;;  where cursor:astring
  ;; A statement involving an explicit or implicit cursor.
  
  (define-struct Recordset (command cursor binary? fields rows) inspector)
  ;;  where fields:(listof (list fieldname:astring fieldtype:number))
  ;;        rows:(listof (vectorof astring|sql-null))
  ;;        binary?:boolean
  ;; A list of fields and a list of rows.  Each row is represented as a vector, 
  ;; where values are given by their string representation, and nulls are 
  ;; represented as sql-null.
  
  (define-struct Copyset (command rows) inspector)
  ;;  where rows:(listof astring)
  ;; The result of a COPY table TO stdout ... statement.  Each string in the list 
  ;; is a string in the specified COPY format.

  ;; QueryResult? : any -> boolean
  (define (QueryResult? r)
    (or (SimpleQueryResult? r)
        (CursorResult? r)
        (Recordset? r)
        (Copyset? r)))
  
  ;; =============== Non-data query results
  ;; The following structures are returned on non-fatal errors, notices, and 
  ;; notification (see LISTEN and NOTIFY). They may be stripped by some interface
  ;; methods (eg, query-list, query-tuple ...)
  
  (define-struct ErrorResult (type message code) inspector)
  (define-struct NoticeResult (type message) inspector)
  (define-struct NotificationResult (event pid) inspector)
  

)