;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module io mzscheme
  (provide io:write
           io:write-int16
           io:write-int32
           io:write-null-terminated-bytes
           io:write-null-terminated-string
           io:write-byte
           io:write-byte/char
           io:write-bytes
           io:write-length+bytes
           io:write-length+string
           
           io:read
           io:read-int16
           io:read-int32
           io:read-null-terminated-bytes
           io:read-null-terminated-string
           io:read-byte
           io:read-byte/char
           io:read-bytes-as-string
           io:read-bytes-as-bytes
           io:read-length+bytes
           io:read-length+string)
  
  (define-syntax (io:write stx)
    (syntax-case stx ()
      [(io:write port #:int16 value)
       #'(io:write-int16 port value)]
      [(io:write port #:int32 value)
       #'(io:write-int32 port value)]
      [(io:write port #:string value)
       #'(io:write-null-terminated-string port value)]
      [(io:write port #:byte value)
       #'(io:write-byte port value)]
      [(io:write port #:byte/char value)
       #'(io:write-byte/char port value)]
      [(io:write port #:bytes value)
       #'(io:write-bytes port value)]
      [(io:write port #:length+bytes value)
       #'(io:write-length+bytes port value)]
      [(io:write port #:length+string value)
       #'(io:write-length+string port value)]
      [(io:write port #:bytes2 value)
       #'(io:write-bytes port value)]
      [(io:write port #:bytes4 value)
       #'(io:write-bytes port value)]
      [(io:write port bad-type . _)
       (raise-syntax-error 'io:write
                           "bad datatype keyword"
                           #'bad-type)]))
  
  (define-syntax (io:read stx)
    (syntax-case stx ()
      [(io:read port #:int16)
       #'(io:read-int16 port)]
      [(io:read port #:int32)
       #'(io:read-int32 port)]
      [(io:read port #:string)
       #'(io:read-null-terminated-string port)]
      [(io:read port #:byte)
       #'(io:read-byte port)]
      [(io:read port #:byte/char)
       #'(io:read-byte/char port)]
      [(io:read port #:bytes length)
       #'(io:read-bytes port length)]
      [(io:read port #:length+bytes)
       #'(io:read-length+bytes port)]
      [(io:read port #:length+string)
       #'(io:read-length+string port)]
      [(io:read port #:bytes2)
       #'(io:read-bytes-as-bytes port 2)]
      [(io:read port #:bytes4)
       #'(io:read-bytes-as-bytes port 4)]
      [(io:read port bad-type . _)
       (raise-syntax-error 'io:read
                           "bad datatype keyword"
                           #'bad-type)]))
  
  (define-syntax define-wr
    (syntax-rules ()
      [(_ (f port arg ...) default . body)
       (define f
         (case-lambda
           [(arg ...)
            (f default arg ...)]
           [(port arg ...)
            . body]))]))
  
  (define-syntax define-w
    (syntax-rules ()
      [(_ (f port arg ...) . body)
       (define-wr (f port arg ...) (current-output-port) . body)]))
  
  (define-syntax define-r
    (syntax-rules ()
      [(_ (f port arg ...) . body)
       (define-wr (f port arg ...) (current-input-port) . body)]))
  
  ;; WRITING FUNCTIONS
  
  ;; Integer functions expect UNSIGNED values.  User is responsible for 
  ;; doing whatever necessary to deal with negative numbers.
  
  ;; NOTE: The write functions do not report errors such as providing a number
  ;; to write-intN that is longer than N bits.  The function should silently
  ;; take the N least significant bits, but no guarantees...
  
  ;; write-int16 : port integer -> (void)
  ;; Writes a 16-bit integer, network byte order
  (define-w (io:write-int16 port val)
    (write-bytes (integer->integer-bytes val 2 #t #t) port))
  
  ;; write-int32 : port integer -> void
  ;; Writes a 32-bit integer, network byte order
  (define-w (io:write-int32 port val)
    (write-bytes (integer->integer-bytes val 4 #t #t) port))

  ;; write-byte : port byte -> void
  (define-w (io:write-byte port byte)
    (write-byte byte port))
  
  ;; write-byte/char : port char -> void
  (define-w (io:write-byte/char port char)
    (write-byte (char->integer char) port))
  
  ;; write-bytes : port bytes -> void
  (define-w (io:write-bytes port bytes)
    (write-bytes bytes port))
  
  ;; write-length+bytes : port bytes/#f -> void
  ;; #f indicates sql-null, represented as length -1
  (define-w (io:write-length+bytes port bytes)
    (if bytes
        (begin (io:write-int32 port (bytes-length bytes))
               (write-bytes bytes port))
        (begin (io:write-int32 port -1))))
  
  ;; write-length+string : port string -> void
  (define-w (io:write-length+string port string)
    (if string
        (begin (io:write-int32 port (string-utf-8-length string))
               (write-string string port))
        (begin (io:write-int32 port -1))))
  
  ;; write-null-terminated-bytes : port bytes -> void
  (define-w (io:write-null-terminated-bytes port bytes)
    (write-bytes bytes port)
    (write-byte 0 port))
  
  ;; write-null-terminated-string : port string -> void
  (define-w (io:write-null-terminated-string port string)
    (write-string string port)
    (write-byte 0 port))
  
  ;; READING
  
  ;; read-int16 : port -> integer
  (define-r (io:read-int16 port)
    (integer-bytes->integer (read-bytes 2 port) #t #t))
  
  ;; read-int32 : port -> integer
  (define-r (io:read-int32 port)
    (integer-bytes->integer (read-bytes 4 port) #t #t))
  
  ;; read-null-terminated-string : port -> string
  (define-r (io:read-null-terminated-string port)
    (let [(strport (open-output-bytes))]
      (let loop ()
        (let ([next (read-byte port)])
          (cond [(zero? next)
                 (get-output-string strport)]
                [else
                 (write-byte next strport)
                 (loop)])))))
  
  ;; read-null-terminated-bytes : port -> bytes
  (define-r (io:read-null-terminated-bytes port)
    (let [(strport (open-output-bytes))]
      (let loop ()
        (let ([next (read-byte port)])
          (cond [(zero? next)
                 (get-output-bytes strport)]
                [else
                 (write-byte next strport)
                 (loop)])))))
  
  ;; read-byte : port -> byte
  (define-r (io:read-byte port)
    (read-byte port))
  
  ;; read-byte : port-> char
  (define-r (io:read-byte/char port)
    (integer->char (read-byte port)))
  
  ;; read-bytes-as-bytes : port number -> bytes
  (define-r (io:read-bytes-as-bytes port n)
    (read-bytes n port))
  
  ;; read-bytes-as-string : port -> string
  (define-r (io:read-bytes-as-string port n)
    (bytes->string/utf-8 (read-bytes n port)))
  
  ;; read-length+bytes : port -> bytes | #f
  ;; As a special case, a "length" of -1 results in #f
  ;; Any other negative number will cause an error.
  (define-r (io:read-length+bytes port)
    (let ([len (io:read-int32 port)])
      (if (= len -1)
          #f
          (io:read-bytes-as-bytes port len))))
  
  ;; read-length+string : port -> string | #f
  ;; As a special case, a "length" of -1 results in #f
  ;; Any other negative number will cause an error.
  (define-r (io:read-length+string port)
    (let ([len (io:read-int32 port)])
      (if (= len -1)
          #f
          (io:read-bytes-as-string port len))))
  
  )
