;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module bitbang mzscheme
  (provide write-int16
           read-int16
           
           write-int32
           read-int32
           
           write-limbytes
           read-limbytes
           
           write-astring
           
           write-limstring
           read-limstring
           
           write-tbytes
           read-tbytes
           
           write-tstring
           read-tstring
           )
  
  (require (lib "string.ss" "srfi" "13"))
  
  
  ;; All output functions should write to ports: this way we can send 
  ;; data directly through a connection, or we can print it out to a string
  ;; and then send the string.  The port shall be the first argument.
  
  ; [DJG] changed
  ;; In the backend documentation, "string" means C-style string in whatever
  ;; character encoding the database the client is set to accept. There are
  ;; two encodings involved:
  ;;   - The "server encoding" is the encoding chosen for the database. The
  ;;     database administrator sets it with createdb (or whatever):
  ;;         createdb --encoding UTF8 my_database
  ;;   - The "client encoding" is whatever encoding the client expects data
  ;;     to be sent in. spgsql sends the following query just after connection:
  ;;         SET CLIENT_ENCODING TO 'UTF8';
  ;;     The client and server encodings can be different as long as there is
  ;;     a compatible encoding conversion installed on the server. It turns out
  ;;     that there are a bunch of conversions suported by default, and UTF8
  ;;     is *always* an okay choice.
  ;; [DJG] /changed
  
  ;; WRITING FUNCTIONS
  
  ;; Integer functions expect UNSIGNED values.  User is responsible for 
  ;; doing whatever necessary to deal with negative numbers.
  
  ;; NOTE: The write functions do not report errors such as providing a number
  ;; to write-intN that is longer than N bits.  The function should silently
  ;; take the N least significant bits, but no guarantees...
  
  ;; write-int16 : port integer -> (void)
  ;; Writes a 16-bit integer, network byte order
  (define (write-int16 port val)
    (write-bytes (integer->integer-bytes val 2 #f #t) port))
  
  ;; write-int32 : port integer -> (void)
  ;; Writes a 32-bit integer, network byte order
  (define (write-int32 port val)
    (write-bytes (integer->integer-bytes val 4 #f #t) port))
  
  ;; write-astring : port string -> (void)
  ;; Writes an ASCII string.
  (define (write-astring port val)
    (write-bytes (string->bytes/utf-8 val)))
  
  ;; write-limstring : port size string -> (void)
  ;; Writes a string of length size, null padded on the end if necessary
  (define (write-limstring port size val)
    (write-limbytes port size (string->bytes/utf-8 val)))
  
  (define (write-limbytes port size val)
    (cond [(<= (bytes-length val) size)
           (write-bytes val port)
           (write-bytes (make-bytes (- size (bytes-length val)) 0) port)]
          [else
           (write-bytes (subbytes val 0 size) port)]))
  
  ;; write-tstring : port string -> (void)
  ;; Writes a null-terminated string to the port
  (define (write-tstring port val)
    (write-tbytes port (string->bytes/utf-8 val)))
  
  (define (write-tbytes port val)
    (write-bytes val port)
    (write-byte 0 port))
  
  ;  ;; write-bytestring : port string -> (void)
  ;  ;; Writes a stream of bytes (chars) as given in the string 
  ;  ;; Does no null-padding, no null-ending
  ;  (define (write-bytestring port bytestring)
  ;    (display bytestring port))
  
  ;; READING
  
  ;; read-int16 : port -> integer
  (define (read-int16 port)
    (integer-bytes->integer (read-bytes 2 port) #f #t))
  
  ;; read-int32 : port -> integer
  (define (read-int32 port)
    (integer-bytes->integer (read-bytes 4 port) #f #t))
  
  ;; read-limstring : port size -> [string of length size]
  ;; Reads size bytes from the port and sticks them in a string
  (define (read-limstring port size)
    (read-limbytes port size))
  
  (define (read-limbytes port size)
    (read-bytes size port))
  
  ;; read-tstring : port -> string
  ;; Reads a null-terminated string from the port
  (define (read-tstring port)
    (bytes->string/utf-8 (read-tbytes port)))
  
  (define (read-tbytes port)
    (let [(buffer (open-output-bytes))]
      (let loop ()
        (let ([next (read-byte port)])
          (unless (zero? next)
            (write-byte next buffer)
            (loop))))
      (get-output-bytes buffer)))
  
  )
 