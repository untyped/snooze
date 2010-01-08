;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module sql-data mzscheme
  (require (lib "plt-match.ss")
           (prefix srfi: (lib "19.ss" "srfi")))
  (provide sql-null
           sql-null?
           
           (struct sql-date (year month day))
           (struct sql-time (hour minute second nanosecond tz))
           (struct sql-timestamp (year month day hour minute second nanosecond tz))
           
           ;; For backwards compatibility
           sql-timestamp-fraction
           set-sql-timestamp-fraction!
           
           sql-datetime->srfi-date
           srfi-date->sql-date
           srfi-date->sql-time
           srfi-date->sql-time-tz
           srfi-date->sql-timestamp
           srfi-date->sql-timestamp-tz
           
           external=>datum
           datum=>external
           datum=>external/typename
           
           format-sql
           concat-sql)
  
  ;; SQL Data
  ;; Datatypes for things that have no appropriate corresponding Scheme datatype
  
  (define-values (sql-null sql-null?)
    (let-struct sql-null ()
      (values (make-sql-null) sql-null?)))
  
  (define-struct sql-date (year month day) #f)
  (define-struct sql-time (hour minute second nanosecond tz) #f)
  (define-struct sql-timestamp
    (year month day hour minute second nanosecond tz) #f)
  
  ;; Wretched hacks to retain compatibility
  (begin
    (set! make-sql-time
          (let ([old-make-time make-sql-time])
            (define make-sql-time
              (case-lambda
                [(h m s n tz) (old-make-time h m s n tz)]
                [(h m s tz) (old-make-time h m s 0 tz)]))
            make-sql-time))
    (define sql-timestamp-fraction sql-timestamp-nanosecond)
    (define set-sql-timestamp-fraction! set-sql-timestamp-nanosecond!))
  
  (define (sql-datetime->srfi-date datetime)
    (match datetime
      [(struct sql-date (year month day))
       (srfi:make-date 0 0 0 0 day month year 0)]
      [(struct sql-time (hour minute second nanosecond tz))
       (srfi:make-date nanosecond second minute hour 0 0 0 (or tz 0))]
      [(struct sql-timestamp (year month day hour minute second nanosecond tz))
       (srfi:make-date nanosecond second minute hour day month year (or tz 0))]
      [else
       (raise-type-error 'sql-datetime->srfi-date
                         "sql-date, sql-time, or sql-timestamp"
                         datetime)]))
  
  (define (srfi-date->sql-date date)
    (make-sql-date (srfi:date-year date)
                   (srfi:date-month date)
                   (srfi:date-day date)))
  
  (define (srfi-date->sql-time* date tz? ns)
    (make-sql-time (srfi:date-hour date)
                   (srfi:date-minute date)
                   (srfi:date-second date)
                   (or ns (srfi:date-nanosecond date))
                   (and tz? (srfi:date-zone-offset date))))
  
  (define srfi-date->sql-time
    (case-lambda
      [(date ns) (srfi-date->sql-time* date #f ns)]
      [(date) (srfi-date->sql-time* date #f #f)]))
  
  (define srfi-date->sql-time-tz
    (case-lambda
      [(date ns) (srfi-date->sql-time* date #t ns)]
      [(date) (srfi-date->sql-time* date #t #f)]))
  
  (define (srfi-date->sql-timestamp* date tz? ns)
    (make-sql-timestamp (srfi:date-year date)
                        (srfi:date-month date)
                        (srfi:date-day date)
                        (srfi:date-hour date)
                        (srfi:date-minute date)
                        (srfi:date-second date)
                        (or ns (srfi:date-nanosecond date))
                        (and tz? (srfi:date-zone-offset date))))
  
  (define srfi-date->sql-timestamp
    (case-lambda
      [(date ns) (srfi-date->sql-timestamp* date #f ns)]
      [(date) (srfi-date->sql-timestamp* date #f #f)]))
  
  (define srfi-date->sql-timestamp-tz
    (case-lambda
      [(date ns) (srfi-date->sql-timestamp* date #t ns)]
      [(date) (srfi-date->sql-timestamp* date #t #f)]))
  
  ;; external representation => Scheme datum
  ;; All input-conversions take strings
  
  ;; raise-parse-error : string string -> (raises error)
  (define (raise-parse-error type rep)
    (raise-user-error
     'external-representation->datum
     (format "cannot interpret as a SQL ~a: ~s" type rep)))
  
  ;; string & bytea need full decoder
  
  (define (parse-string s)
    s
    #;(define (decode in out)
        (define (loop)
          (let ([next (read-char in)])
            (cond [(eof-object? next)
                   (void)]
                  [(eq? next #\\)
                   (escaped-loop)]
                  [else
                   (write-char next out)
                   (loop)])))
        (define (escaped-loop)
          (let ([next (peek-char in)])
            (cond [(eq? next #\\)
                   (read-char in)
                   (write-char next out)]
                  [else
                   (let* ([s (read-string 3 in)]
                          [n (string->number s 8)])
                     (write-char (integer->char n) out))])
            (loop)))
        (loop))
    #;(if (regexp-match? #rx"\\\\" s)
          (let ([out (open-output-string)]
                [in (open-input-string s)])
            (decode in out)
            (get-output-string out))
          s))
  
  (define (parse-bytea s)
    (define (decode in out)
      (define (loop)
        (let ([next (read-char in)])
          (cond [(eof-object? next)
                 (void)]
                [(eq? next #\\)
                 (escaped-loop)]
                [else
                 (let ([next-as-byte (char->integer next)])
                   (unless (< next-as-byte 256)
                     (raise-parse-error "bytea" s))
                   (write-byte next-as-byte out)
                   (loop))])))
      (define (escaped-loop)
        (let ([next (peek-char in)])
          (cond [(eq? next #\\)
                 (read-char in)
                 (write-char next out)]
                [else
                 (let* ([s (read-string 3 in)]
                        [n (string->number s 8)])
                   (unless (< n 256)
                     (raise-parse-error "bytea" s))
                   (write-byte n out))])
          (loop)))
      (loop))
    (if (regexp-match? #rx"\\\\" s)
        (let ([out (open-output-bytes)]
              [in (open-input-string s)])
          (decode in out)
          (get-output-bytes out))
        (with-handlers ([exn:fail?
                         (lambda (e) (raise-parse-error "bytea" s))])
          (string->bytes/latin-1 s))))
  
  ;; other types do not contain non-ASCII bytes (FIXME: VERIFY)
  
  (define (parse-integer s)
    (or (string->number s)
        (raise-parse-error "integer" s)))
  
  (define (parse-real s)
    (cond [(string->number s) => exact->inexact]
          [(equal? s "NaN") +nan.0]
          [(equal? s "Infinity") +inf.0]
          [(equal? s "-Infinity") -inf.0]
          [else (raise-parse-error "real" s)]))
  
  (define (parse-numeric s)
    (cond [(equal? s "NaN") +nan.0]
          [(regexp-match #rx"^([0-9]*)$" s)
           ;; big integer
           => (lambda (m)
                (string->number s))]
          [(regexp-match #rx"^([0-9]*)\\.([0-9]*)$" s)
           => (lambda (m)
                (+ (string->number (cadr m))
                   (let ([fp (caddr m)])
                     (parse-exact-fraction fp))))]
          [else (raise-parse-error "numeric" s)]))
  
  ;; parse-exact-fraction : string[in #rx"[0-9]*"] -> exact number
  ;; Given the fractional part of a number (including leading zeros),
  ;; produces an exact number representing the fraction.
  ;; eg: (parse-exact-fraction "12") = 12/100
  (define (parse-exact-fraction s)
    (/ (string->number s)
       (expt 10 (string-length s))))
  
  (define (parse-boolean s)
    (cond [(equal? s "t") #t]
          [(equal? s "f") #f]
          [else (raise-parse-error "boolean" s)]))
  
  (define (parse-date d)
    (srfi-date->sql-date
     (srfi:string->date d "~Y-~m-~d")))
  
  (define time/ns-rx #rx"^[0-9]*:[0-9]*:[0-9]*\\.([0-9]*)")
  (define timestamp/ns-rx #rx"^.* [0-9]*:[0-9]*:[0-9]*\\.([0-9]*)")
  
  (define (ns-of t rx)
    (let ([m (regexp-match rx t)])
      (if m
          (* #e1e9 (parse-exact-fraction (cadr m)))
          0)))
  
  (define (parse-time t)
    (srfi-date->sql-time
     (srfi:string->date t "~k:~M:~S")
     (ns-of t time/ns-rx)))
  
  (define (parse-time-tz t)
    (srfi-date->sql-time-tz
     (srfi:string->date t "~k:~M:~S~z")
     (ns-of t time/ns-rx)))
  
  (define (parse-timestamp t)
    (srfi-date->sql-timestamp
     (srfi:string->date t "~Y-~m-~d ~k:~M:~S")
     (ns-of t timestamp/ns-rx)))
  
  (define (parse-timestamp-tz t)
    (srfi-date->sql-timestamp-tz
     (srfi:string->date t "~Y-~m-~d ~k:~M:~S~z")
     (ns-of t timestamp/ns-rx)))
  
  (define sql-parsers/list
    `([,parse-integer (int2in int4in int8in tidin xidin cidin oidin)]
      [,parse-real (float4in float8in)]
      [,parse-numeric (numeric_in)]
      [,parse-string (textin varcharin charin)]
      [,parse-bytea (byteain)]
      [,parse-boolean (boolin)]
      [,parse-date (date_in)]
      [,parse-time (time_in)]
      [,parse-time-tz (timetz_in)]
      [,parse-timestamp (timestamp_in)]
      [,parse-timestamp-tz (timestamptz_in)]))
  
  
  ;; Scheme datum => external representation
  ;; All conversions take the appropriate Scheme datatype
  ;; and produce bytes.
  ;; No conversion may be passed sql-null values.
  
  ;; raise-marshal-error : string datum -> (raises error)
  (define (raise-marshal-error type datum)
    (raise-user-error
     'datum->external-representation
     "cannot create ~s representation for value: ~s" type datum))
  
  ;; encode : input-port output-port boolean -> void
  (define (encode in out bytes-mode?)
    (define (loop)
      (let ([next-byte (read-byte in)])
        (cond [(eof-object? next-byte)
               (void)]
              [(= next-byte (char->integer #\\))
               (write-char #\\ out)
               (write-char #\\ out)
               (loop)]
              [(= next-byte 0)
               (unless bytes-mode?
                 (raise-user-error
                  'datum->external-representation
                  "NUL character not allowed"))
               (write-char #\\ out)
               (write-string "000" out)
               (loop)]
              [(and bytes-mode? (> next-byte 127))
               (write-char #\\ out)
               (let ([ns (number->string next-byte 8)])
                 (write-string "000" out (string-length ns) 3)
                 (write-string ns out))
               (loop)]
              [else
               (write-byte next-byte out)
               (loop)])))
    (loop))
  
  (define (marshal-string s)
    (unless (string? s)
      (raise-marshal-error "string" s))
    (if (regexp-match? #rx"[\0\\\\]" s)
        (let ([in (open-input-string s)]
              [out (open-output-string)])
          (encode in out #f)
          (get-output-string out))
        s))
  
  (define (marshal-bytea s)
    (unless (bytes? s)
      (raise-marshal-error "bytea" s))
    (let ([in (open-input-bytes s)]
          [out (open-output-string)])
      (encode in out #t)
      (get-output-string out)))
  
  (define (marshal-int2 n)
    (unless (and (integer? n) (exact? n) (<= #x-8000 n #x7FFF))
      (raise-marshal-error "int2" n))
    (number->string n))
  
  (define (marshal-int4 n)
    (unless (and (integer? n) (exact? n) (<= #x-80000000 n #x7FFFFFFF))
      (raise-marshal-error "int4" n))
    (number->string n))
  
  (define (marshal-int8 n)
    (unless (and (integer? n) (exact? n) (<= #x-8000000000000000 n #x7FFFFFFFFFFFFFFF))
      (raise-marshal-error "int8" n))
    (number->string n))
  
  (define (marshal-real n)
    (unless (real? n)
      (raise-marshal-error "real" n))
    (cond [(eqv? n +inf.0) "Infinity"]
          [(eqv? n -inf.0) "-Infinity"]
          [(eqv? n +nan.0) "NaN"]
          [else
           (number->string
            (exact->inexact n))]))
  
  (define (marshal-numeric n)
    (define (dlog10 n)
      (inexact->exact (ceiling (/ (log n) (log 2)))))
    (cond [(not (real? n))
           (raise-marshal-error "numeric" n)]
          [(eqv? n +nan.0)
           "NaN"]
          [(or (eqv? n +inf.0) (eqv? n -inf.0))
           (raise-marshal-error "numeric" n)]
          [(or (integer? n) (inexact? n))
           (number->string n)]
          [(exact? n)
           ;; Bleah.
           (or (number->exact-decimal n)
               (number->string (exact->inexact n)))]))
  
  (define (number->exact-decimal n)
    (define (factor-out n factor fpower)
      (let-values ([(q r) (quotient/remainder n factor)])
        (if (zero? r)
            (factor-out q factor (add1 fpower))
            (values n fpower))))
    (let* ([whole-part (truncate n)]
           [fractional-part (- (abs n) (abs whole-part))]
           [num (numerator fractional-part)]
           [den (denominator fractional-part)])
      (let*-values ([(den* fives) (factor-out den 5 0)]
                    [(den** twos) (factor-out den* 2 0)])
        (and (= 1 den**)
             (let* ([tens (max fives twos 1)]
                    [new-den (expt 10 tens)]
                    [new-num (* num (quotient new-den den))]
                    [num-str (number->string new-num)])
               (string-append (number->string whole-part)
                              "."
                              (make-string (- tens (string-length num-str))
                                           #\0)
                              num-str))))))
  
  (define (marshal-bool v)
    (if v "t" "f"))
  
  (define (marshal-date d)
    (srfi:date->string (sql-datetime->srfi-date d) "~Y-~m-~d"))
  
  (define (marshal-time t)
    (srfi:date->string (sql-datetime->srfi-date t) "~k:~M:~S.~N"))
  
  (define (marshal-time-tz t)
    (srfi:date->string (sql-datetime->srfi-date t) "~k:~M:~S.~N~z"))
  
  (define (marshal-timestamp t)
    (srfi:date->string (sql-datetime->srfi-date t) "~Y-~m-~d ~k:~M:~S.~N"))
  
  (define (marshal-timestamp-tz t)
    (srfi:date->string (sql-datetime->srfi-date t) "~Y-~m-~d ~k:~M:~S.~N~z"))
  
  #|
  (define (unsupported-marshaller type)
    (lambda (v)
      (raise-marshal-error (format "(unsupported) ~a" type) v)))
  (define marshal-date (unsupported-marshaller "date"))
  (define marshal-time (unsupported-marshaller "time"))
  (define marshal-time-tz (unsupported-marshaller "time-tz"))
  (define marshal-timestamp (unsupported-marshaller "timestamp"))
  (define marshal-timestamp-tz (unsupported-marshaller "timestamp-tz"))
  |#
  
  (define sql-marshal/list
    `([,marshal-int2 (int2out)]
      [,marshal-int4 (int4out xidout cidout oidout)]
      [,marshal-int8 (int8out tidout)]
      [,marshal-real (float4out float8out)]
      [,marshal-numeric (numeric_out)]
      [,marshal-string (textout varcharout charout)]
      [,marshal-bytea (byteaout)]
      [,marshal-bool (boolout)]
      [,marshal-date (date_out)]
      [,marshal-time (time_out)]
      [,marshal-time-tz (timetz_out)]
      [,marshal-timestamp (timestamp_out)]
      [,marshal-timestamp-tz (timestamptz_out)]))
  
  ;; Derived from 
  ;; http://www.us.postgresql.org/users-lounge/docs/7.2/postgres/datatype.html
  
  (define type=>typeout
    #hasheq([int2 . int2out]
            [int4 . int4out]
            [int8 . int8out]
            [xid . xidout]
            [cid . cidout]
            [oid . oidout]
            [tid . tidout]
            [float4 . float4out]
            [float8 . float8out]
            [numeric . numeric_out]
            [text . textout]
            [varchar . varcharout]
            [char . charout]
            [bytea . byteaout]
            [bool . boolout]
            [date . date_out]
            [time . time_out]
            [timetz . timetz_out]
            [timestamp . timestamp_out]
            [timestamptz . timestamptz_out]))
  
  (define alias=>type
    #hasheq(;; Convenience aliases
            (string . text)
            (float . float4)
            
            ;; Standard PostgreSQL aliases
            (bigint . int8)
            (boolean . bool)
            (character . char)
            (double . float8)
            (double-precision . float8)
            (integer . int4)
            (int . int4)
            (real . float4)
            (smallint . int2)
            (decimal . numeric)
            
            (serial . int4)
            (serial4 . int4)
            (serial8 . int8)
            
            ;; (bit-varying varbit)
            (character-varying . varchar)
            
            (time-without-time-zone . time)
            (time-with-time-zone . timetz)
            (timestamp-without-time-zome . timestamp)
            (timestamp-with-time-zone . timestamptz)))
  
  (define external=>datum
    (let [(ht (make-hash-table))]
      (for-each (lambda (t)
                  (for-each (lambda (typename) 
                              (hash-table-put! ht typename (car t)))
                            (cadr t)))
                sql-parsers/list)
      ht))
  
  (define datum=>external
    (let [(ht (make-hash-table))]
      (for-each (lambda (t)
                  (for-each (lambda (typename) 
                              (hash-table-put! ht typename (car t)))
                            (cadr t)))
                sql-marshal/list)
      ht))
  
  (define datum=>external/typename
    (let ([ht (make-hash-table)])
      (hash-table-for-each 
       type=>typeout
       (lambda (k v) (hash-table-put! ht k (hash-table-get datum=>external v))))
      (hash-table-for-each
       alias=>type
       (lambda (k v) (hash-table-put! ht k (hash-table-get ht v))))
      ht))
  
  ;; escape-literal : input-port output-port -> void
  (define (escape-literal in out)
    (define (loop)
      (let ([c (read-char in)])
        (unless (eof-object? c)
          (cond 
            [(or (eq? c #\') (eq? c #\\))
             (write-char #\\ out)
             (write-char c out)]
            [else
             (write-char c out)])
          (loop))))
    (loop))
  
  ;; escape-name-minimally : string -> string
  (define (escape-name-minimally s)
    (if (regexp-match? #rx"[^A-Za-z]" s)
        (escape-name s)
        s))
  
  ;; escape-name : string -> string
  (define (escape-name s)
    (string-append "\""
                   (regexp-replace #rx"\"" s "\"\"")
                   "\""))
  
  ;; sql-parse : symbol bytes -> datum
  (define (sql-parse type s)
    (let ([parser (hash-table-get external=>datum type #f)])
      (if parser
          (parser s)
          s)))
  
  ;; sql-marshal : marshalspec -> string
  (define (sql-marshal marshalspec)
    (match marshalspec
      [(list #:trust datum typename)
       (sql-marshal-trust datum typename)]
      [(list #:Name datum)
       (sql-marshal-name/preserve-case datum)]
      [(list #:name datum)
       (sql-marshal-name/no-preserve-case datum)]
      [(list #:sql code)
       (sql-marshal-sql code)]
      [(list typename datum)
       (sql-marshal-type+datum typename datum)]))
  
  (define (sql-marshal-type+datum typename datum)
    (unless (symbol? typename)
      (raise-type-error 'sql-marshal "symbol" typename))
    (sql-marshal-expr typename
                      datum
                      (symbol->string
                       (hash-table-get alias=>type
                                       typename
                                       (lambda () typename)))))
  
  (define (sql-marshal-sql code)
    (unless (string? code)
      (raise-type-error 'format-sql "string" code))
    code)
  
  (define (sql-marshal-trust datum typename)
    ;; FIXME: do more checking on valid typenames (ex: no "--")
    (unless (string? typename)
      (raise-type-error 'format-sql "string" typename))
    (sql-marshal-expr 'string
                      datum
                      typename))
  
  (define (sql-marshal-name/no-preserve-case datum)
    (unless (string? datum)
      (raise-type-error 'sql-marshal "string" datum))
    (escape-name-minimally (string-downcase datum)))
  
  (define (sql-marshal-name/preserve-case datum)
    (unless (string? datum)
      (raise-type-error 'sql-marshal "string" datum))
    (escape-name datum))
  
  
  ;; sql-marshal-expr : symbol datum string -> string
  (define (sql-marshal-expr type datum cast-type)
    (define marshaller (hash-table-get datum=>external/typename type #f))
    (unless marshaller
      (raise-user-error 'sql-marshal "unknown type: ~s" type))
    (if (sql-null? datum)
        "NULL"
        (let ([data (marshaller datum)]
              [out (open-output-string)])
          (write-string "CAST( E'" out)
          (escape-literal (open-input-string data) out)
          (write-string "' AS " out)
          (write-string cast-type out)
          (write-string ")" out)
          (get-output-string out))))
  
  (define-for-syntax (type-spec->expr function tp)
    (syntax-case tp ()
      [(#:trust datum type)
       #'(sql-marshal-trust datum type)]
      [(#:name datum)
       #'(sql-marshal-name/no-preserve-case datum)]
      [(#:Name datum)
       #'(sql-marshal-name/preserve-case datum)]
      [(#:sql code)
       #'(sql-marshal-sql code)]
      [(type datum)
       (not (memq (syntax-e #'type) '(#:trust #:Name #:name #:sql)))
       (begin
         (unless (identifier? #'type)
           (raise-syntax-error 'format-sql "expected SQL type name"
                               #'type))
         #'(sql-marshal-type+datum 'type datum))]
      [else
       (raise-syntax-error function "bad type-spec" tp)]))
  
  ;; (format-sql format-string [type datum] ...) SYNTAX
  (define-syntax (format-sql stx)
    (syntax-case stx ()
      [(format-sql part ...)
       (let ()
         (define (format-part? part)
           (string? (syntax-e part)))
         (define (ok-format-string? str)
           (regexp-match? #rx"^(?:[^~]|(?:~[an%~]))*$" str))
         (define (partition parts)
           (let loop ([fparts null] [parts parts])
             (cond [(and (pair? parts) (format-part? (car parts)))
                    (unless (ok-format-string? (syntax-e (car parts)))
                      (raise-syntax-error
                       'format-sql
                       "expected format string with only '~a' placeholders"
                       (car parts)))
                    (loop (cons " " (cons (syntax-e (car parts)) fparts))
                          (cdr parts))]
                   [else
                    (values (reverse fparts) parts)])))
         (define-values (format-parts type-parts)
           (partition (syntax->list #'(part ...))))
         (with-syntax ([(part-as-string ...)
                        (map (lambda (tp) (type-spec->expr 'format-sql tp))
                             type-parts)])
           #`(format (quote #,(apply string-append format-parts))
                     part-as-string ...)))]))
  
  ;; concat-sql SYNTAX
  (define-syntax (concat-sql stx)
    (syntax-case stx ()
      [(concat-sql fragment ...)
       (let ()
         (define (process fragment)
           (if (string? (syntax-e fragment))
               #`(quote #,fragment)
               (type-spec->expr 'concat-sql fragment)))
         (with-syntax ([(string-expr* ...)
                        (map process (syntax->list #'(fragment ...)))])
           (with-syntax ([((string-expr ...) ...) #'((string-expr* '" ") ...)])
             #'(string-append string-expr ... ...))))]))
  
  ;  ;; format-sql-proc : format-string ... (list symbol datum) ... -> string
  ;  (define (format-sql-proc . args)
  ;    (let loop ([format-strings null] [args args])
  ;      (if (and (pair? args) (string? (car args)))
  ;          (begin
  ;            (unless (ok-format-string? (car args))
  ;              (raise-type-error 'format-sql-proc
  ;                                "format string with only ~a placeholders"
  ;                                (car args)))
  ;            (loop (cons " " (cons (car args) format-strings))
  ;                  (cdr args)))
  ;          (apply format
  ;                 (apply string-append (reverse format-strings))
  ;                 (map sql-marshal args)))))
  
  ;  (define (ok-format-string? str)
  ;    (regexp-match? #rx"^(?:[^~]|(?:~[an%~]))*$" str))
  
  )
