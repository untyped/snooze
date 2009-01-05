;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module sql-types mzscheme
  (require (lib "match.ss")
           (lib "time.ss" "srfi" "19")
           "exceptions.ss")
  (provide sql-null
           sql-null?
           
           sql-parsers
           sql-parse
           
           (struct sql-date (year month day))
           (struct sql-time (hour minute second tz))
           (struct sql-timestamp (year month day hour minute second fraction tz))
           
           sql-marshallers
           sql-marshal
           
           sql-format)
  
  ;; SQL null
  
  (define-values (sql-null sql-null?)
    (let-struct sql-null ()
      (values (make-sql-null) sql-null?)))
  
  ;; sql-parse procedures

  ;; Conversions take non-NULL values
  (define (parse-integer s)
    (let [(r (string->number (bytes->string/utf-8 s)))]
      (cond [(integer? r) r]
            [else 
             (raise-sp-user-error 'sql-parse 
                               "cannot interpret ~s as a sql integer" s)])))

  (define (parse-float s)
    (let [(r (string->number (bytes->string/utf-8 s)))]
      (cond [(number? r) r]
            [else 
             (raise-sp-user-error 'sql-parse 
                               "cannot interpret ~s as a sql float" s)])))

  (define (parse-string s) (bytes->string/utf-8 s))

  (define (parse-bytea s)
    (let [(out (open-output-string))
          (len (string-length s))]
      (let loop [(index 0)]
        (when (< index len)
          (let [(c (string-ref s index))]
            (if (char=? c #\\)
                (let-values [((char new-index) 
                              (parse-bytea/escaped s (add1 index)))]
                  (write-char char out)
                  (loop new-index))
                (begin (write-char c out)
                       (loop (add1 index)))))))
      (get-output-string out)))

  (define (parse-bytea/escaped s index)
    (let [(nc (string-ref s index))]
      (case nc
        ((#\\ #\') (values nc (add1 index)))
        (else 
         (let* [(ns (substring s index (+ 3 index)))
                (n (string->number ns 8))]
           (values (integer->char n) (+ 3 index)))))))

  (define (parse-boolean s)
    (cond [(bytes=? s #"t") #t]
          [(bytes=? s #"f") #f]
          [else 
           (raise-sp-user-error 'sql-parse 
                             "cannot interpret ~s as a SQL boolean" s)]))

  ;; DATE AND TIME
  
  (define-struct sql-date (year month day) (make-inspector))

  (define (parse-date d)
    (let ((date (string->date d "~Y-~m-~d")))
      (make-sql-date (srfi:date-year date)
                     (srfi:date-month date)
                     (srfi:date-day date))))

  (define-struct sql-time (hour minute second tz) (make-inspector))

  (define (parse-time t)
    (let ((date (string->date t "~k:~M:~S")))
      (make-sql-time (srfi:date-hour date)
                     (srfi:date-minute date)
                     (srfi:date-second date)
                     #f)))

  (define (parse-time-tz t)
    (let ((date (string->date t "~k:~M:~S~z")))
      (make-sql-time (srfi:date-hour date)
                     (srfi:date-minute date)
                     (srfi:date-second date)
                     (date-zone-offset date))))

  (define-struct sql-timestamp 
    (year month day hour minute second fraction tz) (make-inspector))
  
  (define (parse-timestamp t)
    (let ((date (string->date t "~Y-~m-~d ~k:~M:~S.~N")))
      (make-sql-timestamp (srfi:date-year date)
                          (srfi:date-month date)
                          (srfi:date-day date)
                          (srfi:date-hour date)
                          (srfi:date-minute date)
                          (srfi:date-second date)
                          (date-nanosecond date)
                          #f)))

  (define (parse-timestamp-tz t)
    (let ((date (string->date t "~Y-~m-~d ~k:~M:~S.~N~z")))
      (make-sql-timestamp (srfi:date-year date)
                          (srfi:date-month date)
                          (srfi:date-day date)
                          (srfi:date-hour date)
                          (srfi:date-minute date)
                          (srfi:date-second date)
                          (date-nanosecond date)
                          (date-zone-offset date))))
  
  (define (unsupported-parser s)
    (raise-internal-error 'unimplemented 
                          "parser for this data type not yet implemented"))

  (define sql-parsers/list
    (list 
     (list '(int2in int4in int8in tidin xidin cidin oidin) parse-integer)
     (list '(float4in float8in) parse-float)
     (list '(textin varcharin charin) parse-string)
     (list '(bytea) parse-bytea)
     (list '(boolin) parse-boolean)
     (list '(date_in) parse-date)
     (list '(time_in) parse-time)
     (list '(timestamptz_in) parse-timestamp-tz)
     (list '(timestamp_in) parse-timestamp)
     (list '(timetz_in) parse-time-tz)))
  
  
;;  (define iso-date-re
;;    (regexp
;;     "([0-9]*)-([0-9]*)-([0-9]*) 
;;      ([0-9]*):([0-9]*):([0-9]*)(.[0-9]*)?([-+0-9]*)?"))
;;  
;;  (define traditional-date-re
;;    (regexp
;;     "([0-9]*)/([0-9]*)/([0-9]*) ([0-9]*):([0-9]*):([0-9]*)(.[0-9]*)? 
;;      ([a-zA-Z]*)?"))
;;  
;;  (define (sql-parse-date/iso s)
;;    (let [(iso (regexp-match iso-date-re s))]
;;      (and iso
;;           (let [(iso (map (lambda (s) (and s (string->number s))) (cdr iso)))]
;;             (match iso 
;;               [(y m d h min s ps tz)
;;                (make-date (floor (* ps 1000000000))
;;                           s min h d m y tz)])))))
;;  
;;  (define (sql-parse-date/traditional s)
;;    (let [(trad (regexp-match traditional-date-re))]
;;      (and trad
;;           (match trad
;;             [(d/m1 d/m2 y h min s ps tzs)
;;              (make-date (floor (* ps 1000000000))
;;                         s min h 
;;                         (if (day-first) d/m1 d/m2)
;;                         (if (day-first) d/m2 d/m1)
;;                         y 
;;                         (and tzs (string->timezone tzs)))]))))
  
  ;; sql-format procedures
  
  (define (marshal-integer n)
    (format "~a" n))
  
  (define (marshal-float n)
    (format "~a" n))
  
  (define (get-input-port val)
    (cond [(string? val)
           (open-input-string val)]
          [(input-port? val)
           val]))
  
  ;; Reference for PostgreSQL lexical structure
  ;; http://www.us.postgresql.org/users-lounge/docs/7.2/
  ;;   postgres/sql-syntax.html#SQL-SYNTAX-CONSTANTS
  (define (marshal-string s)
    (let* [(s (if (string? s) s (format "~a" s)))
           (out (open-output-string))
           (len (string-length s))]
      (write-char #\' out)
      (let loop [(index 0)]
        (when (< index len)
          (let [(c (string-ref s index))]
            (case c
              [(#\\) (begin (write-char #\\ out)
                            (write-char #\\ out))]
              [(#\') (begin (write-char #\\ out)
                            (write-char #\' out))]
              [(#\nul) 
               (error 'sql-format-string 
                      "NUL character is not allowed in PostgreSQL string")]
              [else (begin (write-char c out))]))
          (loop (add1 index))))
      (write-char #\' out)
      (get-output-string out)))
  
  (define (marshal-bytea s)
    (let [(out (open-output-string))
          (in (get-input-port s))]
      (write-char #\' out)
      (let loop [(index 0)]
        (let [(c (read-char in))]
          (unless (eof-object? c)
            (let [(cn (char->integer c))]
              (cond
                [(char=? c #\\)
                 (display (string #\\ #\\ #\\ #\\) out)]
                [(char=? c #\')
                 (display (string #\\ #\') out)]
                [(and (<= 32 cn) (<= cn 126))
                 (write-char c out)]
                [else
                 (write-char #\\ out)
                 (write-char #\\ out)
                 (let* [(code (number->string cn 8))
                        (code-len (string-length code))]
                   (let loop [(k (- 3 code-len))]
                     (when (positive? k) (write-char #\0 out) (loop (sub1 k))))
                   (display code out))]))
            (loop (add1 index)))))
      (display "'::bytea" out)
      (get-output-string out)))

  (define (marshal-bool v)
    (if v "'t'::boolean" "'f'::boolean"))
  
  (define (unsupported-marshaller s)
    (raise-internal-error 'unimplemented 
                          "marshaller for this data type not yet implemented"))
  
  (define marshal-date unsupported-marshaller)
  (define marshal-time unsupported-marshaller)
  (define marshal-timestamp-tz unsupported-marshaller)
  (define marshal-timestamp unsupported-marshaller)
  (define marshal-time-tz unsupported-marshaller)
  
  (define sql-marshal/list
    (list 
     (list '(int2 int4 int8 tid xid cid oid) marshal-integer)
     (list '(float4 float8) marshal-float)
     (list '(text varchar char) marshal-string)
     (list '(bytea) marshal-bytea)
     (list '(bool) marshal-bool)
     (list '(date) marshal-date)
     (list '(time) marshal-time)
     (list '(timestamptz) marshal-timestamp-tz)
     (list '(timestamp) marshal-timestamp)
     (list '(timetz) marshal-time-tz)
     
     ;; SQL need not be transformed in any way.
     (list '(sql) values)))

#|
  
  AGGREGATE TYPE INPUT
  namein           string (of char)
  int2vectorin     vector (of int2)
  oidvectorin      list
  point_in         pg-point/pair (of float8)
  lseg_in          pg-lseg
  box_in           pg-box
  line_in          pg-line
  array_in         list
  
  SIMPLE TYPE INPUT
  int2in       integer
  int4in
  tidin
  xidin
  cidin
  int8in
  oidin

  float4in     float
  float8in

  textin       string
  varcharin
  byteain
  charin       ???

  boolin       boolean
  
  date_in           srfi-11 date
  time_in
  timstamptz_in
  timestamp_in
  timetz_in
  
  nabstimein        XXX
  reltimein         XXX
  tinterveralin     ???

  interval_in       ???

  regprocin
  path_in
  poly_in
  macaddr_in
  smgrin
  cash_in
  inet_in
  aclitemin
  bpcharin
  cidr_in
  circle_in
  bit_in
  varbit_in
  numeric_in
  
|#
  
  ;; Derived from 
  ;; http://www.us.postgresql.org/users-lounge/docs/7.2/postgres/datatype.html
  
  (define type-aliases
    '(;; Convenience aliases
      (string text)
      (float float4)
      
      ;; Standard PostgreSQL aliases
      (bigint int8)
      (boolean bool)
      (character char)
      (double float8)
      (double-precision float8)
      (integer int4)
      (int int4)
      (real float4)
      (smallint int2)

      (serial int4)
      (serial4 int4)
      (serial8 int8)
      
      ;; (bit-varying varbit)
      (character-varying varchar)
      
      (time-without-time-zone time)
      (time-with-time-zone timetz)
      (timestamp-without-time-zome timestamptz)
      (timestamp-with-time-zone timestamp)))
  
  (define sql-parsers
    (let [(ht (make-hash-table))]
      (for-each
       (match-lambda [`(,types ,parser)
                      (for-each (lambda (typename) 
                                  (hash-table-put! ht typename parser))
                                types)])
       sql-parsers/list)
      ht))
  
  (define sql-marshallers
    (let [(ht (make-hash-table))]
      (for-each (match-lambda 
                 [(typeinputs marshaller)
                  (for-each (lambda (typename) 
                              (hash-table-put! ht typename marshaller))
                            typeinputs)])
                sql-marshal/list)
      (for-each (match-lambda 
                 [(alias type)
                  (hash-table-put! ht alias (hash-table-get ht type))])
                type-aliases)
       ht))

  (define (sql-parse type str)
    (let [(parser (hash-table-get 
                   sql-parsers type
                   (lambda _ 
                     (raise-sp-user-error 'sql-parse
                                       "unknown type: ~s" type))))]
      (parser str)))
  
  (define (sql-marshal type value)
    (let [(marshal (hash-table-get 
                    sql-marshallers type
                    (lambda _ 
                      (raise-sp-user-error 'sql-marshal
                                        "unknown type: ~s" type))))]
      (marshal value)))

  (define (sql-format . args)
    (let loop [(format-strings null) (args args)]
      (cond [(or (null? args) (not (string? (car args))))
             (apply format (apply string-append (reverse format-strings))
                    (map (lambda (arg) (apply sql-marshal arg)) args))]
            [else 
             (loop (cons (car args) (cons " " format-strings)) 
                   (cdr args))])))
  
  )
