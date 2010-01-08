;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module p3-msg-util mzscheme
  (require-for-syntax (planet "stx.ss" ("ryanc" "macros.plt" 1 1)))
  (require (planet "struct.ss" ("ryanc" "macros.plt" 1 1))
           (lib "port.ss")
           "io.ss")
  (provide (all-defined))
  
  (define-struct-property message-writer)
  (define-struct-property message-parser-descrim)
  (define-struct-property message-parser)
  
  ;; Structures for messages
  (define-struct* msg () #:transparent)

  (define-for-syntax stx@ datum->syntax-object)
  
  (define-syntax (define-msg stx)
    (define (gather-fields stx)
      (syntax-case stx (header literal field sequence length*)
        [(header c) null]
        [(literal type val) null]
        [(field type name) (list #'name)]
        [(field type name f unf) (list #'name)]
        [(sequence name part ...) (list #'name)]))
    (define (find-header-char stxs)
      (ormap (lambda (stx)
               (syntax-case stx (header)
                 [(header c) #'c]
                 [_ #f]))
             stxs))
    (define (join sym stx)
      (datum->syntax-object 
       stx
       (string->symbol 
        (string-append (symbol->string sym)
                       (symbol->string (syntax-e stx))))))
    (syntax-case stx ()
      [(_ name clause ...)
       (let [(fields (apply append 
                            (map gather-fields 
                                 (syntax-e #'(clause ...)))))]
         (with-syntax ([(field ...) fields]
                       [parse:name (stx@ #'name (symbol-append 'parse: #'name))]
                       [write:name (stx@ #'name (symbol-append 'write: #'name))]
                       [constructor (stx@ #'name (symbol-append 'make- #'name))])
           #`(begin 
               (define (parse:name p)
                 (msg-parser constructor p () clause ...))
               (define (write:name p v)
                 (msg-writer name v p clause ...))
               (define-msg-struct name (field ...) 
                 #,(find-header-char (syntax->list #'(clause ...)))
                 parse:name
                 write:name))))]))
  
  (define-syntax (define-msg-struct stx)
    (syntax-case stx ()
      [(define-msg-struct name (field ...))
       #'(define-msg-struct name (field ...) #f)]
      [(define-msg-struct name (field ...) descriminator)
       (with-syntax ([parser (stx@ #'name (symbol-append 'parse: #'name))]
                     [writer (stx@ #'name (symbol-append 'write: #'name))])
         #'(define-msg-struct name (field ...) descriminator parser writer))]
      [(define-msg-struct name (field ...) descriminator parser writer)
       #'(define-struct* name (field ...)
           (#:super msg)
           #:transparent
           (#:property message-parser parser)
           (#:property message-writer writer)
           (#:property message-parser-descrim descriminator))]))
  
  ;; Message writers should never call flush-output directly.
  
  (define-syntax (msg-writer stx)
    (define (build-accessor struct field)
      (stx@ struct (symbol-append struct '- field)))
    (define (++ a b) (and a b (+ a b)))
    (define (compute-length clauses)
      (cond [(null? clauses) 4]  ;; the length field itself
            [else
             (syntax-case* (car clauses) (header literal field sequence)
                           literal-identifier=?
               [(literal type value)
                (case (syntax-e #'type)
                  ((#:int16) (++ 2 (compute-length (cdr clauses))))
                  ((#:int32) (++ 4 (compute-length (cdr clauses))))
                  ((#:string) (++ (add1 (string-length (syntax-e #'value)))
                                  (compute-length (cdr clauses))))
                  ((#:byte #:byte/char) (++ 1 (compute-length (cdr clauses))))
                  ((#:bytes) (bytes-length (syntax-e #'value)))
                  (else (raise-syntax-error #f "bad type" #'type)))]
               [(field type . _)
                (case (syntax-e #'type)
                  ((#:int16) (++ 2 (compute-length (cdr clauses))))
                  ((#:int32) (++ 4 (compute-length (cdr clauses))))
                  ((#:byte #:byte/char) (++ 1 (compute-length (cdr clauses))))
                  (else #f))]
               [(sequence . _) #f])]))
    (syntax-case* stx (header literal field sequence) literal-identifier=?
      [(_ n v p)
       #'(void)]
      [(_ n v p (header c) clause ...)
       (let ([len (compute-length (syntax->list #'(clause ...)))])
         (if len
             #`(begin (io:write-byte/char p c)
                      (io:write-int32 p #,len)
                      (msg-writer n v p clause ...))
             #'(begin (let [(oport (open-output-bytes))]
                        (msg-writer n v oport clause ...)
                        (let [(bs (get-output-bytes oport))]
                          (io:write-byte/char p c)
                          (io:write-int32 p (+ 4 (bytes-length bs)))
                          (io:write-bytes p bs))))))]
      [(_ n v p (literal type value) clause ...)
       #'(begin (io:write p type value)
                (msg-writer n v p clause ...))]
      [(_ n v p (field type name) clause ...)
       #'(msg-writer n v p (field type name values values) clause ...)]
      [(_ n v p (field type name f unf) clause ...)
       #`(begin (io:write p type (unf (#,(build-accessor #'n #'name) v)))
                (msg-writer n v p clause ...))]
      ;; Sequences: single component case
      [(_ n v p (sequence name (type0 name0)) clause ...)
       #'(msg-writer n v p (sequence name (type0 name0 values values)) clause ...)]
      [(_ n v p (sequence name (type0 name0 f unf)) clause ...)
       #`(begin
           (let ([name (map unf (#,(build-accessor #'n #'name) v))])
             (io:write-int16 p (length name))
             (for-each (lambda (partval) (io:write p type0 partval)) name))
           (msg-writer n v p clause ...))]
      ;; Sequence: compound component (list) case
      [(_ n v p (sequence name part ...) clause ...)
       #`(begin 
           (let [(name (#,(build-accessor #'n #'name) v))]
             (io:write-int16 p (length name))
             (for-each (lambda (partval) (msg-write-sequence p partval part ...))
                       name))
           (msg-writer n v p clause ...))]))
  
  (define-syntax (msg-write-sequence stx)
    (syntax-case stx ()
      [(_ p partval)
       #'(void)]
      [(_ p partval (type name) part ...)
       #'(msg-write-sequence p partval (type name values values) part ...)]
      [(_ p partval (type name f unf) part ...)
       #'(begin (io:write p type (unf (car partval)))
                (let ([partval (cdr partval)])
                  (msg-write-sequence p partval part ...)))]
      [(_ p partval _else . _elses)
       (raise-syntax-error 'define-msg "unknown sequence part" #'_else)]))
  
  (define-syntax (msg-parser stx)
    (syntax-case stx (header literal field sequence)
      [(_ mk p (v ...))
       #'(mk v ...)]
      [(_ mk p (v ...) (header c) clause ...)
       #`(let* ([_length (io:read-int32 p)]
                [limport (make-limited-input-port p _length)])
           (msg-parser mk limport (v ...) clause ...))]
      [(_ mk p (v ...) (literal type val) clause ...)
       #`(let [(t (io:read p type))]
           (msg-parser mk p (v ...) clause ...))]
      [(_ mk p (v ...) (field type name) clause ...)
       #`(let [(name (io:read p type))]
           (msg-parser mk p (v ... name) clause ...))]
      [(_ mk p (v ...) (field type name f unf) clause ...)
       #`(let [(name (f (io:read p type)))]
           (msg-parser mk p (v ... name) clause ...))]
      ;; Sequence: single component case
      [(_ mk p (v ...) (sequence name (type0 name0)) clause ...)
       #'(msg-parser mk p (v ...) (sequence name (type0 name0 values values)) clause ...)]
      [(_ mk p (v ...) (sequence name (type0 name0 f unf)) clause ...)
       #`(let ([n (io:read-int16 p)])
           (let ([t (let loop ([i n] [items null])
                      (if (zero? i)
                          (reverse items)
                          (loop (sub1 i)
                                (cons (f (io:read p type0)) items))))])
             (msg-parser mk p (v ... t) clause ...)))]
      ;; Sequence: compound component case
      [(_ mk p (v ...) (sequence name part ...) clause ...)
       #`(let [(n (io:read-int16 p))]
           (let [(t (let loop [(i n) (items null)]
                      (if (zero? i)
                          (reverse items)
                          (loop (sub1 i)
                                (cons (msg-parse-sequence p () part ...)
                                      items)))))]
             (msg-parser mk p (v ... t) clause ...)))]
      [(_ mk p vs _else . _elses)
       (raise-syntax-error 'define-msg "unknown data part" #'_else)]))
  
  (define-syntax (msg-parse-sequence stx)
    (syntax-case stx ()
      [(_ p (v ...))
       #'(list v ...)]
      [(_ p (v ...) (type name) part ...)
       #'(msg-parse-sequence p (v ...) (type name values values) part ...)]
      [(_ p (v ...) (type name f unf) part ...)
       #'(let ([name (f (io:read p type))])
           (msg-parse-sequence p (v ... name) part ...))]
      [(_ p vs _else . _elses)
       (raise-syntax-error 'define-msg "unknown data part" #'_else)]))
  
  ;; define-parser SYNTAX
  ;; (define-parser name [spec ...]) defines 'name' as a function
  ;; that takes an input port that returns that next complete message
  ;; (described by the parser specs), an eof object, or raises an error
  ;; if it cannot parse a message from the port.
  (define-syntax (define-parser stx)
    (syntax-case stx ()
      [(_ name [spec ...])
       #'(define name
           (let [(table (list (parser-table-entry spec) ...))]
             (lambda (port)
               (let [(descrim (read-byte port))]
                 (cond [(eof-object? descrim)
                        eof]
                       [(assoc descrim table)
                        => (lambda (entry)
                             ((cdr entry) port))]
                       [else
                        (error 'name 
                               "cannot parse message starting with: ~s (~s)" 
                               descrim (integer->char descrim))])))))]))
  
  (define-syntax (parser-table-entry stx)
    (syntax-case stx ()
      [(_ msg-name)
       (identifier? #'msg-name)
       (let [(struct-type (stx@ #'msg-name (symbol-append 'struct: #'msg-name)))]
         #`(cons (char->integer (message-parser-descrim-value #,struct-type))
                 (message-parser-value #,struct-type)))]
      [(_ [char parser])
       #'(cons (char->integer char) parser)]))
  )
