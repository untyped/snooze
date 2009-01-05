(module sql-quote-unit mzscheme
  
  (require (lib "unitsig.ss")
           (lib "string.ss" "srfi" "13")
           (lib "time.ss" "srfi" "19")
           (lib "vector-lib.ss" "srfi" "43"))
  
  (require (file "../base.ss")
           (file "../era.ss")
           (file "../type.ss")
           (file "sql-sig.ss"))
  
  (provide sql-quote@)
  
  ;; unit sql-quote@ : -> sql-quote^
  (define sql-quote@
    (unit/sig sql-quote^
      (import)
      
      ;; quote-id : (U symbol string) -> string
      ;;
      ;; Quotes a field or table name. In SQLite's case this is 
      ;; done by wrapping the name in square brackets: this is
      ;; non-standard SQL and may be different in other databases
      ;; (e.g. MySQL and PostgreSQL).
      (define (quote-id identifier)
        (if (symbol? identifier)
            (string-append "`" (symbol->string identifier) "`")
            (string-append "`" identifier "`")))
      
      ;; quote-data type any -> string
      (define (quote-data type data)
        (cond
          [(eq? type type:id)
           (cond [(integer? data) (number->string data)]
                 [(equal? data (type-null type)) "NULL"]
                 [else (raise-exn exn:fail:snooze
                         (format "Expected data of type (U integer #f), given ~a" data))])]
          [(eq? type type:revision)
           (cond [(integer? data) (number->string data)]
                 [(equal? data (type-null type)) "NULL"]
                 [else (raise-exn exn:fail:snooze
                         (format "Expected data of type (U integer #f), given ~a" data))])]
          [(eq? type type:text)
           (cond [(string? data) (string-append "'" (regexp-replace* #rx"'" data "''") "'")]
                 [(equal? data (type-null type)) "NULL"]
                 [else (raise-exn exn:fail:snooze
                         (format "Expected data of type (U string #f), given ~a" data))])]
          [(eq? type type:integer)
           (cond [(integer? data) (number->string data)]
                 [(equal? data (type-null type)) "NULL"]
                 [else (raise-exn exn:fail:snooze
                         (format "Expected data of type (U integer #f), given ~a" data))])]
          [(eq? type type:real)
           (cond [(real? data) (number->string (exact->inexact data))]
                 [(equal? data (type-null type)) "NULL"]
                 [else (raise-exn exn:fail:snooze
                         (format "Expected data of type (U real #f), given ~a" data))])]
          [(eq? type type:symbol)
           (cond [(symbol? data) (string-append "'" (regexp-replace* #rx"'" (symbol->string data) "''") "'")]
                 [(equal? data (type-null type)) "NULL"]
                 [else (raise-exn exn:fail:snooze
                         (format "Expected data of type (U symbol #f), given ~a\n" data))])]
          [(eq? type type:boolean)
           (cond [(eq? data #t) "1"]
                 [(eq? data #f) "0"]
                 [else (raise-exn exn:fail:snooze
                         (format "Expected data of type (U #t #f), given ~a\n" data))])]
          [(eq? type type:time-tai)
           (cond [(time? data)
                  (cond [(eq? (time-type data) time-tai)
                         (string-append (number->string (time-second data))
                                        (string-pad (number->string (time-nanosecond data)) 9 #\0))]
                        [(eq? (time-type data) time-utc) 
                         (let ([data (time-utc->time-tai data)])
                           (string-append (number->string (time-second data))
                                        (string-pad (number->string (time-nanosecond data)) 9 #\0)))]
                        [else (raise-exn exn:fail:snooze
                                (format "Expected data of type (U time-tai time-utc #f), given ~a\n" data))])]
                 [(equal? data (type-null type)) "NULL"]
                 [else (raise-exn exn:fail:snooze
                         (format "Expected data of type (U time-tai time-utc #f), given ~a\n" data))])]
          [else
           (raise-exn exn:fail:snooze
             (format "Unrecognised data type / value: ~a / ~a" type data))]))
      
      ;; unquote-data : type string -> any
      (define (unquote-data type data)
        (cond
          [(eq? type type:text) data]
          [(or (eq? type type:id) 
               (eq? type type:revision)
               (eq? type type:integer)
               (eq? type type:real))
           (if data
               (string->number data)
               #f)]
          [(eq? type type:symbol)
           (if data
               (string->symbol data)
               #f)]
          [(eq? type type:boolean)
           (if (equal? data "1") 
               #t 
               #f)]
          [(eq? type type:time-tai)
           (if data
               (cond [(> (string-length data) 9)
                      (let* ([sec  (string->number (string-drop-right data 9))]
                             [nano (string->number (string-take-right data 9))])
                        (make-time time-tai nano sec))]
                     [(let* ([nano (string->number data)])
                        (make-time time-tai (if nano nano 0) 0))])
               #f)]
          [else
           (raise-exn exn:fail:snooze
             (format "Unrecognised data type / raw value: ~a / ~a" type data))]))
            
      ;; make-data-unquoter
      ;;     : (list-of type)
      ;;    -> ((U (vector-of (U string #f)) #f) -> (U (vector-of scheme-primitive) #f))
      ;; 
      ;; Creates a procedure that "unquotes" (parses) a vector of data retrieved from a database.
      (define (make-data-unquoter types)
        (let ([types (list->vector types)])
          (lambda (source)
            (if source
                (vector-map (lambda (index type val)
                              (unquote-data type val))
                            types
                            source)
                #f))))
      
      ))
  
  )
 