(module sql-quote-unit mzscheme
  
  (require (lib "etc.ss")
           (lib "pregexp.ss")
           (lib "unitsig.ss")
           (lib "list.ss"       "srfi" "1")
           (lib "string.ss"     "srfi" "13")
           (lib "time.ss"       "srfi" "19")
           (lib "vector-lib.ss" "srfi" "43"))
  
  (require (file "spgsql-ssl/spgsql.ss")
           (file "../base.ss")
           (file "../era.ss")
           (file "../type.ss")
           (file "../generic/sql-sig.ss")
           (prefix generic: (file "../generic/sql-quote-unit.ss")))
  
  (provide sql-quote@)
  
  ;; Provides a DBMS specific unit for quoting/unquoting data primitives. See:
  ;;
  ;;     ../sql-sig.ss
  ;;     ../generic/sql-quote.ss
  ;;
  ;; for more information.
  
  (define sql-quote@
    (let ([mixin@
           (unit/sig sql-quote^
             (import (generic : sql-quote^))
             
             ;; quote-id : (U symbol string) -> string
             ;;
             ;; Quotes a field or table name. In SQLite's case this is 
             ;; done by wrapping the name in square brackets
             (define (quote-id identifier)
               (if (symbol? identifier)
                   (string-append "\"" (symbol->string identifier) "\"")
                   (string-append "\"" identifier "\"")))
             
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
                  (cond [(eq? data #t) "true"]
                        [(eq? data #f) "false"]
                        [else (raise-exn exn:fail:snooze
                                (format "Expected data of type (U #t #f), given ~a\n" data))])]
                 [(eq? type type:time-tai)
                  (cond [(time? data)
                         ; NOTE: PostgreSQL expects dates in ISO 8601 format. There are formats for date->string
                         ; that write versions of this, but I'm being explicit below and now using them.
                         ;
                         ; NOTE: TODO: HACK: There are hacks here to get around a bug in SRFI-19. This bug has been
                         ; spotted elsewhere and reported appropriately. For example:
                         ; 
                         ;     http://permalink.gmane.org/gmane.comp.java.sisc.devel/634
                         ;
                         ; once this bug has been fixed, the calls to string-append / date->string / string-pad
                         ; can be replaced with single calls to date->string:
                         ;
                         ;     (date->string (time-tai->date data 0) "'~5.~N'")
                         ;
                         ; the "requires" statement for SRFI-13 will probably be superfluous once we're not using
                         ; string-pad anymore.
                         (cond [(eq? (time-type data) time-tai) 
                                ; TODO: Change this expression (see comment above):
                                (string-append (date->string (time-tai->date data 0) "'~Y-~m-~d ~H:~M:~S.")
                                               (string-pad (date->string (time-tai->date data) "~N'") 10 #\0))]
                               [(eq? (time-type data) time-utc) 
                                (let ([data (time-utc->time-tai data)])
                                  ; TODO: Change this expression (see comment above):
                                  (string-append (date->string (time-tai->date data 0) "'~Y-~m-~d ~H:~M:~S.")
                                                 (string-pad (date->string (time-tai->date data) "~N'") 10 #\0)))]
                               [else (raise-exn exn:fail:snooze
                                       (format "Expected data of type (U time-tai time-utc #f), given ~a\n" data))])]
                        [(equal? data (type-null type)) "NULL"]
                        [else (raise-exn exn:fail:snooze
                                (format "Expected data of type (U time-tai time-utc #f), given ~a\n" data))])]
                 [else
                  (raise-exn exn:fail:snooze
                    (format "Unrecognised data type / value: ~a / ~a" type data))]))
             
             ;; unquote-data : type (U bytes string sql-null) -> any
             (define (unquote-data type data)
               (cond
                 [(eq? type type:text) 
                  (cond [(sql-null? data) #f]
                        [(bytes? data)    (bytes->string/utf-8 data)]
                        [(string? data)   data]
                        [else             (raise-exn exn:fail:snooze
                                            (format "Cannot parse ~a: ~s" type data))])]
                 [(or (eq? type type:id) 
                      (eq? type type:revision)
                      (eq? type type:integer)
                      (eq? type type:real))
                  (cond
                    [(sql-null? data)     #f]
                    [(bytes? data)        (string->number (bytes->string/utf-8 data))]
                    [(string? data)       (string->number data)]
                    [else                 (raise-exn exn:fail:snooze
                                            (format "Cannot parse ~a: ~s" type data))])]
                 [(eq? type type:symbol)
                  (cond
                    [(sql-null? data)     #f]
                    [(bytes? data)        (string->symbol (bytes->string/utf-8 data))]
                    [(string? data)       (string->symbol data)]
                    [else                 (raise-exn exn:fail:snooze
                                            (format "Cannot parse ~a: ~s" type data))])]
                 [(eq? type type:boolean)
                  (cond
                    [(sql-null? data)     #f]
                    [(equal? data #"t")   #t]
                    [(equal? data "t")    #t]
                    [else                 #f])]
                 [(eq? type type:time-tai)
                  (cond
                    [(sql-null? data)     #f]
                    ; Note: "~5" means "ISO-8601 year-month-day-hour-minute-second format",
                    ; which is the default format returned by PostgreSQL for timestamp-without-timezone
                    ; data.
                    [(bytes? data)        (unquote-time-tai (bytes->string/utf-8 data))]
                    [(string? data)       (unquote-time-tai data)]
                    [else                 (raise-exn exn:fail:snooze
                                            (format "Cannot parse ~a: ~s" type data))])]
                 [else
                  (raise-exn exn:fail:snooze
                    (format "Unrecognised data type / raw value: ~a / ~a" type data))]))
             
             ;; iso-8601-regexp-1 : pregexp
             ;;
             ;; Matches the yy-mm-dd hh:mm:ss part of a timestamp string.
             (define iso-8601-regexp-1 (pregexp "^([0-9]{4})-([0-9]{2})-([0-9]{2})[ T]([0-9]{2}):([0-9]{2}):([0-9]{2})"))
             
             ;; iso-8601-regexp-2 : pregexp
             ;;
             ;; Matches the nanoseconds part of a timestamp string.
             (define iso-8601-regexp-2 (pregexp "\\.([0-9]{1,9})$"))
             
             ;; unquote-time-tai : string -> time
             (define (unquote-time-tai data)
               (let ([matches1 (pregexp-match iso-8601-regexp-1 data)]
                     [matches2 (pregexp-match iso-8601-regexp-2 data)])
                 (if matches1
                     (date->time-tai (apply make-srfi:date
                                            (append 
                                             ; Nanosecond
                                             (if matches2
                                                 (list (string->number (string-pad-right (cadr matches2) 9 #\0)))
                                                 (list 0))
                                             ; Second minute hour day month year
                                             (map string->number (reverse (cdr matches1)))
                                             ; Timezone-offset
                                             (list 0))))
                     (raise-exn exn:fail:snooze
                       (format "Expected ISO 8601 formatted string, received ~a" data)))))
             
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
             
             )])
      
      (compound-unit/sig
        (import)
        (link (original  : sql-quote^ (generic:sql-quote@))
              (variation : sql-quote^ (mixin@ original)))
        (export (open variation)))))
  
  )
 