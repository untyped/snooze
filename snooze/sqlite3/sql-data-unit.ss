#lang scheme/unit

(require srfi/13/string
         srfi/19/time
         (only-in srfi/43/vector-lib vector-map)
         (planet untyped/unlib:3/time)
         "../base.ss"
         "../era/era.ss"
         "../generic/sql-data-sig.ss")

(import)

(export sql-data^)

; type any -> string
(define (escape-value type value)
  (cond [(boolean-type? type)  (guard value boolean? "boolean")          (if value "1" "0")]
        [(not value)           "NULL"]
        [(guid-type? type)     (guard value guid? "(U guid #f)")         (if (eq? (guid-entity value) (guid-type-entity type))
                                                                             (number->string (guid-id value))
                                                                             (raise-exn exn:fail:contract 
                                                                               (format "Could not escape value for SQL: wrong GUID entity: expected ~a, received ~a."
                                                                                       (entity-name (guid-entity value))
                                                                                       (entity-name (guid-type-entity type)))))]
        [(integer-type? type)  (guard value integer? "(U integer #f)")   (number->string value)]
        [(real-type? type)     (guard value real? "(U real #f)")         (number->string value)]
        [(string-type? type)   (guard value string? "(U string #f)")     (string-append "'" (regexp-replace* #rx"'" value "''") "'")]
        [(symbol-type? type)   (guard value symbol? "(U symbol #f)")     (string-append "'" (regexp-replace* #rx"'" (symbol->string value) "''") "'")]
        [(time-tai-type? type) (guard value time-tai? "(U time-tai #f)") (escape-time time-tai value)]
        [(time-utc-type? type) (guard value time-utc? "(U time-utc #f)") (escape-time time-utc value)]
        [else                  (raise-exn exn:fail:contract (format "Unrecognised type: ~s" type))]))

; type string -> any
(define (parse-value type value)
  (with-handlers ([exn? (lambda (exn) (raise-exn exn:fail:contract (exn-message exn)))])
    (cond [(guid-type? type)     (let ([id (inexact->exact (string->number value))])
                                   (and id (make-guid (guid-type-entity type) id)))]
          [(boolean-type? type)  (equal? value "1")]
          [(not value)           #f]
          [(integer-type? type)  (inexact->exact (string->number value))]
          [(real-type? type)     (string->number value)]
          [(string-type? type)   value]
          [(symbol-type? type)   (string->symbol value)]
          [(time-tai-type? type) (parse-time time-tai value)]
          [(time-utc-type? type) (parse-time time-utc value)]
          [else                  (raise-exn exn:fail:contract (format "Unrecognised type: ~s" type))])))

; (listof type) -> ((U (vectorof database-value) #f) -> (U (vectorof scheme-value) #f))
(define (make-parser type-list)
  ; (vectorof type)
  (define type-vector
    (list->vector type-list))
  ; (U (vectorof database-value) #f) -> (U (vectorof scheme-value) #f)
  (lambda (value-vector)
    (if value-vector
        (vector-map (lambda (index type val)
                      (parse-value type val))
                    type-vector
                    value-vector)
        #f)))

; Helpers --------------------------------------

; srfi19-time-type (U time-tai time-utc) -> string
(define (escape-time time-type time)
  (string-append (number->string (time-second time))
                 (string-pad (number->string (time-nanosecond time)) 9 #\0)))

; srfi19-time-type string -> (U time-tai time-utc)
(define (parse-time time-type value)
  (if (> (string-length value) 9)
      (let* ([sec  (string->number (string-drop-right value 9))]
             [nano (string->number (string-take-right value 9))])
        (make-time time-type nano sec))
      (let* ([nano (string->number value)])
        (make-time time-type (if nano nano 0) 0))))

; (guard any (any -> boolean) string)
(define-syntax guard
  (syntax-rules ()
    [(guard value predicate message)
     (unless (predicate value)
       (raise-exn exn:fail:contract
         (format "Could not escape value for SQL: expected ~a, received ~s." message value)))]))
