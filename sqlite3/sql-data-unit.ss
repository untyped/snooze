#lang scheme/unit

(require "../base.ss")

(require srfi/13
         srfi/19
         (only-in srfi/43 vector-map)
         (unlib-in time)
         "../era/era.ss"
         "../generic/sql-data-sig.ss")

(import)

(export sql-data^)

; type any -> string
(define (escape-value type value)
  (cond [(boolean-type? type)  (guard type value boolean?  "boolean")         (if value "1" "0")]
        [(not value)           "NULL"]
        [(guid-type? type)     (guard type value guid?     "(U guid #f)")     (cond [(not (eq? (guid-entity value) (guid-type-entity type)))
                                                                                     (error (format "wrong guid entity: expected ~a, received ~a."
                                                                                                    (entity-name (guid-entity value))
                                                                                                    (entity-name (guid-type-entity type))))]
                                                                                    [(guid-id value) => number->string]
                                                                                    [else "NULL"])]
        [(integer-type? type)  (guard type value integer?  "(U integer #f)")  (number->string value)]
        [(real-type? type)     (guard type value real?     "(U real #f)")     (number->string value)]
        [(string-type? type)   (guard type value string?   "(U string #f)")   (string-append "'" (regexp-replace* #rx"'" value "''") "'")]
        [(symbol-type? type)   (guard type value symbol?   "(U symbol #f)")   (string-append "'" (regexp-replace* #rx"'" (symbol->string value) "''") "'")]
        [(time-tai-type? type) (guard type value time-tai? "(U time-tai #f)") (escape-time time-tai value)]
        [(time-utc-type? type) (guard type value time-utc? "(U time-utc #f)") (escape-time time-utc value)]
        [else                  (raise-type-error #f "unrecognised type" type)]))

; snooze type string -> any
(define (parse-value snooze type value)
  (with-handlers ([exn? (lambda (exn) (raise-exn exn:fail:contract (exn-message exn)))])
    (cond [(guid-type? type)     (send (send snooze get-guid-cache) get-interned-guid (guid-type-entity type) (inexact->exact value))]
          [(boolean-type? type)  (equal? value "1")]
          [(not value)           #f]
          [(integer-type? type)  (inexact->exact (string->number value))]
          [(real-type? type)     (string->number value)]
          [(string-type? type)   value]
          [(symbol-type? type)   (string->symbol value)]
          [(time-tai-type? type) (parse-time time-tai value)]
          [(time-utc-type? type) (parse-time time-utc value)]
          [else                  (raise-exn exn:fail:contract (format "Unrecognised type: ~s" type))])))

; snooze (listof type) -> ((U (listof database-value) #f) -> (U (listof scheme-value) #f))
(define (make-parser snooze types)
  (let ([parse/snooze (cut parse-value snooze <> <>)])
    (lambda (vals)
      (and vals (map parse/snooze types vals)))))

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
    [(guard type value predicate expected)
     (unless (predicate value)
       (raise-type-error (type-name type) expected value))]))
