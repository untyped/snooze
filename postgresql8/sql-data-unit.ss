#lang scheme/unit

(require srfi/13
         srfi/19
         (only-in srfi/43 vector-map)
         (planet schematics/spgsql:2/spgsql)
         (planet untyped/unlib:3/time)
         "../base.ss"
         "../era/era.ss"
         "../generic/sql-data-sig.ss")

(import)

(export sql-data^)

; type any -> string
(define (escape-value type value)
  (cond [(boolean-type? type)  (guard value boolean? "boolean")          (if value "true" "false")]
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
        [(time-tai-type? type) (guard value time-tai? "(U time-tai #f)") (date->string (time-tai->date value 0) "'~Y-~m-~d ~H:~M:~S.~N'")]
        [(time-utc-type? type) (guard value time-utc? "(U time-utc #f)") (date->string (time-utc->date value 0) "'~Y-~m-~d ~H:~M:~S.~N'")]
        [else                  (raise-exn exn:fail:contract (format "Unrecognised type: ~s" type))]))

; type (U string sql-null) -> any
(define (parse-value type value)
  (with-handlers ([exn? (lambda (exn) (raise-exn exn:fail:contract (exn-message exn)))])
    (cond [(guid-type? type)     (let ([id (inexact->exact value)])
                                   (and id (make-guid (guid-type-entity type) id)))]
          [(sql-null? value)     #f]
          [(boolean-type? type)  value]
          [(integer-type? type)  (inexact->exact value)]
          [(real-type? type)     value]
          [(string-type? type)   value]
          [(symbol-type? type)   (string->symbol value)]
          [(time-tai-type? type) (date->time-tai (sql-datetime->srfi-date value))]
          [(time-utc-type? type) (date->time-utc (sql-datetime->srfi-date value))]
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

; string -> string
(define (escape-string str)
  (regexp-replace* #rx"'" str "''"))

; (guard any (any -> boolean) string)
(define-syntax guard
  (syntax-rules ()
    [(guard value predicate message)
     (unless (predicate value)
       (raise-exn exn:fail:contract
         (format "Could not escape value for SQL: expected ~a, received ~s." message value)))]))
