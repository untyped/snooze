#lang scheme/unit

(require "../base.ss")

(require srfi/13
         srfi/19
         (only-in srfi/43 vector-map)
         (planet schematics/spgsql:2/spgsql)
         (planet untyped/unlib:3/time)
         "../era/era.ss"
         "../generic/sql-data-sig.ss")

(import)

(export sql-data^)

; type any -> string
(define (escape-value type value)
  (cond [(boolean-type? type)  (guard type value boolean? "boolean")          (if value "true" "false")]
        [(not value)           "NULL"]
        [(guid-type? type)     (guard type value guid? "(U guid #f)")         (cond [(not (eq? (guid-entity value) (guid-type-entity type)))
                                                                                     (error (format "wrong guid entity: expected ~a, received ~a."
                                                                                                    (entity-name (guid-entity value))
                                                                                                    (entity-name (guid-type-entity type))))]
                                                                                    [(guid-id value) => number->string]
                                                                                    [else (error "reference to unsaved struct" value)])]
        [(integer-type? type)  (guard type value integer?  "(U integer #f)")  (number->string value)]
        [(real-type? type)     (guard type value real?     "(U real #f)")     (number->string value)]
        [(string-type? type)   (guard type value string?   "(U string #f)")   (string-append "'" (regexp-replace* #rx"'" value "''") "'")]
        [(symbol-type? type)   (guard type value symbol?   "(U symbol #f)")   (string-append "'" (regexp-replace* #rx"'" (symbol->string value) "''") "'")]
        [(time-tai-type? type) (guard type value time-tai? "(U time-tai #f)") (date->string (time-tai->date value 0) "'~Y-~m-~d ~H:~M:~S.~N'")]
        [(time-utc-type? type) (guard type value time-utc? "(U time-utc #f)") (date->string (time-utc->date value 0) "'~Y-~m-~d ~H:~M:~S.~N'")]
        [else                  (raise-type-error #f "unrecognised type" type)]))

; snooze-cache<%> type (U string sql-null) -> any
(define (parse-value snooze type value)
  (with-handlers ([exn? (lambda (exn) (raise-exn exn:fail:contract (exn-message exn)))])
    (cond [(guid-type? type)     (send (send snooze get-guid-cache) get-interned-guid (guid-type-entity type) (inexact->exact value))]
          [(sql-null? value)     #f]
          [(boolean-type? type)  value]
                [(integer-type? type)  (inexact->exact value)]
                [(real-type? type)     value]
                [(string-type? type)   value]
                [(symbol-type? type)   (string->symbol value)]
                [(time-tai-type? type) (date->time-tai (sql-datetime->srfi-date value))]
                [(time-utc-type? type) (date->time-utc (sql-datetime->srfi-date value))]
                [else                  (error "unrecognised type" type)])))

; (listof type) -> ((U (listof database-value) #f) -> (U (listof scheme-value) #f))
(define (make-parser snooze types)
  (let ([parse/snooze (cut parse-value snooze <> <>)])
    (lambda (vals)
      (and vals (map parse/snooze types vals)))))

; Helpers --------------------------------------

; string -> string
(define (escape-string str)
  (regexp-replace* #rx"'" str "''"))

; (guard any (any -> boolean) string)
(define-syntax guard
  (syntax-rules ()
    [(guard type value predicate expected)
     (unless (predicate value)
       (raise-type-error (type-name type) expected value))]))
