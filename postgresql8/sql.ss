#lang scheme/base

(require "../base.ss")

(require scheme/string
         srfi/19
         (spgsql-in spgsql)
         (unlib-in symbol)
         "../era/core.ss"
         "../era/snooze-struct.ss"
         "../generic/generic.ss")

(define postgresql8-sql-mixin
  (mixin (generic-database<%>) (sql-escape<%> parse<%> sql-create<%> sql-drop<%> sql-insert<%> sql-update<%> sql-delete<%>)

    (inspect #f)
    
    (inherit get-snooze)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; symbol -> string
    (define/public (escape-sql-name name)
      (string-append "\"" (symbol->string name) "\""))
    
    ; type any -> string
    (define/public (escape-sql-value type value)
      (cond [(boolean-type? type)  (guard type value boolean? "boolean")          (if value "true" "false")]
            [(not value)           "NULL"]
            [(guid-type? type)     (guard type value guid? "(U guid #f)")         (cond [(not (eq? (guid-entity value) (guid-type-entity type)))
                                                                                         (raise-exn exn:fail:snooze:query
                                                                                           (format "wrong guid entity: expected ~a, received ~a."
                                                                                                   (entity-name (guid-entity value))
                                                                                                   (entity-name (guid-type-entity type))))]
                                                                                        [(guid-id value) => number->string]
                                                                                        [else (raise-exn exn:fail:snooze:query
                                                                                                (format "cannot use unsaved struct in a query: ~s" value)
                                                                                                #f)])]
            [(integer-type? type)  (guard type value integer?  "(U integer #f)")  (number->string value)]
            [(real-type? type)     (guard type value real?     "(U real #f)")     (number->string value)]
            [(string-type? type)   (guard type value string?   "(U string #f)")   (string-append "'" (regexp-replace* #rx"'" value "''") "'")]
            [(symbol-type? type)   (guard type value symbol?   "(U symbol #f)")   (string-append "'" (regexp-replace* #rx"'" (symbol->string value) "''") "'")]
            [(time-tai-type? type) (guard type value time-tai? "(U time-tai #f)") (date->string (time-tai->date value 0) "'~Y-~m-~d ~H:~M:~S.~N'")]
            [(time-utc-type? type) (guard type value time-utc? "(U time-utc #f)") (date->string (time-utc->date value 0) "'~Y-~m-~d ~H:~M:~S.~N'")]
            [else                  (raise-type-error #f "unrecognised type" type)]))
    
    ; entity -> string
    (define/public (create-table-sql entity)
      (let* ([table-name    (entity-table-name entity)]
             [sequence-name (symbol-append table-name '_seq)])
        (format "CREATE SEQUENCE ~a; CREATE TABLE ~a (~a);"
                (escape-sql-name sequence-name)
                (escape-sql-name table-name)
                (string-join (list* (string-append (escape-sql-name 'guid) " INTEGER PRIMARY KEY DEFAULT nextval('" (escape-sql-name sequence-name) "')")
                                    (string-append (escape-sql-name 'revision) "INTEGER NOT NULL DEFAULT 0")
                                    (map (cut column-definition-sql <>) (cddr (entity-attributes entity))))
                             ", "))))
    
    ; attribute -> string
    (define/public (column-definition-sql attr)
      (let ([type (attribute-type attr)]
            [name (attribute-column-name attr)])
        (string-append
         (escape-sql-name name)
         (match type
           [(? guid-type?)      (format " INTEGER REFERENCES ~a(~a)"
                                        (escape-sql-name (entity-table-name (guid-type-entity type)))
                                        (escape-sql-name (attribute-column-name (car (entity-attributes (guid-type-entity type))))))]
           [(? boolean-type?)   " BOOLEAN"]
           [(? integer-type?)   " INTEGER"]
           [(? real-type?)      " REAL"]
           [(? character-type?) (if (character-type-max-length type)
                                    (format " CHARACTER VARYING (~a)" (character-type-max-length type))
                                    " TEXT")]
           [(? temporal-type?)  " TIMESTAMP WITHOUT TIME ZONE"])
         (if (type-allows-null? type) "" " NOT NULL")
         (string-append " DEFAULT " (escape-sql-value type (attribute-default attr))))))
    
    ; (U entity symbol) -> string
    (define/public (drop-table-sql table)
      (let* ([table-name    (match table
                              [(? entity?) (entity-table-name table)]
                              [(? symbol?) table])]
             [sequence-name (symbol-append table-name '_seq)])
        (format "DROP TABLE IF EXISTS ~a; DROP SEQUENCE IF EXISTS ~a;"
                (escape-sql-name table-name)
                (escape-sql-name sequence-name))))
    
    ; snooze-struct -> string
    (define/public (insert-sql struct)
      (let* ([include-id? (and (snooze-struct-guid struct) #t)]
             [entity      (snooze-struct-entity struct)]
             [attrs       (entity-attributes entity)]
             [vals        (snooze-struct-ref* struct)]
             [table-name  (escape-sql-name (entity-table-name entity))]
             [col-names   (string-join (for/list ([attr (in-list (if include-id? attrs (cddr attrs)))])
                                         (escape-sql-name (attribute-column-name attr)))
                                       ", ")]
             [col-values  (string-join (for/list ([attr (in-list (if include-id? attrs (cddr attrs)))]
                                                  [val  (in-list (if include-id? vals  (cddr vals)))])
                                         (escape-sql-value (attribute-type attr) val))
                                       ", ")])
        (format "INSERT INTO ~a (~a) VALUES (~a);" table-name col-names col-values)))
    
    ; snooze-struct -> string
    (define/public (update-sql struct)
      (let* ([entity (snooze-struct-entity struct)]
             [exprs  (for/list ([attr (in-list (entity-attributes entity))]
                                [val  (in-list (snooze-struct-ref* struct))])
                       (string-append (escape-sql-name (attribute-column-name attr))
                                      " = "
                                      (escape-sql-value (attribute-type attr) val)))])
        (if (snooze-struct-saved? struct)
            (format "UPDATE ~a SET ~a WHERE ~a;"
                    (escape-sql-name (entity-table-name entity))
                    (string-join (cdr exprs) ", ")
                    (car exprs))
            (error "struct not in database" struct))))
    
    ; guid -> string
    (define/public (delete-sql guid)
      (let* ([entity (guid-entity guid)]
             [table  (entity-table-name entity)]
             [attr   (car (entity-attributes entity))]
             [id     (guid-id guid)])
        (format "DELETE FROM ~a WHERE ~a = ~a;"
                (escape-sql-name table)
                (escape-sql-name (attribute-column-name attr))
                (escape-sql-value (attribute-type attr) guid))))
    
    ; type (U string sql-null) -> any
    (define/public (parse-value type value)
      (with-handlers ([exn? (lambda (exn) (raise-exn exn:fail:contract (exn-message exn)))])
        (cond [(guid-type? type)     (entity-make-vanilla-guid #:snooze (get-snooze) (guid-type-entity type) (inexact->exact value))]
              [(sql-null? value)     #f]
              [(boolean-type? type)  value]
              [(integer-type? type)  (inexact->exact value)]
              [(real-type? type)     value]
              [(string-type? type)   value]
              [(symbol-type? type)   (string->symbol value)]
              [(time-tai-type? type) (date->time-tai (sql-datetime->srfi-date value))]
              [(time-utc-type? type) (date->time-utc (sql-datetime->srfi-date value))]
              [else                  (raise-exn exn:fail:snooze (format "unrecognised type: ~a" type))])))
    
    ; (listof type) -> ((U (listof database-value) #f) -> (U (listof scheme-value) #f))
    (define/public (make-parser types)
      (lambda (vals)
        (and vals (map (cut parse-value <> <>) types vals))))))

; Helpers --------------------------------------

; (guard any (any -> boolean) string)
(define-syntax guard
  (syntax-rules ()
    [(guard type value predicate expected)
     (unless (predicate value)
       (raise-type-error (type-name type) expected value))]))

; Provide statements ---------------------------

(provide postgresql8-sql-mixin)
