#lang scheme/base

(require "../base.ss")

(require scheme/serialize
         scheme/string
         (only-in srfi/13 string-pad)
         srfi/19
         "../spgsql-hacked/spgsql.ss"
         (unlib-in symbol)
         "../core/struct.ss"
         "../core/snooze-struct.ss"
         "../common/common.ss")

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
      
      ; string -> string
      (define (date-to-string str)
        (string-append "'" (string-pad (date->string str "~Y-~m-~d ~H:~M:~S.~N") 29 #\0) "'"))
      
      (cond [(boolean-type? type)  (guard type value boolean? "boolean")
                                   (if value "true" "false")]
            [(not value)           "NULL"]
            [(guid-type? type)     (guard type value guid+snooze-struct? "(U guid snooze-struct #f)")
                                   (escape-guid type value)]
            [(integer-type? type)  (guard type value integer? "(U integer #f)")
                                   (number->string value)]
            [(real-type? type)     (guard type value real? "(U real #f)")
                                   (number->string value)]
            [(string-type? type)   (guard type value string? "(U string #f)")
                                   (string-append "'" (regexp-replace* #rx"'" value "''") "'")]
            [(symbol-type? type)   (guard type value symbol? "(U symbol #f)")
                                   (string-append "'" (regexp-replace* #rx"'" (symbol->string value) "''") "'")]
            [(time-tai-type? type) (guard type value time-tai? "(U time-tai #f)")
                                   (date-to-string (time-tai->date value 0))]
            [(time-utc-type? type) (guard type value time-utc? "(U time-utc #f)")
                                   (date-to-string (time-utc->date value 0))]
            [(binary-type? type)   (guard type value serializable? "serializable")
                                   (format-sql "~a" [bytea (serialize/bytes value)])]
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
           [(? temporal-type?)  " TIMESTAMP WITHOUT TIME ZONE"]
           [(? binary-type?)    " BYTEA"])
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
      (let* ([include-id? (and (database-guid? (snooze-struct-guid struct)) #t)]
             [entity      (snooze-struct-entity struct)]
             [attrs       (entity-attributes entity)]
             [vals        (snooze-struct-raw-ref* struct)]
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
                                [val  (in-list (snooze-struct-raw-ref* struct))])
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
    ; This is factored out as a procedure because it increases the speed of the map in make-parser by 5-50%.
    (define (private-parse-value type val)
      (with-handlers ([exn? (lambda (exn) (raise-exn exn:fail:contract (exn-message exn)))])
        (cond [(sql-null? val)     #f]
              [(guid-type? type)     (entity-make-guid (guid-type-entity type) (inexact->exact val))]
              [(boolean-type? type)  val]
              [(integer-type? type)  (inexact->exact val)]
              [(real-type? type)     val]
              [(string-type? type)   val]
              [(symbol-type? type)   (string->symbol val)]
              [(time-tai-type? type) (date->time-tai (sql-datetime->srfi-date val))]
              [(time-utc-type? type) (date->time-utc (sql-datetime->srfi-date val))]
              [(binary-type? type)   (deserialize/bytes val)]
              [else                  (raise-exn exn:fail:snooze (format "unrecognised type: ~a" type))])))
    
    ; type (U string sql-null) -> any
    (define/public (parse-value type val)
      (private-parse-value type val))
    
    ; (listof type) -> ((U (listof database-value) #f) -> (U (listof scheme-value) #f))
    (define/public (make-parser types)
      (lambda (vals)
        (and vals (map private-parse-value types vals))))))

; Helpers --------------------------------------

; (guard any (any -> boolean) string)
(define-syntax guard
  (syntax-rules ()
    [(guard type value predicate expected)
     (unless (predicate value)
       (raise-type-error (type-name type) expected value))]))

; any -> boolean
(define (guid+snooze-struct? val)
  (or (guid? val)
      (snooze-struct? val)))

; any -> bytes
(define (serialize/bytes val)
  (let ([out (open-output-bytes)])
    (write (serialize val) out)
    (get-output-bytes out)))

; bytes -> any
(define (deserialize/bytes val)
  (let ([in (open-input-bytes val)])
    (deserialize (read in))))

; Provide statements ---------------------------

(provide postgresql8-sql-mixin)
