#lang scheme/unit

(require mzlib/etc
         srfi/13/string)

(require "../base.ss"
         "../era/era.ss"
         "../generic/sql-data-sig.ss"
         "../generic/sql-name-sig.ss"
         "../generic/sql-update-helpers-sig.ss"
         "../generic/sql-update-sig.ss")

(import sql-data^
        sql-name^)

(export sql-update^
        sql-update-helpers^)

; entity -> string
(define (create-sql entity)
  (string-append "CREATE TABLE " (escape-name (entity-table-name entity)) 
                 " (" (create-fields-sql entity) ");"))

; (U entity symbol) -> string
(define (drop-sql table)
  (define table-name
    (cond [(entity? table) (entity-table-name table)]
          [(symbol? table) table]
          [else            (raise-exn exn:fail:snooze
                             (format "Expceted (U entity symbol), received ~s" table))]))
  (format "DROP TABLE IF EXISTS ~a;"  (escape-name table-name)))

; snooze-struct -> string
(define (insert-sql struct [preserve-guids? #f])
  (car (insert-multiple-sql (list struct) preserve-guids?)))

; (listof snooze-struct) -> (listof string)
(define (insert-multiple-sql structs [preserve-guids? #f])
  (cond [(not (list? structs))
         (raise-exn exn:fail:contract
           (format "Expected (listof snooze-struct), received ~s" structs))]
        [(null? structs) null]
        [else
         (begin-with-definitions
           ; entity
           (define entity (struct-entity (car structs)))
           
           ; void
           ;
           ; Checks all the structs are unsaved and of the same type.
           ; Raises exn:fail:snooze if they aren't.
           (check-insert-structs entity structs preserve-guids?)
           
           ; symbol
           (define table-name 
             (escape-name (entity-table-name entity)))
           
           ; string ; cddr skips id and revision
           (define col-names
             (string-join (if preserve-guids?
                              (column-names entity)
                              (cddr (column-names entity)))
                          ", "))
           
           ; string
           (define prefix
             (string-append "INSERT INTO " table-name " (" col-names ") VALUES ("))
           
           ; string
           (define suffix
             ");")
           
           ; (listof string)
           ;
           ; One string per SQL statement. SQLite doesn't support multiple inserts in one statement,
           ; so we do it in multiple statements.
           (map (lambda (struct)
                  ; string
                  ;
                  ; cddr skips the id and revision attributes
                  (string-append prefix (string-join (if preserve-guids?
                                                         (column-values struct)
                                                         (cddr (column-values struct)))
                                                     ", ") suffix))
                structs))]))

; snooze-struct -> string
(define (update-sql struct)
  (define entity (struct-entity struct))
  (if (struct-saved? struct)
      (string-append "UPDATE " (escape-name (entity-table-name entity)) " SET "
                     (string-join (map (lambda (attr value)
                                         (define name (attribute-column-name attr))
                                         (define type (attribute-type attr))
                                         (string-append (escape-name name) " = " (escape-value type value)))
                                       (cdr (entity-attributes entity))
                                       (cdr (snooze-struct-ref* struct)))
                                  ", ")
                     " WHERE " (escape-name 'id) " = " (escape-value type:integer (guid-id (struct-guid struct))) ";")
      (error "struct not in database" struct)))

; guid -> string
(define (delete-sql guid)
  (define name (entity-table-name (guid-entity guid)))
  (string-append "DELETE FROM " (escape-name name) " WHERE " (escape-name 'id) " = " (escape-value type:integer (guid-id guid)) ";"))

; entity (listof snooze-struct) -> void | exn:fail:snooze
;
; Checks all structs are unsaved and of the specified type.
(define (check-insert-structs entity structs [skip-guid-checks? #f])
  (for-each (lambda (struct)
              ; void | exn:fail:snooze
              (unless (eq? entity (struct-entity struct))
                (raise-exn exn:fail:snooze
                  (format "All structs must be of the same type: ~s" structs)))
              ; void | exn:fail:snooze
              (when (and (struct-guid struct) (not skip-guid-checks?))
                (error "struct already in database" struct)))
            structs))


; entity -> string
(define (create-fields-sql entity)
  (string-join (list* (string-append (escape-name 'id) " INTEGER PRIMARY KEY")
                      (string-append (escape-name 'revision) " INTEGER NOT NULL DEFAULT 0")
                      (map attribute-definition-sql (cddr (entity-attributes entity))))
               ", "))

; entity -> (listof string)
(define (column-names entity)
  (map (lambda (attr)
         (escape-name (attribute-column-name attr)))
       (entity-attributes entity)))

; snooze-struct -> string
(define (column-values struct)
  (define entity (struct-entity struct))
  (map (lambda (attr value)
         (escape-value (attribute-type attr) value))
       (entity-attributes entity)
       (snooze-struct-ref* struct))) 

; attribute -> string
(define (attribute-definition-sql attr)
  (let ([type (attribute-type attr)]
        [name (attribute-column-name attr)])
    (string-join (append (list (escape-name name)
                               (cond [(guid-type? type)     (format "INTEGER REFERENCES ~a.~a" 
                                                                    (escape-name (entity-name (guid-type-entity type)))
                                                                    (escape-name 'id))]
                                     [(boolean-type? type)  "INTEGER"]
                                     [(integer-type? type)  "INTEGER"]
                                     [(real-type? type)     "REAL"]
                                     [(string-type? type)   (string-type-definition-sql (string-type-max-length type))]
                                     [(symbol-type? type)   (string-type-definition-sql (symbol-type-max-length type))]
                                     [(time-tai-type? type) "INTEGER"]
                                     [(time-utc-type? type) "INTEGER"]
                                     [else                  (raise-exn exn:fail:snooze (format "Unrecognised type: ~a" type))]))
                         (if (type-allows-null? type)
                             null
                             (list "NOT NULL"))
                         (list (string-append "DEFAULT " (escape-value type (attribute-default attr)))))
                 " ")))

; (U integer #f) -> string
(define (string-type-definition-sql max-length)
  (if max-length
      (format "CHARACTER VARYING (~a)" max-length)
      "TEXT"))
