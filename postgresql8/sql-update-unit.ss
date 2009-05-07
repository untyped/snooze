#lang scheme/unit

(require mzlib/etc
         srfi/13/string
         (planet untyped/unlib:3/symbol)
         "../base.ss"
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
  (define table-name (entity-table-name entity))
  (define sequence-name (symbol-append table-name '_seq))
  (format "CREATE SEQUENCE ~a; CREATE TABLE ~a (~a);"
          (escape-name sequence-name)
          (escape-name table-name)
          (create-fields-sql entity)))

; entity -> string
(define (drop-sql table)
  (define table-name 
    (cond [(entity? table) (entity-table-name table)]
          [(symbol? table) table]
          [else            (raise-exn exn:fail:snooze
                             (format "Expceted (U entity symbol), received ~s" table))]))
  (define sequence-name
    (symbol-append table-name '_seq))
  (format "DROP TABLE IF EXISTS ~a; DROP SEQUENCE IF EXISTS ~a;"
          (escape-name table-name)
          (escape-name sequence-name)))

; snooze-struct -> string
(define (insert-sql struct [preserve-guids? #f])
  (car (insert-multiple-sql (list struct) preserve-guids?)))

; (listof snooze-struct) [boolean] -> (listof string)
; 
; We don't insert the ID or revision explicitly: we rely on the SEQUENCE
; to generate the ID and the DEFAULT VALUE of the revision field.
;
; We will always be in a transaction at this point, so (insert-record ...)
; can perform a subsequent "SELECT currval('sequence');" to retrieve the last ID
; inserted. From this value we can calculate the IDs of every inserted struct.
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
           (define col-values
             (string-join (map (lambda (struct)
                                 ; string ; cddr skips id and revision
                                 (string-append
                                  "(" (string-join (column-values struct preserve-guids?)
                                                   ", ")
                                  ")"))
                               structs)
                          ", "))
           
           ; (listof string) ; one string per SQL statement, and PostgreSQL can do it all in one statement
           (list (string-append "INSERT INTO " table-name " (" col-names ") VALUES " col-values ";")))]))

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

; Helpers --------------------------------------

; entity (listof snooze-struct) [boolean] -> void | exn:fail:snooze
;
; Checks all structs are unsaved and of the specified type.
(define (check-insert-structs entity structs [skip-guid-check? #f])
  (for-each (lambda (struct)
              ; void | exn:fail:snooze
              (unless (eq? entity (struct-entity struct))
                (raise-exn exn:fail:snooze
                  (format "All structs must be of the same type: ~s" structs)))
              ; void | exn:fail:snooze
              (when (and (struct-saved? struct) (not skip-guid-check?))
                (error "struct already in database" struct)))
            structs))

; entity -> string
(define (create-fields-sql entity)
  (define seq-name (symbol-append (entity-table-name entity) '_seq))
  (string-join (list* (string-append (escape-name 'id) " INTEGER PRIMARY KEY "
                                     "DEFAULT nextval('" (escape-name seq-name) "')")
                      (string-append (escape-name 'revision) "INTEGER NOT NULL DEFAULT 0")
                      (map attribute-definition-sql (cddr (entity-attributes entity))))
               ", "))

; entity -> (list-of string)
(define (column-names entity)
  (map (lambda (attr)
         (escape-name (attribute-column-name attr)))
       (entity-attributes entity)))

; snooze-struct boolean -> string
(define (column-values struct include-guid+revision?)
  (define entity (struct-entity struct))
  (map (lambda (attr value)
         (escape-value (attribute-type attr) value))
       (if include-guid+revision?
           (entity-attributes entity)
           (cddr (entity-attributes entity)))
       (if include-guid+revision?
           (snooze-struct-ref* struct)
           (cddr (snooze-struct-ref* struct)))))

; attribute -> string
(define (attribute-definition-sql attr)
  (let ([type (attribute-type attr)]
        [name (attribute-column-name attr)])
    (string-join (append (list (escape-name name)
                               (cond [(guid-type? type)     (format "INTEGER REFERENCES ~a(~a)"
                                                                    (escape-name (entity-table-name (guid-type-entity type)))
                                                                    (escape-name (attribute-column-name (car (entity-attributes (guid-type-entity type))))))]
                                     [(boolean-type? type)  "BOOLEAN"]
                                     [(integer-type? type)  "INTEGER"]
                                     [(real-type? type)     "REAL"]
                                     [(string-type? type)   (string-type-definition-sql (string-type-max-length type))]
                                     [(symbol-type? type)   (string-type-definition-sql (symbol-type-max-length type))]
                                     [(time-tai-type? type) "TIMESTAMP WITHOUT TIME ZONE"]
                                     [(time-utc-type? type) "TIMESTAMP WITHOUT TIME ZONE"]
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
