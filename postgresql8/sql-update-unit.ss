(module sql-update-unit mzscheme
  
  (require (lib "unitsig.ss")
           (lib "list.ss"   "srfi" "1")
           (lib "string.ss" "srfi" "13"))
  
  (require (planet "symbol.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "../base.ss")
           (file "../era.ss")
           (file "../type.ss")
           (file "../generic/sql-sig.ss"))
  
  (provide sql-update@)
  
  ;; unit sql-update@ : sql-quote^ -> sql-update^
  (define sql-update@
    (unit/sig sql-update^
      (import sql-update-internals^ sql-quote^)
      
      ;; create-sql : entity -> string
      (define (create-sql entity)
        (let* ([table-name (entity-name entity)]
               [sequence-name (symbol-append table-name '-seq)])
          (format "CREATE SEQUENCE ~a; CREATE TABLE ~a (~a);"
                  (quote-id sequence-name)
                  (quote-id table-name)
                  (string-join 
                   (cons* (format "~a INTEGER PRIMARY KEY DEFAULT nextval('~a')" 
                                  (quote-id 'id)
                                  (quote-id sequence-name))
                          (format "~a INTEGER NOT NULL DEFAULT 0"
                                  (quote-id 'revision))
                          (map-attributes/entity create-field-sql entity))
                   ", "))))
      
      ;; drop-sql : entity -> string
      (define (drop-sql entity)
        (let* ([table-name (entity-name entity)]
               [sequence-name (symbol-append table-name '-seq)])
          (format "DROP TABLE ~a; DROP SEQUENCE ~a;"
                  (quote-id table-name)
                  (quote-id sequence-name))))
      
      ;; insert-sql : persistent-struct -> string
      (define (insert-sql struct)
        (if (get-id struct)
            (raise-exn exn:fail:snooze
              (format "ID must be null to perform INSERT: ~a" struct))
            (let* ([entity (struct-entity struct)]
                   [name (entity-name entity)]
                   ; We don't insert an ID explicitly: we rely on the SEQUENCE
                   ; to auto-generate one, and on the fact that we're in a transaction
                   ; to allow (insert-record ...) to perform a subsequent
                   ; "SELECT currval('sequence');" to retrieve it.
                   [column-names  (string-join (cddr (column-names entity)) ", ")]   ; cddr skips id and revision
                   [column-values (string-join (cddr (column-values struct)) ", ")]) ; cddr skips id and revision
              (string-append "INSERT INTO " (quote-id name) " (" column-names ") VALUES (" column-values ");"))))
      
      ;; update-sql : persistent-struct -> string
      (define (update-sql struct)
        (let* ([entity   (struct-entity struct)]
               [id       (get-id struct)]
               [revision (get-revision struct)])
          (if id
              (string-append
               "UPDATE " (quote-id (entity-name entity)) " SET "
               (string-join
                (cons (string-append (quote-id 'revision) " = " (quote-data type:revision revision))
                      (map-attributes/struct
                       (lambda (name type val)
                         (string-append
                          (quote-id name) " = " (quote-data type val)))
                       struct))
                ", ")
               " WHERE " (quote-id 'id) " = " (quote-data type:id id) ";")
              (raise-exn exn:fail:snooze
                "Cannot update an entity with no ID.\n"))))
      
      ;; delete-sql : entity integer -> string
      (define (delete-sql entity id)
        (let ([name (entity-name entity)])
          (string-append
           "DELETE FROM " (quote-id name) " WHERE " (quote-id 'id) " = " (quote-data type:id id) ";")))
      
      ))
  
  )