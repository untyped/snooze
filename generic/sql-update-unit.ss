(module sql-update-unit mzscheme
  
  (require (lib "unitsig.ss")
           (lib "list.ss"   "srfi" "1")
           (lib "string.ss" "srfi" "13"))
  
  (require (file "../base.ss")
           (file "../era.ss")
           (file "../type.ss")
           (file "sql-sig.ss"))
  
  (provide sql-update@)
  
  ;; unit sql-update@ : sql-quote^ -> sql-update^
  (define sql-update@
    (unit/sig sql-update^
      (import sql-update-internals^ sql-quote^)
      
      ;; create-sql : entity -> string
      (define (create-sql entity)
        (string-append
         "CREATE TABLE " (quote-id (entity-name entity)) " ("
         (string-join 
          (cons*
           (string-append (quote-id 'id) " INTEGER PRIMARY KEY")
           (string-append (quote-id 'revision) " INTEGER NOT NULL DEFAULT 0")
           (map-attributes/entity 
            create-field-sql 
            entity)) 
          ", ")
         ");"))
      
      ;; drop-sql : entity -> string
      (define (drop-sql entity)
        (string-append "DROP TABLE " (quote-id (entity-name entity)) ";"))
      
      ;; insert-sql : persistent-struct -> string
      (define (insert-sql struct)
        (if (get-id struct)
            (raise-exn exn:fail:snooze
              (format "ID must be null to perform INSERT: ~a" struct))
            (let* ([entity (struct-entity struct)]
                   [name (entity-name entity)]
                   [column-names (string-join (cddr (column-names entity)) ", ")]    
                   [column-values (string-join (cddr (column-values struct)) ", ")])
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