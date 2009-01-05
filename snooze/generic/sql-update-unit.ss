(module sql-update-unit mzscheme
  
  (require (lib "plt-match.ss")
           (lib "unitsig.ss")
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
         "CREATE TABLE " (quote-id (entity-name entity)) " (" (create-fields-sql entity) ");"))
      
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
               [id       (get-id struct)])
          (if id
              (string-append
               "UPDATE " (quote-id (entity-name entity)) " SET "
               (string-join (map (match-lambda*
                                   [(list (struct attribute (name type)) val)
                                    (string-append (quote-id name) " = " (quote-data type val))])
                                 (cdr (entity-fields (struct-entity struct)))
                                 (cdr (get-attribute-values struct)))
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