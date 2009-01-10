(module sql-update-internals-unit mzscheme
  
  (require (lib "unitsig.ss")
           (lib "list.ss"   "srfi" "1")
           (lib "string.ss" "srfi" "13"))
  
  (require (file "../base.ss")
           (file "../era.ss")
           (file "../type.ss")
           (file "sql-sig.ss"))
  
  (provide sql-update-internals@)
  
  ;; unit sql-update-internals@ : sql-quote^ -> sql-update-internals^
  (define sql-update-internals@
    (unit/sig sql-update-internals^
      (import sql-quote^)
      
      ;; create-field-sql : symbol type -> string
      (define (create-field-sql name type)
        (cond [(eq? type type:text)     (string-append (quote-id name) " TEXT")]
              [(eq? type type:integer)  (string-append (quote-id name) " INTEGER")]
              [(eq? type type:real)     (string-append (quote-id name) " REAL")]
              [(eq? type type:symbol)   (string-append (quote-id name) " TEXT")]
              [(eq? type type:boolean)  (string-append (quote-id name) " INTEGER")]
              [(eq? type type:time-tai) (string-append (quote-id name) " INTEGER")]
              [else (raise-exn exn:fail:snooze
                      (format "Unrecognised field type: ~a" type))]))
      
      ;; column-names : entity -> (list-of string)
      ;;
      ;; Given an entity returns a list of column names suitable for 
      ;; use in INSERT and SELECT statements
      (define (column-names entity)
        (cons*
         (quote-id 'id)
         (quote-id 'revision)
         (map-attributes/entity
          (lambda (name type)
            (quote-id name))
          entity)))
      
      ;; column-values : persistent-struct -> string
      ;;
      ;; Given an entity and a structure returns a list of
      ;; comma separated quoted values suitable for use in INSERT
      ;; statements
      (define (column-values struct)
        (cons*
         (quote-data type:id (get-id struct))
         (quote-data type:revision (get-revision struct))
         (map-attributes/struct
          (lambda (name type val)
            (quote-data type val))
          struct)))
      
      ))
  
  )