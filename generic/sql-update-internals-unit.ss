(module sql-update-internals-unit mzscheme
  
  (require (lib "plt-match.ss")
           (lib "unitsig.ss")
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
      
      ;; create-fields-sql : entity -> string
      (define (create-fields-sql entity)
        (string-join (map (match-lambda
                            [(struct attribute (name (app type-base base)))
                             (cond [(eq? base type:id)       (string-append (quote-id name) " INTEGER PRIMARY KEY")]
                                   [(eq? base type:revision) (string-append (quote-id name) " INTEGER NOT NULL DEFAULT 0")]
                                   [(eq? base type:text)     (string-append (quote-id name) " TEXT")]
                                   [(eq? base type:integer)  (string-append (quote-id name) " INTEGER")]
                                   [(eq? base type:real)     (string-append (quote-id name) " REAL")]
                                   [(eq? base type:symbol)   (string-append (quote-id name) " TEXT")]
                                   [(eq? base type:boolean)  (string-append (quote-id name) " INTEGER")]
                                   [(eq? base type:time-tai) (string-append (quote-id name) " INTEGER")]
                                   [else                     (raise-exn exn:fail:snooze
                                                               (format "Unrecognised field type: ~a" base))])])
                          (entity-fields entity))
                     ", "))
      
      ;; column-names : entity -> (list-of string)
      ;;
      ;; Given an entity returns a list of column names suitable for 
      ;; use in INSERT and SELECT statements
      (define (column-names entity)
        (map (match-lambda
               [(struct attribute (name type))
                (quote-id name)])
             (entity-fields entity)))
      
      ;; column-values : persistent-struct -> string
      ;;
      ;; Given an entity and a structure returns a list of
      ;; comma separated quoted values suitable for use in INSERT
      ;; statements
      (define (column-values struct)
        (map (match-lambda*
               [(list (struct attribute (name type)) val)
                (quote-data type val)])
             (entity-fields (struct-entity struct))
             (get-attribute-values struct)))
      
      ))
  
  )