(module sql-quote-unit mzscheme
  
  (require (lib "etc.ss")
           (lib "unitsig.ss")
           (lib "list.ss"   "srfi" "1")
           (lib "string.ss" "srfi" "13")
           (lib "vector-lib.ss" "srfi" "43"))
  
  (require (file "../base.ss")
           (file "../era.ss")
           (file "../generic/sql-sig.ss")
           (prefix generic: (file "../generic/sql-quote-unit.ss")))
  
  (provide sql-quote@)
  
  ;; Provides a DBMS specific unit for quoting/unquoting data primitives. See:
  ;;
  ;;     ../sql-sig.ss
  ;;     ../generic/sql-quote.ss
  ;;
  ;; for more information.
  
  (define sql-quote@
    (let ([mixin@
           (unit/sig sql-quote^
             (import (generic : sql-quote^))
             
             ;; quote-id : (U symbol string) -> string
             ;;
             ;; Quotes a field or table name. In SQLite's case this is 
             ;; done by wrapping the name in square brackets
             (define (quote-id identifier)
               (if (symbol? identifier)
                   (string-append "[" (symbol->string identifier) "]")
                   (string-append "[" identifier "]")))
             
             ;; quote-data type any -> string
             (define quote-data generic:quote-data)
             
             ;; unquote-data : type string -> any
             (define unquote-data generic:unquote-data)
             
             ;; make-data-unquoter
             ;;     : (list-of type)
             ;;    -> ((U (vector-of (U string #f)) #f) -> (U (vector-of scheme-primitive) #f))
             ;; 
             ;; Creates a procedure that "unquotes" (parses) a vector of data retrieved from a database.
             (define (make-data-unquoter types)
               (let ([types (list->vector types)])
                 (lambda (source)
                   (if source
                       (vector-map (lambda (index type val)
                                     (unquote-data type val))
                                   types
                                   source)
                       #f))))
             
             )])
      
      (compound-unit/sig
        (import)
        (link (original  : sql-quote^ (generic:sql-quote@))
              (variation : sql-quote^ (mixin@ original)))
        (export (open variation)))))
  
  )
 