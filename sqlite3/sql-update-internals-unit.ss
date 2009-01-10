(module sql-update-internals-unit mzscheme
  
  (require (lib "unitsig.ss")
           (lib "string.ss" "srfi" "13"))
  
  (require (file "../era.ss")
           (file "../test-base.ss")
           (file "../type.ss")
           (file "../generic/sql-sig.ss")
           (prefix generic: (file "../generic/sql-update-internals-unit.ss"))
           (file "sql-quote-unit.ss"))
  
  (provide sql-update-internals@)
  
  ;; unit sql-update-internals@ : sql-quote^ -> sql-update-internals^
  (define sql-update-internals@
    (let ([mixin@
           (unit/sig sql-update-internals^
             (import (generic : sql-update-internals^)
                     sql-quote^)
             
             ;; create-field-sql : symbol type -> string
             (define (create-field-sql name type)
               (cond [(eq? type type:text)     (string-append (quote-id name) " TEXT")]
                 [(eq? type type:integer)  (string-append (quote-id name) " INTEGER")]
                 [(eq? type type:real)     (string-append (quote-id name) " REAL")]
                 [(eq? type type:symbol)   (string-append (quote-id name) " TEXT")]
                 [(eq? type type:boolean)  (string-append (quote-id name) " INTEGER")]
                 [(eq? type type:time-tai) (string-append (quote-id name) " REAL")]
                 [else (raise-exn exn:fail:snooze
                         (format "Unrecognised field type: ~a" type))]))
             
             (define column-names generic:column-names)
             
             (define column-values generic:column-values)
             
             )])
      
      (compound-unit/sig
          (import (quote : sql-quote^))
        (link (original  : sql-update-internals^ (generic:sql-update-internals@ quote))
              (variation : sql-update-internals^ (mixin@ original quote)))
        (export (open variation)))))
  
  )