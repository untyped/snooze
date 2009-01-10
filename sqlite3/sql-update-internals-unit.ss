(module sql-update-internals-unit mzscheme
  
  (require (lib "plt-match.ss")
           (lib "unitsig.ss")
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
             
             (define column-names generic:column-names)
             
             (define column-values generic:column-values)
             
             )])
      
      (compound-unit/sig
        (import (quote : sql-quote^))
        (link (original  : sql-update-internals^ (generic:sql-update-internals@ quote))
              (variation : sql-update-internals^ (mixin@ original quote)))
        (export (open variation)))))
  
  )