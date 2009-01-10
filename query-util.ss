(module query-util mzscheme
  
  (require (file "base.ss")
           (prefix era: (file "era.ss"))
           (file "query-core.ss")
           (prefix q: (file "query-lang.ss")))
  
  ; let-entities ---------------------------------
  
  ; In the following contract definitions:
  ;
  ;     what-item : (U field aggregate table entity select)
  
  ;; let-alias-item : symbol (U era:entity symbol what-item) -> what-item
  (define (let-alias-item name item)
    (cond [(era:entity? item)
           (q:entity name item)]
          [(symbol? item)
           (q:table name item)]
          [(or (field? item) (aggregate? item) (table? item) (entity? item) (select? item))
           item]
          [else (raise-exn exn:fail:snooze
                  (format "let-alias: ~a: expected (U era-entity table-name-symbol any-what-item), received ~a."
                          name
                          item))]))
  
  ;; syntax (let-alias ([identifier (U era:entity symbol what-item)] ...) statement ...)
  (define-syntax (let-alias stx)
    (syntax-case stx ()
      [(_ ([identifier item] ...) stmt ...)
       #'(let ([identifier (let-alias-item 'identifier item)] ...)
           stmt ...)]))
  
  ;; syntax (let*-alias ([identifier (U era:entity symbol what-item)] ...) statement ...)
  (define-syntax (let*-alias stx)
    (syntax-case stx ()
      [(_ ([identifier item] ...) stmt ...)
       #'(let* ([identifier (let-alias-item 'identifier item)] ...)
           stmt ...)]))

  ; Provide statements --------------------------- 
  
  (provide let-alias
           let*-alias)
  
  )