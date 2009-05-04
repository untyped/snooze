#lang scheme/base

(require scheme/match
         scheme/string
         srfi/26
         (planet untyped/unlib:3/syntax)
         "../base.ss"
         "../era/info.ss"
         (prefix-in sql: "sql-lang.ss")
         "sql-struct.ss"
         "sql-syntax-util.ss"
         (for-template scheme/base
                       (prefix-in sql: "sql-lang.ss")))

; Helpers ----------------------------------------

(define (or-expand message [backtrace null])
  (match-lambda*
    [(list)
     (lambda (stx)
       (raise-exn exn:fail:snooze
         (format "Expanding ~s: no match for rule ~s~n~a"
                 (syntax->datum stx)
                 message
                 (string-join (filter (lambda (item)
                                        (not (regexp-match #rx": bad syntax" item)))
                                      backtrace)
                              "\n"))))]
    [(list-rest expand rest)
     (lambda (stx)
       (with-handlers
           ([exn? (lambda (exn)
                    (define backtrace*
                      (cons (exn-message exn) backtrace))
                    (define next-expand
                      (or-expand message backtrace*))
                    ((apply next-expand rest) stx))])
         (expand stx)))]))

; Expansions -------------------------------------

(define (expand-unquote stx)
  (syntax-case* stx (unquote) symbolic-identifier=?
    [(unquote expr) #'expr]))

(define (expand-identifier stx)
  (syntax-case stx ()
    [id (simple-dotted-identifier? #'id 2 2)
        (with-syntax ([(alias attr) (dotted-identifier-split #'id)])
          (cond [(sql-identifier? #'alias)
                 (syntax/loc stx (sql:attr alias 'attr))]
                [(entity-identifier? #'alias)
                 (with-syntax ([entity (entity-info-default-alias-id (entity-info-ref #'alias))])
                   (syntax/loc stx (sql:attr entity 'attr)))]
                [else (raise-syntax-error 'expand-identifier "not an SQL identifier" stx #'id)]))]
    [id (identifier? #'id)
        (cond [(sql-identifier? #'id) #'id]
              [(entity-identifier? #'id)
               (entity-info-default-alias-id (entity-info-ref #'id))]
              [else (raise-syntax-error 'expand-identifier "not an SQL identifier" stx #'id)])]))

(define (expand-literal stx)
  (syntax-case* stx (quote quasiquote) symbolic-identifier=?
    [(quote literal)      (self-quoting-literal? #'literal) #'(sql:literal (quote literal))]
    [(quasiquote literal) (self-quoting-literal? #'literal) #'(sql:literal (quasiquote literal))]
    [literal              (self-quoting-literal? #'literal) #'(sql:literal literal)]))

(define (expand-true+false stx)
  (syntax-case* stx () symbolic-identifier=?
    [#t #'#t]
    [#f #'#f]))

(define (expand-false stx)
  (syntax-case* stx () symbolic-identifier=?
    [#f #'#f]))

(define expand-literal+unquote
  ((or-expand "literal+unquote")
   expand-literal
   expand-unquote))

(define (expand-integer stx)
  (syntax-case* stx (quote quasiquote) symbolic-identifier=?
    [(quote literal)      (integer? (syntax->datum #'literal)) #'literal]
    [(quasiquote literal) (integer? (syntax->datum #'literal)) #'literal]
    [literal              (integer? (syntax->datum #'literal)) #'literal]))

(define expand-integer+unquote
  ((or-expand "integer+unquote")
   expand-integer
   expand-unquote))

(define expand-integer+false+unquote
  ((or-expand "integer+unquote")
   expand-integer
   expand-false
   expand-unquote))

(define (expand-source stx)
  (syntax-case* stx (outer inner left right select) symbolic-identifier=?
    [(outer a b)      #`(sql:outer #,(expand-source+unquote #'a) #,(expand-source+unquote #'b))]
    [(inner a b on)   #`(sql:inner #,(expand-source+unquote #'a) #,(expand-source+unquote #'b) #,(expand-expression+unquote #'on))]
    [(left  a b on)   #`(sql:left  #,(expand-source+unquote #'a) #,(expand-source+unquote #'b) #,(expand-expression+unquote #'on))]
    [(right a b on)   #`(sql:right #,(expand-source+unquote #'a) #,(expand-source+unquote #'b) #,(expand-expression+unquote #'on))]
    [(select arg ...) (expand-select #'(select arg ...))]
    [id               (identifier? #'id)
                      (expand-identifier #'id)]))

(define expand-source+unquote
  ((or-expand "source+unquote")
   expand-source
   expand-unquote))

(define (expand-in-haystack stx)
  (syntax-case* stx (select) symbolic-identifier=?
    [(select arg ...) (expand-select stx)]
    [(item ...)       #`(list #,@(expand-expression+unquote (syntax->list #'(item ...))))]))

(define expand-in-haystack+unquote
  ((or-expand "in-haystack+unquote")
   expand-in-haystack
   expand-unquote))

(define (expand-function stx)
  (syntax-case* stx (and or not + - * / abs floor ceiling round = <> < <= > >=
                         like regexp-match regexp-match-ci regexp-replace regexp-replace* regexp-replace-ci regexp-replace*-ci 
                         string-append string-replace null? coalesce ->string ->symbol in if cond else) symbolic-identifier=?
    [(and                arg ...)            #`(sql:and                #,@(map expand-expression+unquote (syntax->list #'(arg ...))))]
    [(or                 arg ...)            #`(sql:or                 #,@(map expand-expression+unquote (syntax->list #'(arg ...))))]
    [(not                arg)                #`(sql:not                #,@(map expand-expression+unquote (syntax->list #'(arg))))]
    [(+                  arg ...)            #`(sql:+                  #,@(map expand-expression+unquote (syntax->list #'(arg ...))))]
    [(-                  arg ...)            #`(sql:-                  #,@(map expand-expression+unquote (syntax->list #'(arg ...))))]
    [(*                  arg ...)            #`(sql:*                  #,@(map expand-expression+unquote (syntax->list #'(arg ...))))]
    [(/                  arg ...)            #`(sql:/                  #,@(map expand-expression+unquote (syntax->list #'(arg ...))))]
    [(abs                arg)                #`(sql:abs                #,@(map expand-expression+unquote (syntax->list #'(arg))))]
    [(floor              arg)                #`(sql:floor              #,@(map expand-expression+unquote (syntax->list #'(arg))))]
    [(ceiling            arg)                #`(sql:ceiling            #,@(map expand-expression+unquote (syntax->list #'(arg))))]
    [(round              arg)                #`(sql:round              #,@(map expand-expression+unquote (syntax->list #'(arg))))]
    [(=                  arg1 arg2)          #`(sql:=                  #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2))))]
    [(<>                 arg1 arg2)          #`(sql:<>                 #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2))))]
    [(<                  arg1 arg2)          #`(sql:<                  #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2))))]
    [(<=                 arg1 arg2)          #`(sql:<=                 #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2))))]
    [(>                  arg1 arg2)          #`(sql:>                  #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2))))]
    [(>=                 arg1 arg2)          #`(sql:>=                 #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2))))]
    [(like               arg1 arg2)          #`(sql:like               #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2))))]
    [(regexp-match       arg1 arg2)          #`(sql:regexp-match       #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2))))]
    [(regexp-match-ci    arg1 arg2)          #`(sql:regexp-match-ci    #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2))))]
    [(regexp-replace     arg1 arg2 arg3)     #`(sql:regexp-replace     #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2 arg3))))]
    [(regexp-replace*    arg1 arg2 arg3)     #`(sql:regexp-replace*    #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2 arg3))))]
    [(regexp-replace-ci  arg1 arg2 arg3)     #`(sql:regexp-replace-ci  #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2 arg3))))]
    [(regexp-replace*-ci arg1 arg2 arg3)     #`(sql:regexp-replace*-ci #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2 arg3))))]
    [(string-append      arg ...)            #`(sql:string-append      #,@(map expand-expression+unquote (syntax->list #'(arg ...))))]
    [(string-replace     arg1 arg2 arg3)     #`(sql:string-replace     #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2 arg3))))]
    [(null?              arg)                #`(sql:null?              #,@(map expand-expression+unquote (syntax->list #'(arg))))]
    [(coalesce           arg ...)            #`(sql:coalesce           #,@(map expand-expression+unquote (syntax->list #'(arg ...))))]
    [(->string           arg1 arg2)          #`(sql:->string           #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2))))]
    [(->symbol           arg1 arg2)          #`(sql:->symbol           #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2))))]
    [(in                 arg1 arg2)          #`(sql:in                 #,(expand-expression+unquote #'arg1) #,(expand-in-haystack+unquote #'arg2))]
    [(if                 arg1 arg2 arg3)     #`(sql:if                 #,@(map expand-expression+unquote (syntax->list #'(arg1 arg2 arg3))))]
    [(cond [test expr] ... [else else-expr]) #`(sql:cond               #,@(map (lambda (test-stx expr-stx)
                                                                                 #`[#,(expand-expression+unquote test-stx)
                                                                                    #,(expand-expression+unquote expr-stx)])
                                                                               (syntax->list #'(test ...))
                                                                               (syntax->list #'(expr ...)))
                                                                       [else #,(expand-expression+unquote #'else-expr)])]))
                                             
(define (expand-aggregate stx)
  (syntax-case* stx (count* count min max average) symbolic-identifier=?
    [(count*)           #`(sql:count*)]
    [(count*   alias)   #`(sql:count*  #,(expand-expression+unquote #'alias))]
    [(count    alias)   #`(sql:count   #,(expand-expression+unquote #'alias))]
    [(min      alias)   #`(sql:min     #,(expand-expression+unquote #'alias))]
    [(max      alias)   #`(sql:max     #,(expand-expression+unquote #'alias))]
    [(average  alias)   #`(sql:average #,(expand-expression+unquote #'alias))]))

(define expand-expression
  ((or-expand "expression")
   expand-function
   expand-aggregate
   expand-identifier
   expand-literal))

(define expand-expression+unquote
  ((or-expand "expression+unquote")
   expand-expression
   expand-unquote))

(define expand-distinct
  ((or-expand "expression")
   expand-true+false
   expand-function
   expand-aggregate
   expand-identifier
   expand-literal))

(define expand-distinct+unquote
  ((or-expand "distinct+unquote")
   expand-distinct
   expand-unquote))

(define expand-column
  ((or-expand "column")
   expand-expression
   expand-identifier))

(define expand-column+unquote
  ((or-expand "column+unquote")
   expand-column
   expand-unquote))

(define (expand-column-list stx)
  #`(list #,@(map expand-column+unquote (syntax->list stx))))

(define expand-column-list+unquote
  ((or-expand "column-list+unquote")
   expand-column-list
   expand-unquote))

(define expand-column+column-list
  ((or-expand "column+column-list")
   expand-column+unquote
   expand-column-list))

(define expand-column+column-list+unquote
  ((or-expand "column+column-list")
   expand-column+column-list
   expand-unquote))

(define (expand-direction stx)
  (syntax-case* stx (quote quasiquote asc desc) symbolic-identifier=?
    [(quote asc)       #'(quote asc)]
    [(quote desc)      #'(quote desc)]
    [(quasiquote asc)  #'(quasiquote asc)]
    [(quasiquote desc) #'(quasiquote desc)]))

(define expand-direction+unquote
  ((or-expand "direction+unquote")
   expand-direction
   expand-unquote))

(define (expand-order stx)
  (syntax-case* stx (asc desc ord order) symbolic-identifier=?
    [(asc   expr)     #`(sql:asc   #,(expand-expression+unquote #'expr))]
    [(desc  expr)     #`(sql:desc  #,(expand-expression+unquote #'expr))]
    [(ord   expr dir) #`(sql:order #,(expand-expression+unquote #'expr) #,(expand-direction+unquote #'dir))]
    [(order expr dir) #`(sql:order #,(expand-expression+unquote #'expr) #,(expand-direction+unquote #'dir))]))

(define expand-order+unquote
  ((or-expand "order+unquote")
   expand-order
   expand-unquote))

(define (expand-order-list stx)
  #`(list #,@(map expand-order+unquote (syntax->list stx))))

(define expand-order-list+unquote
  ((or-expand "order-list+unquote")
   expand-order-list
   expand-unquote))

(define (expand-select-arguments select-stx args)
  (match args
    ; No arguments left:
    [(list) null]
    ; Only a single argument left (must be an error):
    [(list (and key-stx (app syntax->datum key)))
     (if (keyword? key)
         (raise-syntax-error #f (format "Missing value for ~s" key) select-stx key-stx)
         (raise-syntax-error #f (format "Expected keyword, received ~s" key) select-stx key-stx))]
    ; Keyword and value arguments present:
    [(list-rest (and key-stx (app syntax->datum key))
                (and arg-stx (app syntax->datum arg))
                rest)
     (cond [(not (keyword? key))  (raise-syntax-error #f (format "Expected keyword, received ~s" key) select-stx key-stx)]
           [(keyword? arg)        (raise-syntax-error #f (format "Missing value for ~s" arg) select-stx arg-stx)]
           ; TODO : Check arg-stx is a list
           [(eq? key '#:what)     (list* key-stx 
                                         (expand-column+column-list+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [(eq? key '#:from)     (list* key-stx
                                         (expand-source+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [(eq? key '#:where)    (list* key-stx
                                         (expand-expression+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           ; TODO : Check arg-stx is a list
           [(eq? key '#:group)    (list* key-stx
                                         (expand-column-list+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           ; TODO : Check arg-stx is a list
           [(eq? key '#:order)    (list* key-stx
                                         (expand-order-list+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [(eq? key '#:having)   (list* key-stx
                                         (expand-expression+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [(eq? key '#:limit)    (list* key-stx
                                         (expand-integer+false+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [(eq? key '#:offset)   (list* key-stx
                                         (expand-integer+false+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [(eq? key '#:distinct) (list* key-stx
                                         (expand-distinct+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [else                  (raise-syntax-error #f (format "Unknown select keyword: ~s" key) select-stx key-stx)])]))

(define (expand-select stx)
  (syntax-case* stx (select) symbolic-identifier=?
    [(select kw+expr ...)
     #`(sql:select #,@(expand-select-arguments stx (syntax->list #'(kw+expr ...))))]))

(define expand-top-level
  ((or-expand "sql")
   expand-select
   expand-column
   expand-source
   expand-expression
   expand-order))

; Provide statements -----------------------------

(provide expand-top-level)
