#lang scheme/base

(require (for-syntax scheme/base
                     scheme/match
                     scheme/pretty
                     srfi/26/cut
                     (planet untyped/unlib:3/syntax)
                     "../era/info.ss"
                     "sql-syntax-util.ss")
         (prefix-in sql: "sql-lang.ss")
         "sql-struct.ss")

; syntax struct-info-list -> syntax syntax
(define-for-syntax (expand-entity-alias id-stx info)
  (match info
    [(list entity-stx attr-stxs attr-names)
     (with-syntax ([entity-id     id-stx]
                   [entity        entity-stx]
                   [(attr-id ...) (map (cut make-id id-stx id-stx '- <>) attr-names)]
                   [(attr ...)    attr-stxs])
       (values #`(entity-id attr-id ...)
               #`(let ([entity-alias (make-entity-alias 'entity-id entity)])
                   (values entity-alias (make-attribute-alias entity-alias attr) ...))))]))

; (_ id expr)
(define-syntax (define-sql stx)
  (syntax-case stx ()
    [(_ id val)
     (with-syntax ([secret-binding (make-private-sql-identifier (syntax->datum #'id))])
       #`(begin (define secret-binding val)
                (define-syntax id #,(make-sql-transformer #'secret-binding))))]))

; (_ id expr)
(define-syntax (define-alias stx)
  (syntax-case stx ()
    [(_ id val)
     (entity-identifier? #'val)
     (match (entity-info-ref #'val)
       [(and (app entity-info-id entity-stx)
             (app entity-info-attribute-info attrs))
        (with-syntax ([entity        entity-stx]
                      [(attr-id ...) (map (cut make-id #'id #'id '- <>)
                                          (map attribute-info-id attrs))]
                      [(attr ...)    (map attribute-info-id attrs)])
          #'(define-sql id (sql:alias 'id entity)))])]
    [(_ id val)
     #'(define-sql id (sql:alias 'id val))]))

; (_ ([id val] ...) expr ...)
(define-syntax (let-sql stx)
  (syntax-case stx ()
    [(_ () expr ...)
     #'(begin expr ...)]
    [(_ ([id val] [id2 val2] ...) expr ...)
     #'(let ()
         (define-sql id val)
         (let-sql ([id2 val2] ...)
           expr ...))]))

; (_ ([id val] ...) expr ...)
(define-syntax (let-alias stx)
  (syntax-case stx ()
    [(_ () expr ...)
     #'(begin expr ...)]
    [(_ ([id val] [id2 val2] ...) expr ...)
     #'(let ()
         (define-alias id val)
         (let-alias ([id2 val2] ...)
           expr ...))]))

; Provide statments ------------------------------

(provide define-sql
         define-alias
         let-sql
         let-alias)
