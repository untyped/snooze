#lang scheme/base

(require "../base.ss")

; GUIDs ------------------------------------------

; (struct entity integer)
(define-serializable-struct guid 
  (entity id)
  #:transparent
  #:property
  prop:custom-write
  (lambda (guid port write?)
    (define show (if write? write display))
    (display "#(guid" port)
    (when (print-struct)
      (display " entity:" port)
      (show (entity-name (guid-entity guid)) port)
      (display " " port)
      (show (guid-id guid) port))
    (display ")" port)))

; Attribute types --------------------------------

; (struct boolean any)
(define-serializable-struct type (allows-null? default) #:transparent)

; (struct boolean any entity)
(define-serializable-struct (guid-type type) (entity) #:transparent)

; (struct boolean any)
(define-serializable-struct (boolean-type type) ()  #:transparent)

; (struct boolean any)
(define-serializable-struct (integer-type type) () #:transparent)

; (struct boolean any)
(define-serializable-struct (real-type type) () #:transparent)

; (struct boolean any (U integer #f))
(define-serializable-struct (string-type type) (max-length) #:transparent)

; (struct boolean any (U integer #f))
(define-serializable-struct (symbol-type type) (max-length) #:transparent)

; (struct boolean any)
(define-serializable-struct (time-utc-type type) () #:transparent)

; (struct boolean any)
(define-serializable-struct (time-tai-type type) () #:transparent)

; Entities, relationships and attributes ---------

; (struct symbol 
;         symbol
;         struct-type
;         (any ... -> persistent-struct)
;         (any -> boolean)
;         (persistent-struct integer -> any)
;         (persistent-struct integer any -> void)
;         (listof attribute)
;         (listof (conn struct -> integer))
;         (listof (conn struct -> integer))
;         (listof (conn struct -> integer))
;         (listof (conn struct -> integer)))
(define-serializable-struct entity 
  (name table-name struct-type constructor predicate accessor mutator attributes save-pipeline insert-pipeline update-pipeline delete-pipeline)
  #:transparent
  #:mutable
  #:property 
  prop:custom-write
  (lambda (entity port write?)
    (fprintf port "#<entity:~a>" (entity-name entity))))

; TODO : Relationship struct type

; (struct symbol 
;         entity
;         (struct -> any)
;         (struct any -> void)
;         integer
;         type)
(define-serializable-struct attribute 
  (name column-name entity index accessor mutator type)
  #:transparent
  #:mutable
  #:property
  prop:custom-write
  (lambda (attribute port write?)
    (fprintf port "#<attr:~a-~a>" 
             (entity-name (attribute-entity attribute))
             (attribute-name attribute))))

; Provide statements -----------------------------

(provide/contract
 [struct entity               ([name            symbol?]
                               [table-name      symbol?]
                               [struct-type     (or/c struct-type? false/c)]
                               [constructor     procedure?]
                               [predicate       procedure?]
                               [accessor        procedure?]
                               [mutator         procedure?]
                               [attributes      (listof attribute?)]
                               [save-pipeline   (listof stage?)]
                               [insert-pipeline (listof stage?)]
                               [update-pipeline (listof stage?)]
                               [delete-pipeline (listof stage?)])]
 ;[struct relationship   ()]
 [struct attribute            ([name            symbol?]
                               [column-name     symbol?]
                               [entity          entity?]
                               [index           integer?]
                               [accessor        procedure?]
                               [mutator         procedure?]
                               [type            type?])]
 [struct guid                 ([entity          entity?]
                               [id              integer?])]
 [struct type                 ([allows-null?    boolean?]
                               [default         any/c])]
 [struct (guid-type type)     ([allows-null?    boolean?]
                               [default         false/c]
                               [entity          entity?])]
 [struct (boolean-type type)  ([allows-null?    boolean?]
                               [default         (or/c boolean? false/c)])]
 [struct (integer-type type)  ([allows-null?    boolean?]
                               [default         (or/c integer? false/c)])]
 [struct (real-type type)     ([allows-null?    boolean?]
                               [default         (or/c real? false/c)])]
 [struct (string-type type)   ([allows-null?    boolean?]
                               [default         (or/c string? false/c)]
                               [max-length      (or/c integer? false/c)])]
 [struct (symbol-type type)   ([allows-null?    boolean?]
                               [default         (or/c symbol? false/c)]
                               [max-length      (or/c integer? false/c)])]
 [struct (time-utc-type type) ([allows-null?    boolean?]
                               [default         (or/c time-utc? false/c)])]
 [struct (time-tai-type type) ([allows-null?    boolean?]
                               [default         (or/c time-tai? false/c)])])
