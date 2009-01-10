#lang scheme/base

(require scheme/contract
         syntax/boundmap)

; Variables --------------------------------------

(define info-cache (make-module-identifier-mapping))

; Structure types --------------------------------

; (struct syntax
;         syntax
;         syntax
;         syntax
;         syntax
;         syntax
;         syntax
;         (listof syntax)
;         (listof syntax)
;         (listof syntax)
;         (listof symbol))
(define-struct persistent-struct-info
  (id
   struct-type-id
   entity-id
   constructor-id
   constructor/defaults-id
   copy-struct-id
   predicate-id
   attribute-ids
   accessor-ids
   mutator-ids
   attribute-names)
  #:transparent)

; Procedures -------------------------------------

;  syntax
;  syntax
;  syntax
;  syntax
;  syntax
;  syntax
;  syntax
;  (listof syntax)
;  (listof syntax)
;  (listof syntax)
;  (listof symbol)
; ->
;  boolean
(define (persistent-struct-info-set!
         id
         struct-type-id
         entity-id
         constructor-id
         constructor/defaults-id
         copy-struct-id
         predicate-id
         attribute-ids
         accessor-ids
         mutator-ids
         attribute-names)
  (module-identifier-mapping-put! info-cache
                                  id
                                  (make-persistent-struct-info id
                                                               struct-type-id
                                                               entity-id
                                                               constructor-id
                                                               constructor/defaults-id
                                                               copy-struct-id
                                                               predicate-id
                                                               attribute-ids
                                                               accessor-ids
                                                               mutator-ids
                                                               attribute-names)))

; syntax -> boolean
(define (persistent-struct-info-set? id)
  (with-handlers ([exn? (lambda _ #f)])
    (module-identifier-mapping-get info-cache id) 
    #t))

; syntax -> persistent-struct-info
(define (persistent-struct-info-ref id)
  (module-identifier-mapping-get info-cache id))

; Provide statements -----------------------------

(provide/contract
 [struct persistent-struct-info ([id                      identifier?]
                                 [struct-type-id          identifier?]
                                 [entity-id               identifier?]
                                 [constructor-id          identifier?]
                                 [constructor/defaults-id identifier?]
                                 [copy-struct-id          identifier?]
                                 [predicate-id            identifier?]
                                 [attribute-ids           (listof identifier?)]
                                 [accessor-ids            (listof identifier?)]
                                 [mutator-ids             (listof identifier?)]
                                 [attribute-names         (listof symbol?)])]
 [persistent-struct-info-set!   (-> identifier?
                                    identifier?
                                    identifier?
                                    identifier?
                                    identifier?
                                    identifier?
                                    identifier?
                                    (listof identifier?)
                                    (listof identifier?)
                                    (listof identifier?)
                                    (listof symbol?)
                                    void?)]
 [persistent-struct-info-set?   (-> identifier? boolean?)]
 [persistent-struct-info-ref    (-> identifier? (or/c persistent-struct-info? false/c))])
