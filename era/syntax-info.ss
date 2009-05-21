#lang scheme/base

(require scheme/contract
         syntax/boundmap
         "syntax-info-internal.ss")

; Variables --------------------------------------

(define/provide-info-struct entity-info
  (id
   private-id
   struct-type-id
   constructor-id
   predicate-id
   id-accessor-id
   saved-predicate-id
   vanilla-predicate-id
   local-predicate-id
   pretty-formatter-id
   defaults-constructor-id
   copy-constructor-id
   guid-id
   guid-constructor-id
   guid-predicate-id
   default-alias-id
   attribute-info)
  private-id)

(define-struct attribute-info
  (id private-id type-id accessor-id mutator-id)
  #:transparent)

(define info-cache (make-module-identifier-mapping))

; Procedures -------------------------------------

; entity-info -> entity-info
(define (entity-info-add! info)
  (module-identifier-mapping-put! info-cache (entity-info-id info) info)
  info)

; identifier -> boolean
(define (entity-info-set? id)
  (with-handlers ([exn? (lambda _ #f)])
    (module-identifier-mapping-get info-cache id) 
    #t))

; identifier -> entity-info
(define (entity-info-ref id)
  (module-identifier-mapping-get info-cache id))

; Provide statements -----------------------------

(provide (struct-out attribute-info))

(provide/contract
 [entity-info-add! (-> entity-info? entity-info?)]
 [entity-info-set? (-> identifier? boolean?)]
 [entity-info-ref  (-> identifier? (or/c entity-info? #f))])
