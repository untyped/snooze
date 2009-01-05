#lang scheme/base

(require scheme/contract
         syntax/boundmap)

; Variables --------------------------------------

(define info-cache (make-module-identifier-mapping))

; Structure types --------------------------------

(define-struct persistent-struct-info
  (entity-id attribute-ids attribute-names)
  #:transparent)

; Procedures -------------------------------------

; syntax syntax (listof syntax) (listof symbol) -> boolean
(define (persistent-struct-info-set! id entity-id attr-ids attr-names)
  (module-identifier-mapping-put! info-cache id (make-persistent-struct-info entity-id attr-ids attr-names)))

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
 [struct persistent-struct-info ([entity-id       identifier?]
                                 [attribute-ids   (listof identifier?)]
                                 [attribute-names (listof symbol?)])]
 [persistent-struct-info-set!   (-> identifier? identifier? (listof identifier?) (listof symbol?) void?)]
 [persistent-struct-info-set?   (-> identifier? boolean?)]
 [persistent-struct-info-ref    (-> identifier? (or/c persistent-struct-info? false/c))])
