#lang scheme/base

(require scheme/contract
         syntax/boundmap
         "persistent-struct-info-internal.ss")

; Variables --------------------------------------

(define info-cache (make-module-identifier-mapping))

; Procedures -------------------------------------

;  procedure
;  identifier
;  identifier
;  identifier
;  identifier
;  identifier
;  identifier
;  identifier
;  identifier
;  (listof identifier)
;  (listof identifier)
;  (listof identifier)
;  (listof symbol)
; ->
;  persistent-struct-info
(define (persistent-struct-info-set!
         struct-info-proc
         id
         struct-type-id
         entity-id
         constructor-id
         constructor/defaults-id
         copy-struct-id
         predicate-id
         default-alias-id
         attribute-ids
         accessor-ids
         mutator-ids
         attribute-names)
  (let ([info (make-persistent-struct-info struct-info-proc
                                           id
                                           struct-type-id
                                           entity-id
                                           constructor-id
                                           constructor/defaults-id
                                           copy-struct-id
                                           predicate-id
                                           default-alias-id
                                           attribute-ids
                                           accessor-ids
                                           mutator-ids
                                           attribute-names)])
    (module-identifier-mapping-put! info-cache id info)
    (module-identifier-mapping-put! info-cache entity-id info)
    info))

; syntax -> boolean
(define (persistent-struct-info-set? id)
  (with-handlers ([exn? (lambda _ #f)])
    (module-identifier-mapping-get info-cache id) 
    #t))

; identifier -> persistent-struct-info
(define (persistent-struct-info-ref id)
  (module-identifier-mapping-get info-cache id))

; Provide statements -----------------------------

(provide (all-from-out "persistent-struct-info-internal.ss"))

(provide/contract
 [persistent-struct-info-set!   (-> procedure?
                                    identifier?
                                    identifier?
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
                                    persistent-struct-info?)]
 [persistent-struct-info-set?   (-> syntax? boolean?)]
 [persistent-struct-info-ref    (-> identifier? (or/c persistent-struct-info? #f))])
