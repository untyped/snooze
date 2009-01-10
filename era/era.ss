#lang scheme/base

(require (for-syntax scheme/base
                     scheme/struct-info)
         scheme/unit
         "../base.ss"
         "annotation.ss"
         "era-internal.ss"
         "era-sig.ss"
         "era-struct.ss"
         "era-unit.ss"
         "transaction-sig.ss"
         "transaction-unit.ss")

; Unit invocations -------------------------------

(define-compound-unit/infer complete-era@
  (import)
  (export era^ transaction^)
  (link era@ transaction@))

(define-values/invoke-unit/infer complete-era@)

; Extra definitions ------------------------------

; struct-info
;
; Transformer binding for persistent-struct. struct-id, set-struct-id!,
; struct-revision and set-struct-revision! are used as the accessors and mutators.
(define-syntaxes (persistent-struct)
  (let ([certify (syntax-local-certifier #t)])
    (make-struct-info 
     (lambda ()
       (list (certify #'struct:persistent-struct)
             (certify #'make-persistent-struct)
             (certify #'persistent-struct?)
             (map certify (list #'struct-revision #'struct-id))
             (map certify (list #'set-struct-revision! #'set-struct-id!))
             #t)))))

; Contract helpers -----------------------------

; any -> boolean
(define (attr/value-list? item)
  (or (null? item)
      (and (pair? item)
           (or (attribute? (car item))
               (symbol? (car item)))
           (value/attr-list? (cdr item)))))

; any -> boolean
(define (value/attr-list? item)
  (and (pair? item)
       (attr/value-list? (cdr item))))

; Provide statements ---------------------------

; From era-struct.ss:

(provide (except-out (all-from-out "annotation.ss"
                                   "era-internal.ss"
                                   "era-struct.ss")
                     struct-entity)
         struct:persistent-struct
         make-persistent-struct
         persistent-struct?
         persistent-struct-ref
         persistent-struct-set!
         persistent-struct)

(provide/contract
 [struct-entity                         (-> (or/c persistent-struct? struct-type?) entity?)]
 [entity:persistent-struct              entity?]
 [make-persistent-struct-field-accessor (-> struct-accessor-procedure? integer? symbol? procedure?)]
 [make-persistent-struct-field-mutator  (-> struct-mutator-procedure? integer? symbol? procedure?)]
 [struct-id                             (-> persistent-struct? (or/c integer? false/c))]
 [struct-guid                           (-> persistent-struct? guid?)]
 [set-struct-id!                        (-> persistent-struct? (or/c integer? false/c) void?)]
 [struct-revision                       (-> persistent-struct? (or/c integer? false/c))]
 [set-struct-revision!                  (-> persistent-struct? (or/c integer? false/c) void?)]
 [struct-saved?                         (-> persistent-struct? boolean?)]
 [struct-has-attribute?                 (-> persistent-struct? (or/c symbol? attribute?) boolean?)]
 [struct-attribute                      (-> persistent-struct? (or/c symbol? attribute?) any)]
 [set-struct-attribute!                 (-> persistent-struct? (or/c symbol? attribute?) any/c void?)]
 [struct-attributes                     (-> persistent-struct? list?)]
 [set-struct-attributes!                (-> persistent-struct? list? void?)]
 [make-persistent-struct/defaults       (->* (entity?) () #:rest attr/value-list? persistent-struct?)]
 [copy-persistent-struct                (->* (persistent-struct?) () #:rest attr/value-list? persistent-struct?)]
 [update-persistent-struct-from-copy!   (-> persistent-struct? persistent-struct? void?)])

; From transaction^:

(provide/contract
 [enable-transaction-backups?           (parameter/c boolean?)]
 [call-with-transaction-frame           (-> procedure? any)]
 [store-transaction-backup!             (-> persistent-struct? void?)])
