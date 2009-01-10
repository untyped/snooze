#lang scheme/base

(require scheme/dict
         (planet untyped/unlib:3/contract)
         "base.ss")

; Check results can be decorated with one or more "annotations", which
; are mappings from an annotation struct to an arbitrary value.
; 
; An "annotation" is a structure that acts as a key in the check-result
; annotations hash and contains code to create and combine annotated values.

; Structure types --------------------------------

; (struct (U 'result 'era) symbol)
(define-struct annotation (id) #:prefab)

; (hasheqof annotation (annotated -> any))
(define default-value-procedures
  (make-hasheq))

; (hasheqof annotation (annotated old-any new-any -> any))
(define value-combinators
  (make-hasheq))

; Syntax -----------------------------------------

; (_ id any (any any -> any))
(define-syntax define-annotation
  (syntax-rules ()
    [(_ id default combinator)
     (define id
       (let ([ans #s(annotation 'id)])
         (hash-set! default-value-procedures ans (check-arity 'id 'default-value-procedure default 1))
         (hash-set! value-combinators ans (check-arity 'id 'value-combinator combinator 3))
         ans))]))

; Procedures -------------------------------------

; annotation -> (annotated -> any)
(define (annotation-default annotation)
  (hash-ref default-value-procedures annotation))

; annotation -> (annotated old-any new-any -> any)
(define (annotation-combinator annotation)
  (hash-ref value-combinators annotation))

; Helpers ----------------------------------------

; procedure -> procedure | exn:fail:contract
(define (check-arity annote-id proc-id proc n)
  (if (and (procedure? proc) (procedure-arity-includes? proc n))
      proc
      (raise-exn exn:fail:contract
        (format "define-annotation ~a: ~a: expected procedure of arity ~a, received ~a"
                annote-id proc-id n proc))))

; Provide statements -----------------------------

(provide define-annotation)

(provide/contract
 [struct annotation     ([id symbol?])]
 [annotation-default    (-> annotation? (arity/c 1))]
 [annotation-combinator (-> annotation? (arity/c 3))])
