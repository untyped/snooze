#lang scheme/base

(require "../base.ss")

(require "core.ss")

; Variables --------------------------------------

; (struct symbol)
(define-struct annotation (id) #:prefab)

; (hasheqof annotation (annotated -> any))
(define default-value-procedures
  (make-hasheq))

; (hasheqof annotation (annotated old-any new-any -> any))
(define value-combinators
  (make-hasheq))

; Syntax -----------------------------------------

; (_ id (annotated -> any) (annotated any any -> any))
(define-syntax define-annotation
  (syntax-rules ()
    [(_ id default combinator)
     (define id
       (let ([ans #s(annotation (gensym 'id))])
         (hash-set! default-value-procedures ans (check-arity 'id 'default-value-procedure default 1))
         (hash-set! value-combinators ans (check-arity 'id 'value-combinator combinator 3))
         ans))]))

; Procedures -------------------------------------

; annotation check-result -> any
(define (annotation-default annotation result)
  ((hash-ref default-value-procedures annotation) result))

; annotation check-result any any -> any
(define (annotation-compose annotation result old new)
  ((hash-ref value-combinators annotation) result old new))

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
 [struct annotation  ([id symbol?])]
 [annotation-default (-> annotation? check-result? any)]
 [annotation-compose (-> annotation? check-result? any/c any/c any)])