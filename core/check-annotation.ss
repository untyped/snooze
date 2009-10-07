#lang scheme/base

(require "../base.ss")

(require (for-syntax scheme/base)
         "struct.ss")

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

; procedure -> procedure | exn:fail:contract
(define (check-arity annote-id proc-id proc n)
  (if (and (procedure? proc) (procedure-arity-includes? proc n))
      proc
      (raise-syntax-error #f
                          (format "~a: expected procedure of arity ~a, received ~a"
                                  proc-id n proc))))

; (_ id (annotated -> any) (annotated any any -> any))
(define-syntax (define-annotation stx)
  (syntax-case stx ()
    [(_ id default combinator)
     #'(define id
         (let ([ans #s(annotation (gensym 'id))])
           (hash-set! default-value-procedures ans (check-arity 'id 'default-value-procedure default 1))
           (hash-set! value-combinators ans (check-arity 'id 'value-combinator combinator 3))
           ans))]))

; annotation
(define-annotation ann:struct
  (lambda (result) #f)
  (lambda (result old new) new))

; annotation
(define-annotation ann:attrs
  (lambda (result) null)
  (lambda (result old new) (append old new)))

; Annotation combinators -------------------------

; annotation check-result -> any
(define (annotation-default annotation result)
  ((hash-ref default-value-procedures annotation) result))

; annotation check-result any any -> any
(define (annotation-compose annotation result old new)
  ((hash-ref value-combinators annotation) result old new))

; Check result procedures ------------------------

; check-result annotation -> any
(define (check-result-annotation result annote)
  (hash-ref (check-result-annotations result)
            annote
            (cut annotation-default annote result)))

; check-result annotation -> boolean
(define (check-result-has-annotation? result annote)
  (with-handlers ([exn? (lambda _ #f)])
    (hash-ref (check-result-annotations result) annote)
    #t))

; check-result annotation any -> check-result
(define (check-result-annotation-set result annote val)
  (let* ([message (check-result-message result)]
         [old     (check-result-annotation result annote)]
         [new     (annotation-compose annote result old val)]
         [annotes (hash-set (check-result-annotations result) annote new)])
    ; check-result
    (cond [(check-success? result) (make-check-success message annotes)]
          [(check-warning? result) (make-check-warning message annotes)]
          [(check-failure? result) (make-check-failure message annotes)]
          [(check-fatal? result)   (make-check-fatal   message annotes (check-fatal-exn result))])))

; ann:struct, ann:attrs wrappers -----------------

; check-result -> (listof attribute)
(define (check-result-attributes result)
  (check-result-annotation result ann:attrs))

; check-result attribute -> boolean
(define (check-result-has-attribute? result attr)
  (and (memq attr (check-result-attributes result)) #t))

; check-result (listof attributes) -> check-result
(define (check-result-attributes-add result val)
  (check-result-annotation-set result ann:attrs val))

; check-result -> (U guid #f)
(define (check-result-struct result)
  (check-result-annotation result ann:struct))

; check-result (U guid #f) -> check-result
(define (check-result-struct-set result val)
  (check-result-annotation-set result ann:struct val))

; Provide statements -----------------------------

(provide define-annotation
         ann:struct
         ann:attrs)

(provide/contract
 [struct annotation            ([id symbol?])]
 [annotation-default           (-> annotation? check-result? any)]
 [annotation-compose           (-> annotation? check-result? any/c any/c any)]
 [check-result-annotation      (-> check-result? annotation? any)]
 [check-result-has-annotation? (-> check-result? annotation? boolean?)]
 [check-result-annotation-set  (-> check-result? annotation? any/c check-result?)]
 [check-result-attributes      (-> check-result? (listof attribute?))]
 [check-result-has-attribute?  (-> check-result? attribute? boolean?)]
 [check-result-attributes-add  (-> check-result? (listof attribute?) check-result?)]
 [check-result-struct          (-> check-result? (or/c snooze-struct? #f))]
 [check-result-struct-set      (-> check-result? (or/c snooze-struct? #f) check-result?)])
