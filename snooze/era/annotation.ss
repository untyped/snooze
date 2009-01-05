#lang scheme

(require (file "../annotation.ss")
         (file "../base.ss")
         (file "era-struct.ss"))

; Type definitions used below:
;     era : (U entity relationship attribute)

; Master annotation table ------------------------

; (hasheqof era (hasheqof annotation any))
(define master-hash
  (make-hasheq))

; era -> (hashof annotation any)
(define (annotations-ref era)
  (hash-ref master-hash era
            (lambda ()
              (define hash (make-hasheq))
              (hash-set! master-hash era hash)
              hash)))

; Public accessors and mutators ------------------

; era annotation -> any
(define (era-annotation-ref era annote)
  (hash-ref (annotations-ref era) 
            annote
            (cut (annotation-default annote) era)))

; era annotation -> boolean
(define (era-annotation-set? era annote)
  (with-handlers ([exn? (lambda _ #f)])
    (hash-ref (annotations-ref era) annote)
    #t))

; era annotation any -> void
(define (era-annotation-set! era annote value)
  ; (hashof annotation any)
  (define hash 
    (annotations-ref era))
  (hash-set! hash annote ((annotation-combinator annote)
                          era (era-annotation-ref era annote) value)))

; Predefined annotations -------------------------

; (annotation (U string #f))
(define-annotation ann:pretty-name 
  ; era -> string
  (lambda (era) 
    (symbol->string
     (cond [(entity? era) (entity-name era)]
           #;[(relationship? era) (relationship-name era)]
           [(attribute? era) (attribute-name era)])))
  ; era string string -> string
  (lambda (era old new)
    new))

; Provide statements -----------------------------

(provide/contract
 [ann:pretty-name                                         annotation?]
 [rename era-annotation-ref entity-annotation             (-> entity? annotation? any)]
 ;[rename era-annotation-ref relationship-annotation       (-> relationship? annotation? any)]
 [rename era-annotation-ref attribute-annotation          (-> attribute? annotation? any)]
 [rename era-annotation-set? entity-has-annotation?       (-> entity? annotation? boolean?)]
 ;[rename era-annotation-set? relationship-has-annotation? (-> relationship? annotation? boolean?)]
 [rename era-annotation-set? attribute-has-annotation?    (-> attribute? annotation? boolean?)]
 [rename era-annotation-set! set-entity-annotation!       (-> entity? annotation? any/c void?)]
 ;[rename era-annotation-set! set-relationship-annotation! (-> relationship? annotation? any/c void?)]
 [rename era-annotation-set! set-attribute-annotation!    (-> attribute? annotation? any/c void?)])
