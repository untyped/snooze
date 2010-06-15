#lang scheme/base

(require "../base.ss")

(require "struct.ss")

; Procedures -------------------------------------

; entity (alternating-listof (U symbol attribute) any) -> (listof attribute) (listof any)
(define (check-attribute-keywords entity args)
  ; boolean (listof (U attribute any)) (listof attribute) (listof any) -> (listof attribute) (listof any)
  (let loop ([even? #f] [args args] [attrs-accum null] [vals-accum null])
    (if even?
        ; Attribute value:
        (if (null? args)
            (raise-type-error 'check-attribute-keywords "no value for attribute" (car attrs-accum))
            (let ([attr (car attrs-accum)]
                  [val  (car args)])
              (if (attribute? val)
                  (raise-type-error 'check-attribute-keywords "no value for attribute" (car attrs-accum))
                  (loop (not even?)
                        (cdr args)
                        attrs-accum
                        (cons val vals-accum)))))
        ; Attribute:
        (if (null? args)
            (values (reverse attrs-accum)
                    (reverse vals-accum))
            (let* ([attr+name (car args)]
                   [attr      (cond [(attribute? attr+name) (entity-attribute entity attr+name)]
                                    [(symbol? attr+name)    (entity-attribute entity attr+name)]
                                    [else                   (raise-type-error 'check-attribute-keywords
                                                                              (if (null? attrs-accum)
                                                                                  "no attribute for value"
                                                                                  "multiple values for attribute")
                                                                              (if (null? attrs-accum)
                                                                                  attr+name
                                                                                  (car attrs-accum)))])])
              (if (for/or ([attr* (in-list attrs-accum)])
                    (and (attribute-name-equal? attr attr*) attr*))
                  (raise-type-error 'check-attribute-keywords "attribute specified more than once" attr)
                  (loop (not even?)
                        (cdr args)
                        (cons attr attrs-accum)
                        vals-accum)))))))

; entity attribute (listof attribute) (listof any) (attribute -> any) -> any
(define (attribute-keyword-get entity needle attrs vals default-ref)
  (let/ec return
    (for ([attr attrs] [val vals])
      (when (eq? needle attr)
        (return val)))
    (default-ref needle)))

; any -> boolean
(define (attr/value-list? item)
  (or (null? item)
      (and (pair? item)
           (or (attribute? (car item))
               (symbol? (car item)))
           (value/attr-list? (cdr item)))))

; Helpers --------------------------------------

; attribute attribute -> boolean
(define (attribute-name-equal? attr1 attr2)
  (equal? (attribute-name attr1)
          (attribute-name attr2)))

; any -> boolean
(define (value/attr-list? item)
  (and (pair? item)
       (attr/value-list? (cdr item))))

; Provide statements -----------------------------

(provide check-attribute-keywords
         attribute-keyword-get
         attr/value-list?)
