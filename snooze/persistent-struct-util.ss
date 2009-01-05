#lang scheme/base

(require "base.ss"
         "era/era.ss")

(define (keywords->attributes entity args)
  (map (lambda (arg)
         (if (keyword? arg)
             (let ([attr (entity-attribute entity (string->symbol (keyword->string arg)))])
               (if attr
                   attr
                   (raise-exn exn:fail:contract
                     (format "Attribute not found: ~s" arg))))
             arg))
       args))

; Provide statements -----------------------------

(provide keywords->attributes)
