#lang scheme/base

(require "base.ss")

(require "era/era.ss")

(define guid-cache%
  (class* object% (guid-cache<%>)
    
    (inspect #f)
    
    ; Fields -------------------------------------
    
    ; (hashof (cons symbol natural) guid)
    (field [guids (make-hash)])
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; -> hash
    (define (get-guids) guids)
    
    ; entity natural -> guid
    (define/public (get-interned-guid entity id)
      (parameterize ([in-cache-code? #t])
        (unless (entity? entity) (raise-type-error 'get-interned-guid "entity" entity))
        (unless (number? id)     (raise-type-error 'get-interned-guid "number" id))
        (hash-ref guids
                  (cons (entity-name entity) id)
                  (lambda ()
                    (let ([guid (entity-make-guid entity id)])
                      (intern-guid! guid)
                      guid)))))
    
    ; guid -> void
    (define/public (intern-guid! guid)
      (parameterize ([in-cache-code? #t])
        (hash-set! guids (cons (entity-name (guid-entity guid)) (guid-id guid)) guid)))
    
    ; guid -> void
    (define/public (unintern-guid! entity id)
      (parameterize ([in-cache-code? #t])
        (hash-remove! guids (cons (entity-name entity) id))))))

(provide guid-cache%)