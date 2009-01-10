#lang scheme/base

(require scheme/class)

; Interfaces -------------------------------------

(define cache<%>
  (interface ()
    clear-cache!))

(define entity-cache<%>
  (interface (cache<%>)
    id->entity
    entity->id))

(define attribute-cache<%>
  (interface (cache<%>)
    id->attribute
    attribute->id))

(define cache-mixin
  (mixin () (entity-cache<%> attribute-cache<%>)
    
    ; Fields -------------------------------------
    
    ; entity-cache<%>
    (init-field entity-cache)
    
    ; attribute-cache<%>
    (init-field attribute-cache)

    (super-new)
    
    ; Methods ------------------------------------

    ; integer -> entity
    (define/public (id->entity id)
      (send entity-cache id->entity id))
    
    ; entity -> integer
    (define/public (entity->id entity)
      (send entity-cache entity->id entity))
    
    ; integer -> attribute
    (define/public (id->attribute id)
      (send attribute-cache id->attribute id))
    
    ; attribute -> integer
    (define/public (attribute->id attr)
      (send attribute-cache attribute->id attr))
    
    ; -> void
    (define/public (clear-cache!)
      (send entity-cache clear-cache!)
      (send attribute-cache clear-cache!))
    
    (inspect #f)))

; Provide statements -----------------------------

(provide entity-cache<%>
         attribute-cache<%>
         cache-mixin)
    
