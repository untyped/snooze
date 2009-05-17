#lang scheme/base

(require "base.ss")

(require scheme/dict
         "era/core.ss"
         "era/snooze-struct.ss")

; Cache frame ------------------------------------

(define snooze-cache%
  (class* object% (snooze-cache<%>)
    
    (inspect #f)
    
    ; Fields -------------------------------------
    
    ; snooze-cache<%>
    (init-field [parent #f])
    
    ; (hashof guid (cons (U vanilla-guid #f) struct))
    ;
    ; For vanilla guids, the cached value is (cons #f struct).
    ; For local guids, the cached value is (cons vanilla-guid struct).
    (field [data (make-custom-hash guid=? guid=?-hash-code)])
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Private methods ----------------------------
    
    ; guid -> (U struct #f)
    (define (struct-ref guid)  
      (let ([vanilla+struct (dict-ref data guid)])
        (and vanilla+struct
             (cdr vanilla+struct))))
    
    ; guid -> (U guid #f)
    (define (vanilla-ref guid)
      (if (guid-local? guid)
          (let ([vanilla+struct (dict-ref data guid)])
            (unless vanilla+struct (error "not found in cache" guid))
            (and vanilla+struct
                 (car vanilla+struct)))
          (intern-guid guid)))
    
    ; guid -> guid struct
    (define (vanilla+struct-ref guid)
      (values (vanilla-ref guid)
              (struct-ref guid)))
    
    ; guid (U vanilla-guid #f) struct -> vuid
    (define (cache-set! guid vanilla-guid struct)
      (if (guid-local? guid)
          (unless (and vanilla-guid (not (guid-local? vanilla-guid)))
            (error "attempt to cache local guid against invalid vanilla guid" (list guid vanilla-guid struct)))
          (when vanilla-guid
            (error "attempt to cache vanilla guid against " (list guid vanilla-guid struct))))
      (dict-set! data guid (cons vanilla-guid struct)))
    
    ; Methods ------------------------------------
    
    ; -> (U snooze-cache<%> #f)
    (define/public (get-parent)
      parent)
    
    ; guid -> snooze-struct
    ;(define/public (cache-ref guid)
    ;  (parameterize ([in-cache-code? #t])
    ;    (log-cache "cache.cache-ref" guid)
    ;    
    ;    ; Try to retrieve from the local cache...
    ;    (or (let ([guid+struct (dict-ref structs guid #f)])
    ;          (and guid+struct (cdr guid+struct)))
    ;        ; ...then try to retrieve from the parent cache:
    ;        (let ([struct (and parent (send parent cache-ref guid))])
    ;          (and struct
    ;               (let ([local-guid (intern-guid (struct-guid struct))])
    ;                 (dict-set! structs local-guid struct)  ;; IMPLEMENT IN TERMS OF CACHE-ADD! ???
    ;                 struct))))))
    
    ; guid -> local-guid
    (define/public (make-local-copy! original)
      (let-values ([(vanilla struct) (vanilla+struct-ref original)])
        (unless (and vanilla struct)
          (error "struct not cached" original))
        (let ([local (entity-make-local-guid
                      #:snooze (guid-snooze (struct-guid struct))
                      (struct-entity struct))])
          (cache-set! local vanilla struct) 
          local)))
    
    ; snooze-struct -> guid
    ;(define/public (cache-add! struct)
    ;  (if (struct-guid struct)
    ;      (cache-add/db! struct)
    ;      (cache-add/local! struct)))
    
    ; snooze-struct -> local-guid
    ;(define/public (cache-add/db! struct)
    ;  (cache+create-local! (struct-guid struct)))
    
    ; guid -> local-guid
    ;(define/public (cache+create-local! original-guid)
    ;  (let* ([vanilla-guid  (intern-guid original-guid)]
    ;         [local-guid    (entity-make-local-guid #:snooze snooze (struct-entity struct))])
    ;    (when (guid-local? original-guid)
    ;      (error "guid should not be local" original-guid))
    ;    (when (guid-interned? original-guid)
    ;      (error "guid should not be interned" original-guid))
    ;    (dict-set! structs vanilla-guid (cons #f struct))
    ;    (dict-set! structs local-guid (cons vanilla-guid struct))
    ;    local-guid))
    
    ; snooze-struct -> guid
    ;(define/public (cache-add/local! struct)
    ;  (let ([guid (entity-make-local-guid #:snooze snooze (struct-entity struct))])
    ;    (dict-set! structs guid (cons #f struct))
    ;    guid))
    ; 
    ;  (parameterize ([in-cache-code? #t])
    ;    (log-cache "cache.cache-add!" struct)
    ;    (let* ([guid0 (struct-guid struct)]
    ;           [guid1 (copy-guid guid0)])
    ;      (when (and (guid-serial guid1) (dict-ref structs guid1 #f))
    ;        (raise-cache-add-error struct))
    ;      (dict-set! structs guid1 struct)
    ;      (when (and parent (not (guid-serial guid1)))
    ;        (send parent cache-add! (copy-snooze-struct struct)))
    ;      guid1)))
    
    ; guid -> snooze-struct
    ;(define/public (cache-remove! guid)
    ;  (parameterize ([in-cache-code? #t])
    ;    (log-cache "cache.cache-remove!" guid)
    ;    (let ([ans (dict-ref structs guid)])
    ;      (dict-remove! structs guid)
    ;      (when parent (send parent cache-remove! guid))
    ;      ans)))
    
    ))

; Helpers ----------------------------------------

; guid -> void
(define (raise-cache-add-error guid)
  (raise-exn exn:fail:snooze:cache
    (format "cache-add!: struct already cached: ~s" guid)))

; guid -> void
(define (raise-cache-ref-error guid)
  (raise-exn exn:fail:snooze:cache
    (format "cache-ref: struct not found in cache: ~s" guid)))

; Provide statements -----------------------------

(provide snooze-cache%)
