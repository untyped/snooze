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
    
    ; snooze<%>
    (init-field snooze)
    
    ; (U snooze-cache<%> #f)
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
    
    ; snooze-struct (U interned-vanilla-guid #f) -> local-guid
    ; Returns a cached local guid pointing to the supplied struct / vanilla guid.
    ; Assumes any vaniila-guid caching is taken care of elsewhere.
    (define (localize-guid struct vanilla-guid)
      (printf "localize-guid ~s ~s~n" struct vanilla-guid)
      (let ([local-guid (entity-make-local-guid #:snooze snooze (struct-entity struct))])
        (dict-set! data local-guid (cons vanilla-guid struct))
        local-guid))
    
    ; Public methods -----------------------------
    
    ; -> (U snooze<%> #f)
    (define/public (get-snooze)
      snooze)
    
    ; -> (U snooze-cache<%> #f)
    (define/public (get-parent)
      parent)
    
    ; local-guid -> (U snooze-struct #f)
    (define/public (cache-ref/local guid)
      (printf "cache-ref/local ~s~n" guid)
      (unless (and guid (guid-local? guid))
        (raise-type-error 'cache-ref/local "local-guid" guid))
      (struct-ref guid))
    
    ; snooze-struct -> local-guid
    (define/public (cache-add! struct)
      (printf "cache-add! ~s~n" struct)
      (let* (; (U vanilla-guid #f)
             [struct-guid  (struct-guid struct)]
             ; (U interned-vanilla-guid)
             [vanilla-guid (and struct-guid (cache-add/vanilla! struct (intern-guid struct-guid)))])
        (localize-guid struct vanilla-guid)))
    
    ; snooze-struct interned-vanilla-guid -> interned-vanilla-guid
    (define (cache-add/vanilla! struct vanilla-guid)
      (printf "cache-add/vanilla! ~s ~s~n" struct vanilla-guid)
      (let ([parent (get-parent)])
        (dict-set! data vanilla-guid (cons #f struct))
        (if parent 
            (send (get-parent) cache-add/vanilla! struct vanilla-guid)
            vanilla-guid)))))

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
