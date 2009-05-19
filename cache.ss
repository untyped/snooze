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
    ; (field [data (make-weak-hasheq)])
    ; (field [data (make-weak-custom-hash guid=? guid=?-hash-code)])
    (field [data (make-custom-hash guid=? guid=?-hash-code)])
    ; (field [data (if parent
    ;                  (make-weak-custom-hash guid=? guid=?-hash-code)
    ;                  (make-custom-hash guid=? guid=?-hash-code))])
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Private methods ----------------------------
    
    ; guid -> (U struct #f)
    (define (struct-ref guid)
      (let ([vanilla+struct (dict-ref data guid #f)])
        (and vanilla+struct
             (cdr vanilla+struct))))
    
    ; guid -> guid struct
    (define (vanilla+struct-ref guid)
      (let ([vanilla+struct (dict-ref data guid #f)])
        (and vanilla+struct
             (values (car vanilla+struct)
                     (cdr vanilla+struct)))))
    
    ; snooze-struct (U interned-vanilla-guid #f) -> local-guid
    ; Returns a cached local guid pointing to the supplied struct / vanilla guid.
    ; Assumes any vanilla-guid caching is taken care of elsewhere.
    (define (localize-guid struct vanilla-guid)
      (let ([local-guid (entity-make-local-guid #:snooze snooze (struct-entity struct))])
        (dict-set! data local-guid (cons (and vanilla-guid (intern-guid vanilla-guid)) struct))
        local-guid))
    
    ; Public methods -----------------------------
    
    ; -> (U snooze<%> #f)
    (define/public (get-snooze)
      snooze)
    
    ; -> (U snooze-cache<%> #f)
    (define/public (get-parent)
      parent)
    
    ; -> (weak-custom-hashof guid (cons (U vanilla-guid #f) snooze-struct))
    (define/public (get-data)
      data)
    
    ; Referencing the cache ----------------------
    
    ; guid -> vanilla-guid
    (define/public (get-vanilla-guid guid)
      (if (guid-local? guid)
          (let ([vanilla+struct (dict-ref data guid #f)])
            (unless vanilla+struct (error "not found in cache" guid))
            (and vanilla+struct (car vanilla+struct)))
          (intern-guid guid)))
    
    ; guid -> vanilla-guid
    (define/public (get-saveable-guid guid)
      (if (guid-local? guid)
          (let*-values ([(original)        guid]
                        [(vanilla struct1) (vanilla+struct-ref guid)]
                        [(struct2)         (and vanilla (struct-ref vanilla))])
            (if (and struct1 struct2 (eq? struct1 struct2))
                (copy-guid vanilla)
                (error "struct contains reference to unsaved struct" struct1)))
          guid))
    
    ; local-guid -> (U snooze-struct #f)
    ;
    ; Searches for guid in this cache, and returns the mapped struct.
    (define/public (cache-ref/local guid)
      ;(printf "cache-ref/local ~s~n" guid)
      ;(unless (and guid (guid? guid) (guid-local? guid))
      ;  (raise-type-error 'cache-ref/local "local-guid" guid))
      (struct-ref guid))
    
    ; vanilla-guid -> (U snooze-struct #f)
    ;
    ; Searches for guid in this and ancestor caches, promoting it as necessary.
    ; Returns the mapped struct, or #f if it wasn't found in any cache.
    (define/public (cache-ref/vanilla guid)
      ;(printf "cache-ref/vanilla ~s~n" guid)
      (unless (vanilla-guid? guid)
        (raise-type-error 'cache-ref/vanilla "vanilla-guid" guid))
      (cond [(struct-ref guid)
             => (lambda (struct)
                  struct)]
            [(and parent (send parent cache-ref/vanilla guid))
             => (lambda (struct)
                  (dict-set! data (intern-guid guid) (cons #f struct))
                  struct)]
            [else #f]))
    
    ; Adding to the cache ------------------------
    
    ; snooze-struct -> local-guid
    (define/public (add-copied-struct! struct)
      (parameterize ([in-cache-code? #t])
        (let ([local-guid   (entity-make-local-guid #:snooze snooze (struct-entity struct))]
              [vanilla-guid (and (struct-guid struct) (intern-guid (struct-guid struct)))])
          (dict-set! data local-guid (cons vanilla-guid struct))
          local-guid)))
    
    ; snooze-struct -> local-guid
    (define/public (add-extracted-struct! struct)
      (parameterize ([in-cache-code? #t])
        (unless (struct-guid struct)
          (raise-type-error 'add-extracted-struct! "struct-with-guid" struct))
        
        (let ([local-guid   (entity-make-local-guid #:snooze snooze (struct-entity struct))]
              [vanilla-guid (and (struct-guid struct) (intern-guid (struct-guid struct)))])
          (store-vanilla-struct! vanilla-guid struct)
          (dict-set! data local-guid   (cons vanilla-guid struct))
          local-guid)))
    
    ; snooze-struct local-guid -> local-guid
    (define/public (add-saved-struct! struct old-guid)
      (parameterize ([in-cache-code? #t])
        (unless (struct-guid struct)
          (raise-type-error 'add-saved-struct! "struct-with-guid" struct))
        
        (unless (local-guid? old-guid)
          (raise-type-error 'add-saved-struct! "local-guid" old-guid))
        
        (let ([local-guid   (entity-make-local-guid #:snooze snooze (struct-entity struct))]
              [vanilla-guid (and (struct-guid struct) (intern-guid (struct-guid struct)))])
          (store-vanilla-struct! vanilla-guid struct)
          (dict-set! data old-guid     (cons vanilla-guid struct))
          (dict-set! data local-guid   (cons vanilla-guid struct))
          local-guid)))
    
    ; snooze-struct local-guid -> local-guid
    (define/public (add-deleted-struct! struct old-guid)
      (parameterize ([in-cache-code? #t])
        (when (struct-guid struct)
          (raise-type-error 'add-saved-struct! "struct-with-no-guid" struct))
        
        (unless (local-guid? old-guid)
          (raise-type-error 'add-deleted-struct! "local-guid" old-guid))
        
        (let ([local-guid       (entity-make-local-guid #:snooze snooze (struct-entity struct))]
              [old-vanilla-guid (get-vanilla-guid old-guid)])
          
          (unless (interned-guid? old-vanilla-guid)
            (raise-type-error 'add-deleted-struct! "interned-vanilla-guid" old-vanilla-guid))
          
          (store-vanilla-struct! old-vanilla-guid #f)
          (dict-set! data old-guid     (cons old-vanilla-guid struct))
          (dict-set! data local-guid   (cons old-vanilla-guid struct))
          local-guid)))
    
    ; interned-vanilla-guid (U struct #f) -> void
    (define/public (store-vanilla-struct! vanilla-guid struct)
      (dict-set! data vanilla-guid (cons #f struct))
      (when parent
        (send parent store-vanilla-struct! vanilla-guid struct)))
    
    ; Introducing local guids --------------------
    
    ; guid -> (U local-guid #f)
    ;
    ; Searches for guid in this cache (and its ancestors, performing fetches, if applicable).
    ; Returns a local guid pointing to the same struct, or #f if the struct is not in the cache.
    (define/public (get-local-alias guid)
      ;(printf "get-local-alias ~s~n" guid)
      (if (guid-local? guid)
          (let-values ([(vanilla struct) (vanilla+struct-ref guid)])
            (localize-guid struct vanilla))
          (let ([struct (cache-ref/vanilla guid)])
            (and struct (localize-guid struct guid)))))))

; Provide statements -----------------------------

(provide snooze-cache%)
