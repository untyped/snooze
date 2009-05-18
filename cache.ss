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
    (field [data (make-weak-custom-hash guid=? guid=?-hash-code)])
    
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
    ; Assumes any vaniila-guid caching is taken care of elsewhere.
    (define (localize-guid struct vanilla-guid)
      ;(printf "localize-guid ~s ~s~n" struct vanilla-guid)
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
          (let*-values ([(vanilla struct1) (vanilla+struct-ref guid)]
                        [(struct2)         (struct-ref vanilla)])
            (if (and struct1 struct2 (eq? struct1 struct2))
                (copy-guid vanilla)
                (error "reference to unsaved struct" struct1)))
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
      (unless (and guid (guid? guid) (not (guid-local? guid)))
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
    ;
    ; Adds a struct to the cache and returns a new local guid that points to it:
    ;   - if the struct contains an id, it is cached by vanilla and local guid;
    ;   - if the struct's id is #f, it is cached by local guid only.
    (define/public (add-struct! struct)
      ;(printf "add-struct! ~s~n" struct)
      (let* (; (U vanilla-guid #f)
             [struct-guid  (struct-guid struct)]
             ; (U interned-vanilla-guid)
             [vanilla-guid (and struct-guid (add-vanilla-struct! struct (intern-guid struct-guid)))])
        (localize-guid struct vanilla-guid)))
    
    ; snooze-struct interned-vanilla-guid -> interned-vanilla-guid
    ;
    ; Adds a vanilla struct to the cache and its ancestors,
    ; returning the interned local guid used.
    (define (add-vanilla-struct! struct vanilla-guid)
      ;(printf "add-vanilla-struct! ~s ~s~n" struct vanilla-guid)
      (let ([parent (get-parent)])
        (dict-set! data vanilla-guid (cons #f struct))
        (if parent 
            (send (get-parent) add-vanilla-struct! struct vanilla-guid)
            vanilla-guid)))
    
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
