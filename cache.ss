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
    
    ; (hashof guid struct)
    (field [structs (make-custom-hash guid=? guid=?-hash-code)])
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; -> (U snooze-cache<%> #f)
    (define/public (get-parent)
      parent)
    
    ; -> hash
    (define/public (get-structs)
      structs)
    
    ; guid -> snooze-struct
    (define/public (cache-ref guid)
      (parameterize ([in-cache-code? #t])
        (log-cache "cache.cache-ref" guid)
        ; Load locally:
        (or (dict-ref structs guid #f)
            ; Fetch from parent and cache locally:
            (let ([remote (and parent (send parent cache-ref guid))])
              (and remote
                   (let ([local (copy-snooze-struct remote)])
                     (dict-set! structs (copy-guid guid) local)  ;; IMPLEMENT IN TERMS OF CACHE-ADD! ???
                     local))))))
    
    ; snooze-struct -> guid
    (define/public (cache-add! struct)
      (parameterize ([in-cache-code? #t])
        (log-cache "cache.cache-add!" struct)
        (let* ([guid0 (struct-guid struct)]
               [guid1 (copy-guid guid0)])
          (when (and (guid-serial guid1) (dict-ref structs guid1 #f))
            (raise-cache-add-error struct))
          (dict-set! structs guid1 struct)
          (when (and parent (not (guid-serial guid1)))
            (send parent cache-add! (copy-snooze-struct struct)))
          guid1)))
    
    ; guid -> snooze-struct
    (define/public (cache-remove! guid)
      (parameterize ([in-cache-code? #t])
        (log-cache "cache.cache-remove!" guid)
        (let ([ans (dict-ref structs guid)])
          (dict-remove! structs guid)
          (when parent (send parent cache-remove! guid))
          ans)))))

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
