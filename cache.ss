#lang scheme/base

(require "base.ss")

(require "era/core.ss"
         "era/snooze-struct.ss")

; Cache frame ------------------------------------

(define snooze-cache%
  (class* object% (snooze-cache<%>)
    
    (inspect #f)
    
    ; Fields -------------------------------------
    
    ; guid-cache<%>
    (init-field guid-cache)
    
    ; snooze-cache<%>
    (init-field [parent #f])
    
    ; (hashof guid struct)
    (field [structs (make-weak-hasheq)])
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; -> (U snooze-cache<%> #f)
    (define/public (get-parent)
      parent)
    
    ; -> hash
    (define/public (get-structs)
      structs)
    
    ; -> hash
    (define/public (get-guids)
      (send guid-cache get-guids))
    
    ; guid [(guid -> any)] -> snooze-struct
    (define/public (cache-ref guid)
      (parameterize ([in-cache-code? #t])
        (log-cache "cache.cache-ref" guid)
        (or (hash-ref structs guid #f)
            (let ([remote (and parent (send parent cache-ref guid))])
              (and remote
                   (let ([local (copy-snooze-struct remote)])
                     (hash-set! structs (struct-guid local) local)
                     local))))))
    
    ; snooze-struct -> guid
    (define/public (cache-add! struct)
      (parameterize ([in-cache-code? #t])
        (log-cache "cache.cache-add!" struct)
        (let ([guid (struct-guid struct)])
          (when (hash-ref structs (struct-guid struct) #f)
            (raise-cache-add-error struct))
          (hash-set! structs guid struct)
          guid)))
    
    ; snooze-struct -> guid
    (define/public (deep-cache-add! struct)
      (parameterize ([in-cache-code? #t])
        (log-cache "cache.deep-cache-add!" struct)
        (cache-add! struct)
        (if parent
            (send parent deep-cache-add! (copy-snooze-struct struct))
            (struct-guid struct))))
    
    ; guid -> guid
    (define/public (save! guid)
      (parameterize ([in-cache-code? #t])
        (log-cache "cache.save!" guid)
        (let/debug ([struct (hash-ref structs guid (cut raise-cache-ref-error guid))]
                    [entity (guid-entity guid)]
                    [id     (guid-id guid)])
          (when parent
            (printf "going to parent...~n")
            (send parent cache-add! (copy-snooze-struct struct))
            (send parent save! guid))
          (printf "uninterning...~n")
          (send guid-cache unintern-guid! entity id)
          (send guid-cache intern-guid! guid)
          guid)))
    
    ; guid -> guid
    (define/public (delete! guid)
      (parameterize ([in-cache-code? #t])
        (log-cache "cache.delete!" guid)
        (let ([struct (hash-ref structs guid (cut raise-cache-ref-error guid))]
              [entity (guid-entity guid)]
              [id     (guid-id guid)])
          (when parent
            (send parent delete! guid))
          (send guid-cache unintern-guid! entity id)
          guid)))))

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
