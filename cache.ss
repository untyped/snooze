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
    
    ; guid-cache<%>
    (init-field guid-cache)
    
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
    
    ; -> hash
    (define/public (get-guids)
      (send guid-cache get-guids))
    
    ; guid [(guid -> any)] -> snooze-struct
    (define/public (cache-ref guid)
      (parameterize ([in-cache-code? #t])
        (log-cache "cache.cache-ref" guid)
        ; Load locally:
        (or (dict-ref structs guid #f)
            ; Fetch from parent and cache locally:
            (let ([remote (and parent (send parent cache-ref guid))])
              (and remote
                   (let ([local (copy-snooze-struct remote)])
                     (dict-set! structs (struct-guid local) local)
                     local))))))
    
    ; snooze-struct -> guid
    (define/public (cache-add! struct)
      (parameterize ([in-cache-code? #t])
        (log-cache "cache.cache-add!" struct)
        (let* ([guid0 (struct-guid struct)]
               [guid1 (entity-make-guid #:snooze (guid-snooze guid0) (guid-entity guid0) (guid-id guid0) (guid-serial guid0))])
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
          ans)))
    
    ; guid (U natural #f) (U symbol #f) -> void
    (define/public (recache! guid id serial)
      (let ([struct (cache-remove! guid)])
        (set-guid-id! guid id)
        (set-guid-serial! guid serial)
        (set-guid-id! (struct-guid struct) id)
        (set-guid-serial! (struct-guid struct) serial)
        (cache-add! struct)))))

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
