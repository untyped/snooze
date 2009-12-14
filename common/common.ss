#lang scheme/base

(require "../base.ss")

(require scheme/class
         "../core/core.ss"
         "connection.ss"
         "cross-reference.ss"
         "extract.ss"
         "interface.ss"
         "snooze-reraise.ss"
         "sql-query.ss")

(define generic-database%
  (generic-cross-reference-mixin
   (generic-extract-mixin
    (class* object% (generic-database<%>)
      
      ; Fields -------------------------------------
      
      ; (U snooze<%> #f)
      (field [snooze #f])
      
      ; Constructor --------------------------------
      
      (super-new)
      
      ; Methods ------------------------------------
      
      ; -> snooze<%>
      (define/public (get-snooze)
        snooze)
      
      ; snooze<%> -> void
      (define/public (set-snooze! the-snooze)
        (set! snooze the-snooze))))))

; Guid escaping / parsing ------------------------

; type guid -> integer
(define (escape-guid type val)
  (match val
    [(? guid?)
     (cond [(temporary-guid? val)
            (raise-exn exn:fail:snooze:query
              (format "cannot use unsaved struct in a query: ~s" val)
              #f)]
           [(not (eq? (guid-entity val) (guid-type-entity type)))
            (raise-exn exn:fail:snooze:query
              (format "wrong guid entity: expected ~a, received ~a."
                      (entity-name (guid-entity val))
                      (entity-name (guid-type-entity type))))]
           [else (number->string (guid-id val))])]
    [(? snooze-struct?)
     (cond [(not (snooze-struct-saved? val))
            (raise-exn exn:fail:snooze:query
              (format "cannot use unsaved struct in a query: ~s" val)
              #f)]
           [(not (eq? (snooze-struct-entity val) (guid-type-entity type)))
            (raise-exn exn:fail:snooze:query
              (format "wrong guid entity: expected ~a, received ~a."
                      (entity-name (snooze-struct-entity val))
                      (entity-name (guid-type-entity type))))]
           [else (number->string (snooze-struct-id val))])]
    [_ (raise-type-error 'escape-guid "(U guid snooze-struct)" val)]))

; Provide statements -----------------------------

(provide generic-database%
         (all-from-out "connection.ss"
                       "interface.ss"
                       "snooze-reraise.ss"
                       "sql-query.ss"))

(provide/contract
 [escape-guid (-> type? (or/c guid? snooze-struct?) string?)])

