#lang scheme/base

(require "../base.ss")

(require scheme/class
         "../core/core.ss"
         "connection.ss"
         "extract.ss"
         "interface.ss"
         "snooze-reraise.ss"
         "sql-query.ss")

(define generic-database%
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
       (set! snooze the-snooze)))))

; Guid escaping / parsing ------------------------

; type guid -> integer
(define (escape-guid type guid)
  (cond [(not (eq? (guid-entity guid) (guid-type-entity type)))
         (raise-exn exn:fail:snooze:query
           (format "wrong guid entity: expected ~a, received ~a."
                   (entity-name (guid-entity guid))
                   (entity-name (guid-type-entity type))))]
        [(guid-id guid)
         => (lambda (id)
              (if (number? id)
                  (number->string id)
                  (error "expected number, received " id)))]
        [else (raise-exn exn:fail:snooze:query
                (format "cannot use unsaved struct in a query: ~s" guid)
                #f)]))

; Provide statements -----------------------------

(provide generic-database%
         (all-from-out "connection.ss"
                       "interface.ss"
                       "snooze-reraise.ss"
                       "sql-query.ss"))

(provide/contract
 [escape-guid (-> type? guid? integer?)])

