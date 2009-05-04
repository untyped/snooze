#lang scheme/base

(require scheme/class
         scheme/contract
         srfi/19/time
         (planet untyped/unlib:3/time)
         "../snooze.ss"
         "attribute.ss"
         "cache.ss")

(define-snooze-struct audit-delta
  ([transaction-id  (make-integer-type #f #f)   #:column-name 'transactionID]
   [type            (make-symbol-type #f #f 1)] ; (U 'I 'U 'D 'K)
   [entity-id       (make-integer-type #f #f)   #:column-name 'entityID]
   [struct-id       (make-integer-type #f #f)   #:column-name 'structID]
   [struct-revision (make-integer-type #t #f)   #:column-name 'structRevision]
   [attribute-id    (make-integer-type #t #f)   #:column-name 'attributeID]
   [boolean-value   (make-boolean-type #t #f)   #:column-name 'booleanValue]
   [integer-value   (make-integer-type #t #f)   #:column-name 'integerValue]
   [real-value      (make-real-type #t #f)      #:column-name 'realValue]
   [string-value    (make-string-type #t #f #f) #:column-name 'stringValue]
   [time-utc-value  (make-time-utc-type #t #f)  #:column-name 'timeValue])
  #:table-name 'auditdeltas)

; API --------------------------------------------

(define delta-api<%>
  (interface ()
    make-insert-delta
    make-update-delta
    make-delete-delta
    audit-delta-entity
    audit-delta-guid
    audit-delta-attribute
    audit-delta-value
    revert-delta!))

(define delta-api%
  (class* (cache-mixin object%) (delta-api<%>)
    
    (inherit id->entity
             entity->id
             id->attribute
             attribute->id)
    
    ; Fields -------------------------------------

    ; snooze<%>
    (init-field snooze)
    
    (super-new)
    
    ; Public methods -----------------------------
    
    ; audit-transaction guid -> audit-delta
    (define/public (make-insert-delta txn guid)
      (define entity-id (entity->id (guid-entity guid)))
      (define id        (guid-id guid))
      (make-audit-delta (struct-id txn) ; transaction-id
                        'I              ; type
                        entity-id       ; entity-id
                        id              ; struct-id
                        #f              ; struct-revision
                        #f              ; attribute-id
                        #f              ; boolean-value
                        #f              ; integer-value
                        #f              ; real-value
                        #f              ; string-value
                        #f))            ; time-utc-value
    
    ; audit-transaction guid integer attribute any -> audit-delta
    (define/public (make-update-delta txn guid revision attr value)
      (make-update/delete-delta txn 'U guid revision attr value))
    
    ; audit-transaction guid integer attribute any -> audit-delta
    (define/public (make-delete-delta txn guid revision attr value)
      (make-update/delete-delta txn 'D guid revision attr value))
    
    ; audit-delta -> entity
    (define/public (audit-delta-entity delta)
      (id->entity (audit-delta-entity-id delta)))
    
    ; audit-delta -> guid
    (define/public (audit-delta-guid delta)
      (make-guid (id->entity (audit-delta-entity-id delta))
                 (audit-delta-struct-id delta)))
    
    ; audit-delta -> (U attribute #f)
    (define/public (audit-delta-attribute delta)
      (define id (audit-delta-attribute-id delta))
      (if id (id->attribute id) #f))

    ; audit-delta type -> any
    (define/public (audit-delta-value delta type)
      (cond [(boolean-type?  type) (audit-delta-boolean-value delta)]
            [(integer-type?  type) (audit-delta-integer-value delta)]
            [(real-type?     type) (audit-delta-real-value delta)]
            [(string-type?   type) (audit-delta-string-value delta)]
            [(symbol-type?   type) (if (audit-delta-string-value delta)
                                       (string->symbol (audit-delta-string-value delta))
                                       #f)]
            [(time-tai-type? type) (if (audit-delta-time-utc-value delta)
                                       (time-utc->time-tai (audit-delta-time-utc-value delta))
                                       #f)]
            [(time-utc-type? type) (audit-delta-time-utc-value delta)]))

    ; guid audit-delta (U snooze-struct #f) -> (U snooze-struct #f)
    (define/public (revert-delta! guid delta struct)
      (unless (equal? guid (audit-delta-guid delta))
        (raise-exn exn:fail:snooze
          (format "Delta does not apply to the correct GUID: ~s ~s" guid delta)))
      (if (eq? (audit-delta-type delta) 'I)
          (begin #f)
          (let ([struct (if struct struct (make-snooze-struct/defaults (guid-entity guid)))]
                [attr   (id->attribute (audit-delta-attribute-id delta))])
            (unless (struct-id struct)
              (set-struct-id! struct (audit-delta-struct-id delta)))
            (unless (struct-revision struct)
              (set-struct-revision! struct (audit-delta-struct-revision delta)))
            (set-struct-attribute! struct (attribute-name attr) (audit-delta-value delta (attribute-type attr)))
            struct)))
    
    ; Helpers --------------------------------------
    
    ; audit-transaction (U 'U 'D) guid integer attribute any -> audit-delta
    (define (make-update/delete-delta txn type guid revision attr value)
      (define entity-id (entity->id (guid-entity guid)))
      (define id        (guid-id guid))
      (define attr-id   (attribute->id attr))
      (define attr-type (attribute-type attr))
      (apply make-audit-delta
             (struct-id txn) ; transaction-id
             type            ; type
             entity-id       ; entity-id
             id              ; struct-id
             revision        ; struct-revision
             attr-id         ; attribute-id
             (expand-value attr-type value)))
    
    ;  ( type
    ;    (U boolean integer real string symbol time-tai time-utc)
    ; -> 
    ;    (list boolean (U integer #f) (U real #f) (U string #f) (U time-utc #f)) )
    (define (expand-value type value)
      (if type
          (list (if (boolean-type? type) value #f)
                (if (integer-type? type) value #f)
                (if (real-type? type) value #f)
                (cond [(string-type? type) value]
                      [(symbol-type? type) (if value (symbol->string value) value)]
                      [else #f])
                (cond [(time-tai-type? type) (if value (time-tai->time-utc value) value)]
                      [(time-utc-type? type) value]
                      [else #f]))
          (list #f #f #f #f #f)))
    
    (inspect #f)))
    
; Provide statements -----------------------------

(provide (snooze-struct-out audit-delta)
         delta-api<%>
         delta-api%)
