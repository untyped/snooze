#lang scheme/base

(require "../base.ss")

(require scheme/list
         "../core/core.ss"
         "../sql/sql.ss"
         "../sql/sql-struct.ss"
         "interface.ss")

(define generic-extract-mixin
  (mixin (generic-database<%>) (extract<%>)
    
    (inspect #f)
    
    (inherit get-snooze)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; query -> (U single-item-extractor multi-item-extractor)
    (define/public (make-query-extractor query)
      (let ([info (query-extract-info query)])
        (if (or (pair? info) (null? info))
            (make-multiple-item-extractor
             (map (lambda (info)
                    (and info (model-entity (current-model) info)))
                  info))
            (make-single-item-extractor
             (and info (model-entity (current-model) info))))))
    
    ; (U entity #f) -> single-item-extractor
    ; where single-item-extractor
    ;         : (U (listof scheme-primitive) #f)
    ;           (U transaction-frame #f)
    ;          ->
    ;           (U snooze-struct scheme-primitive)
    (define/public (make-single-item-extractor entity)
      (lambda (source frame)
        (and source (car (row->structs source (list entity) frame)))))
    
    ; (listof (U entity #f)) -> multiple-item-extractor
    ; where multiple-item-extractor
    ;         : (U (listof scheme-primitive) #f)
    ;           (U transaction-frame #f)
    ;          ->
    ;           (listof (U snooze-struct scheme-primitive))
    (define/public (make-multiple-item-extractor entities)
      (lambda (source frame)
        (and source (row->structs source entities frame))))
    
    ;  (listof scheme-primitive)
    ;  (listof (U entity #f))
    ;  (U transaction-frame #f)
    ; ->
    ;  (listof (U snooze-struct scheme-primitive))
    ;
    ; Loads a list of snooze-structs, created from the data stored from
    ; start-col to the end of row. The types of the snooze-structs are
    ; determined by the supplied list of entities.
    ;
    ; Persistent structs are cached when they are encountered for the first time
    ; if the same record is discovered again later on in the result set, the cached
    ; structure is re-used.
    (define (row->structs row entities frame)
      (cond [(null? entities) null]
            [(entity? (car entities))
             (let*-values ([(entity)      (car entities)]
                           [(struct rest) (row->struct row entity frame)])
               (cons struct (row->structs rest (cdr entities) frame)))]
            [else (cons (car row) (row->structs (cdr row) (cdr entities) frame))]))
    
    ;  (listof scheme-primitive)
    ;  (listof (U entity type))
    ;  (U transaction-frame #f)
    ; ->
    ;  (listof (U snooze-struct #f))
    ;  (listof scheme-primitive)
    ;
    ; Loads a snooze-struct, either from a temporary query cache or from the first N columns of the row.
    ;
    ; The procedure returns two values: the struct, and the rest of the row following
    ; the struct's attributes.
    (define (row->struct row entity frame)
      (let* ([guid      (car row)]
             [revision  (cadr row)]
             [num-attrs (length (entity-attributes entity))])
        (with-handlers ([exn? (lambda (exn) (error "could not parse entity" row entity exn))])
          (unless (or (database-guid? guid) (not guid))
            (raise-type-error 'row->struct "(U database-guid #f)" guid))
          (if frame
              (values (and guid (or (transaction-frame-cache-ref frame guid)
                                    (transaction-frame-cache-add!
                                     frame
                                     (apply (entity-private-constructor entity)
                                            guid
                                            (cdr (take row num-attrs))))))
                      (drop row num-attrs))
              (values (and guid (apply (entity-private-constructor entity)
                                       guid
                                       (cdr (take row num-attrs))))
                      (drop row num-attrs))))))))
  
  ; Provide statements -----------------------------
  
  (provide generic-extract-mixin)
  