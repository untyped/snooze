#lang scheme/base

(require "../base.ss")

(require scheme/list
         "../era/era.ss"
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
      (let ([extract-info (query-extract-info query)])
        (if (or (pair? extract-info) (null? extract-info))
            (make-multiple-item-extractor
             (list (and (entity? extract-info) extract-info)))
            (make-single-item-extractor
             (map (lambda (item) (and (entity? item) item))
                  (list extract-info))))))
    
    ; (U entity #f) -> single-item-extractor
    ; where single-item-extractor : (U (listof scheme-primitive) #f) -> (U snooze-struct scheme-primitive)
    (define/public (make-single-item-extractor entity)
      (lambda (source)
        (and source (car (row->structs source (list entity))))))
    
    ; (listof (U entity #f)) -> multiple-item-extractor
    ; where multiple-item-extractor : (U (listof scheme-primitive) #f) -> (listof (U snooze-struct scheme-primitive))
    (define/public (make-multiple-item-extractor entities)
      (lambda (source)
        (and source (row->structs source entities))))
    
    ; (listof scheme-primitive) (listof (U entity #f)) -> (listof (U snooze-struct scheme-primitive))
    ;
    ; Loads a list of snooze-structs, created from the data stored from
    ; start-col to the end of row. The types of the snooze-structs are
    ; determined by the supplied list of entities.
    ;
    ; Persistent structs are cached when they are encountered for the first time
    ; if the same record is discovered again later on in the result set, the cached
    ; structure is re-used.
    (define (row->structs row entities)
      (cond [(null? entities) null]
            [(entity? (car entities))
             (let*-values ([(entity)      (car entities)]
                           [(struct rest) (row->struct row entity)])
               (cons struct (row->structs rest (cdr entities))))]
            [else (cons (car row) (row->structs (cdr row) (cdr entities)))]))
    
    ;  (listof scheme-primitive)
    ;  (listof (U entity type))
    ; ->
    ;  (listof (U snooze-struct #f))
    ;  (listof scheme-primitive)
    ;
    ; Loads a snooze-struct, either from the cache or from created from the first N
    ; columns of the row.
    ;
    ; The procedure returns two values: the struct, and the rest of the row following
    ; the struct's attributes.
    (define (row->struct row entity)
      (printf "row->struct~n")
      (let* ([cache     (send (get-snooze) get-current-cache)]
             [guid      (car row)]
             [num-attrs (length (entity-attributes entity))])
        (unless (or (not guid) (and (guid? guid) (not (guid-local? guid))))
          (raise-type-error 'row->struct "(U vanilla-guid #f)" guid))
        (values (and guid                                  ; if id is #f, struct is null
                     (or (send cache get-local-alias guid) ; if struct is cached (locally or in an ancestor cache), return a new local guid
                         (send cache add-struct!           ; else add the new struct to the cache, and return a new local guid
                               (apply (entity-private-constructor entity)
                                      guid
                                      (cdr (take row num-attrs))))))
                (drop row num-attrs))))))

; Provide statements -----------------------------

(provide generic-extract-mixin)
