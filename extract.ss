#lang scheme/base

(require scheme/list)

(require "base.ss"
         "era/era.ss"
         "sql/sql.ss")

;  (U (listof (U entity type)) entity type)
;  snooze-cache<%>
; ->
;  (  (U (listof scheme-primitive) #f)
;    ->
;     (U (listof (U snooze-struct scheme-primitive))
;        snooze-struct
;        scheme-primitive)))
;
; Creates a procedure that extracts snooze-structs from a list of scheme primitives.
(define (make-struct-extractor extract-info snooze)
  ; (listof (U entity #f))
  ; boolean
  (define-values (entities single-item?)
    (if (or (pair? extract-info) (null? extract-info))
        (values (map (lambda (item)
                       (if (entity? item) 
                           item
                           #f))
                     extract-info)
                #f)
        (values (if (entity? extract-info)
                    (list extract-info)
                    (list #f))
                #t)))
  (if single-item?
      (lambda (source)
        (and source (car (row->structs source entities snooze))))
      (lambda (source)
        (and source (row->structs source entities snooze)))))

;  (listof scheme-primitive)
;  (listof (U entity type))
;  snooze-cache<%>
; ->
;  (listof (U snooze-struct scheme-primitive)))
;
; Loads a list of snooze-structs, created from the data stored from
; start-col to the end of row. The types of the snooze-structs are
; determined by the supplied list of entities.
;
; Persistent structs are cached when they are encountered for the first time
; if the same record is discovered again later on in the result set, the cached
; structure is re-used.
(define (row->structs row entities snooze)
  (cond [(null? entities) null]
        [(entity? (car entities))
         (let*-values ([(entity)      (car entities)]
                       [(struct rest) (row->struct row entity snooze)])
           (cons struct
                 (row->structs rest 
                               (cdr entities)
                               snooze)))]
        [else (cons (car row)
                    (row->structs (cdr row)
                                  (cdr entities)
                                  snooze))]))

;  (listof scheme-primitive)
;  (listof (U entity type))
;  snooze-cache<%>
; ->
;  (listof (U snooze-struct #f))
;  (listof scheme-primitive)
;
; Loads a snooze-struct, either from the cache or from created from the first N
; columns of the row.
;
; The procedure returns two values: the struct, and the rest of the row following
; the struct's attributes.
(define (row->struct row entity snooze)
  (let ([guid      (car row)]
        [num-attrs (length (entity-attributes entity))])
    (values 
     ; If id is #f, struct is null:
     (and guid
          ; If struct is cached return that:
          (or (and (send (send snooze get-current-cache) cache-ref guid)
                   guid)
              ; Else parse the data in the row:
              (send snooze deep-cache-add!
                    (apply (entity-private-constructor entity)
                           guid
                           (cdr (take row num-attrs))))))
     (drop row num-attrs))))

; Provide statements -----------------------------

(provide/contract
 [make-struct-extractor (-> (or/c entity? type? (listof (or/c entity? type?)))
                            (is-a?/c snooze-cache<%>)
                            (-> (or/c pair? null? #f) any))])
