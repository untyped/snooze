#lang scheme/base

(require "../base.ss")

(require (for-syntax scheme/base
                     (cce-scheme-in syntax))
         scheme/dict
         scheme/list
         "../core/core.ss"
         "../sql/sql.ss"
         "../sql/sql-struct.ss"
         "../sql/sql-util.ss"
         "interface.ss")

; Helpers ----------------------------------------

; Version of for/fold that accumulates two lists and N other accumulators.
; The final return value is the reverse of each of the two lists.
; The remaining accumulators are discarded.
(define-syntax (for/fold2/reverse stx)
  (syntax-case stx ()
    [(_ (accum ...) (seq ...) expr ...)
     (with-syntax* ([(temp ...) (generate-temporaries #'(accum ...))]
                    [ans1       (car  (syntax->list #'(temp ...)))]
                    [ans2       (cadr (syntax->list #'(temp ...)))])
       (syntax/loc stx
         (let-values ([(temp ...) (for/fold (accum ...) (seq ...) expr ...)])
           (values (reverse ans1)
                   (reverse ans2)))))]))

; Mixins -----------------------------------------

(define generic-cross-reference-mixin
  (mixin (generic-database<%>) (cross-reference<%>)
    
    (inspect #f)
    
    (inherit get-snooze)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; query -> (query-result -> query-result)
    (define/public (make-query-cross-referencer query)
      (make-cross-referencer
       (query-what query)
       (query-extract-info query)
       (source->foreign-keys (query-from query))))
    
    ;  (listof column)
    ;  (U symbol #f (listof (U symbol #f)))
    ;  (hashof column column)
    ; -> 
    ;  (query-result -> query-result)
    (define/public (make-cross-referencer cols info xrefs)
      ; Pass throughs: there's NEVER any cross-referencing to do if:
      ;   - we're not selecting more than one entity;
      ;   - there are no cross references in the FROM clause.
      (if (or (<= (count-entities info) 1)
              (zero? (dict-count xrefs)))
          ; Create a dummy cross-referencer that does nothing:
          (lambda (item frame) item)
          ; Create a real cross-referencer:
          (let*-values ([(sizes) (entities->sizes info)]
                        ; Mask out any primary and foreign keys that aren't part of an extracted snooze-struct:
                        [(cols)  (entities->mask info cols)]
                        [(mutators local-indices remote-indices)
                         (for/fold ([mutators null] [local-indices null] [remote-indices null])
                                   ([fk (in-list cols)])
                                   (let* ([pk       (and fk (hash-ref xrefs fk #f))]
                                          [fk-index (and pk (column->struct-index fk cols sizes))]
                                          [pk-index (and pk (column->struct-index pk cols sizes))])
                                     (if (and fk-index pk-index)
                                         (values (cons (attribute-mutator (attribute-alias-attribute fk)) mutators)
                                                 (cons fk-index local-indices)
                                                 (cons pk-index remote-indices))
                                         (values mutators local-indices remote-indices))))])
            (lambda (item frame)
              (for ([mutator (in-list mutators)]
                    [local   (in-list local-indices)]
                    [remote  (in-list remote-indices)])
                (let ([local  (list-ref item local)]
                      [remote (list-ref item remote)])
                  (when (and local remote)
                    (mutator local remote))))
              item))))))

; Helpers ----------------------------------------

; (listof (U symbol #f)) [natural] -> natural
(define (count-entities info [accum 0])
  (cond [(pair? info)
         (if (car info)
             (count-entities (cdr info) (add1 accum))
             (count-entities (cdr info) accum))]
        [(null? info) accum]
        [info 1]
        [else 0]))

; (listof (U symbol #f)) -> (listof (U column #f))
;
; Returns a mask of booleans over the query's columns:
; any columns not involved in entities are masked out with #f.
;
; We need this to avoid cross-referencing columns that aren't inside entities.
; See the test case "make-cross-referencer: mixture of entities and single columns" for more information.
(define (entities->mask info cols)
  (map (cut and <> <>)
       (append-map (lambda (info)
                     (if info
                         (make-list (length (entity-attributes (model-entity (current-model) info))) #t)
                         (list #f)))
                   info)
       cols))

; (listof (U symbol #f)) -> (listof integer)
; Works out the size of each item in a result row.
(define (entities->sizes info)
  (map (lambda (info)
         (if info
             (length (entity-attributes (model-entity (current-model) info)))
             1))
       info))

; column (listof column) (listof integer) -> (U natural #f)
; Works out which struct any given column is inside.
(define (column->struct-index col cols sizes)
  (let loop ([index 0] [cols cols] [sizes sizes])
    (cond [(null? cols)            #f] ; not found in the WHAT clause
          [(null? sizes)           #f] ; not found in the WHAT clause
          [(zero? (car sizes))     (loop (add1 index) cols (cdr sizes))]
          [(equal? col (car cols)) index]
          [else                    (loop index (cdr cols) (cons (sub1 (car sizes))
                                                                (cdr sizes)))])))

; Provide statements -----------------------------

(provide generic-cross-reference-mixin)
