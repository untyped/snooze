#lang scheme/base

(require mzlib/etc
         (only-in srfi/43/vector-lib s:vector->list))

(require "base.ss"
         "era/era.ss"
         "sql/sql.ss")

(provide make-struct-extractor)

;  (U (listof (U entity type)) entity type)
; ->
;   ((U (vectorof scheme-primitive) #f)
;   ->
;    (U (listof (U persistent-struct scheme-primitive))
;       persistent-struct
;       scheme-primitive)))
;
; Creates a procedure that extracts persistent-structs from a vector of scheme primitives.
(define (make-struct-extractor extract-info)
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
  (let ([cache (make-hasheq)])
    (if single-item?
        (lambda (source)
          (if source
              (car (row->structs source 0 entities cache))
              #f))
        (lambda (source)
          (if source
              (row->structs source 0 entities cache)
              #f)))))

;  (-> (vectorof scheme-primitive)
;      integer
;      (listof (U entity #f))
;      (hashof entity (hashof integer persistent-struct))
;      (listof (U persistent-struct scheme-primitive)))
;
; Loads a list of persistent-structs, created from the data stored from
; start-col to the end of row. The types of the persistent-structs are
; determined by the supplied list of entities.
;
; Rersistent structs are cached when they are encountered for the first time
; if the same record is discovered again later on in the result set, the cached
; structure is re-used.
(define (row->structs row start-col entities cache)
  (cond [(null? entities) null]
        [(entity? (car entities))
         (begin-with-definitions
           (define entity (car entities))
           (define-values (struct end-col)
             (row->struct row start-col entity cache))
           (cons struct (row->structs row end-col (cdr entities) cache)))]
        [else (cons (vector-ref row start-col)
                    (row->structs row
                                  (add1 start-col)
                                  (cdr entities)
                                  cache))]))

;  (vectorof scheme-primitive)
;  (listof entity)
;  (hashof entity (hashof integer persistent-struct))
; ->
;  (listof (U persistent-struct #f))
;  integer
;
; Loads a persistent-struct, created from the relevant amount of data stored from
; start-col of row. The type of the persistent-struct is determined by the supplied 
; entity.
;
; If the struct is already in the supplied cache, the cached version is returned.
; Otherwise, the struct is created from scractch and cached before it is returned.
;
; The procedure returns two values: the struct, and the index in row at which the
; ID of the next struct can be found.
(define (row->struct row start-col entity cache)
  (begin-with-definitions
    (unless (< start-col (vector-length row))
      (raise-exn exn:fail:snooze
        (format "Start column index too high: ~a ~a" start-col row)))
    (define id      (vector-ref row start-col))
    (define end-col (+ start-col (length (entity-attributes entity))))
    (define cached  (cache-ref cache entity id))
    (values (cond [(not id) #f]
                  [cached   cached]
                  [else     (cache-put! 
                             cache 
                             entity
                             id
                             (apply (entity-constructor entity)
                                    (s:vector->list row start-col end-col)))])
            end-col)))

; Cache for rows->structs ------------------

;  (hashof entity (hashof integer persistent-struct))
;  entity
;  (U integer #f)
; ->
;  (U persistent-struct #f)
;
; Tries to retrieve a persistent struct from the cache. Returns #f if
; the struct is not cached.
(define (cache-ref cache entity id)
  (if id
      (let ([subtable (cache-subtable cache entity)])
        (hash-ref subtable id #f))
      #f))

;  (hashof entity (hashof integer persistent-struct))
;  entity
;  integer
;  persistent-struct
; ->
;  persistent-struct
;
; Caches and returns a (newly created) persistent struct.
(define (cache-put! cache entity id struct)
  (define subtable 
    (cache-subtable cache entity))
  (hash-set! subtable id struct)
  struct)

;  (hashof entity (hashof integer persistent-struct))
; ->
;  (hashof integer persistent-struct)
;
; Utility procedure. Used in cache-ref and cache-put!.
(define (cache-subtable cache entity)
  (hash-ref 
   cache
   entity
   (lambda ()
     (let ([subtable (make-hasheq)])
       (hash-set! cache entity subtable)
       subtable))))
