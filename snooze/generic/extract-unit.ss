(module extract-unit mzscheme
  
  (require (lib "etc.ss")
           (lib "unitsig.ss")
           (lib "list.ss" "srfi" "1")
           (lib "cut.ss" "srfi" "26")
           (lib "vector-lib.ss" "srfi" "43"))
  
  (require (planet "list.ss" ("untyped" "unlib.plt" 2))
           (planet "profile.ss" ("untyped" "unlib.plt" 2))
           (only (planet "project.ss" ("untyped" "unlib.plt" 2)) partition/mask)
           (planet "yield.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "../base.ss")
           (file "../era.ss")
           (file "../type.ss")
           (file "extract-sig.ss")
           (file "sql-sig.ss"))
  
  (provide extract@)
  
  (define extract@
    (unit/sig extract^
      (import sql-quote^)

      ;; make-struct-extracter 
      ;;     : (list-of (U entity #f))
      ;;       boolean
      ;;    -> ((U (vector-of scheme-primitive) #f) 
      ;;        -> (U (list-of (U persistent-struct scheme-primitive))
      ;;              persistent-struct
      ;;              scheme-primitive
      ;;              g:end)
      ;;
      ;; Creates a procedure that extracts persistent-structs from a vector of scheme primitives.
      (define (make-struct-extractor entities single-item?)
        (let ([cache (make-hash-table)])
          (if single-item?
              (lambda (source)
                (if source
                    (car (row->structs source 0 entities cache))
                    #f))
              (lambda (source)
                (if source
                    (row->structs source 0 entities cache)
                    #f)))))
      
      ;; row->structs
      ;;     : (vector-of scheme-primitive)
      ;;       integer
      ;;       (list-of (U entity #f))
      ;;       (hash-table-of entity (hash-table-of integer persistent-struct))
      ;;    -> (list-of (U persistent-struct scheme-primitive))
      ;;
      ;; Loads a list of persistent-structs, created from the data stored from
      ;; start-col to the end of row. The types of the persistent-structs are
      ;; determined by the supplied list of entities.
      ;;
      ;; Rersistent structs are cached when they are encountered for the first time
      ;; if the same record is discovered again later on in the result set, the cached
      ;; structure is re-used.
      (define (row->structs row start-col entities cache)
        (cond [(null? entities)
               null]
              [(entity? (car entities))
               (let*-values ([(entity)         (car entities)]
                             [(struct end-col) (row->struct row start-col entity cache)])
                 (cons struct (row->structs row end-col (cdr entities) cache)))]
              [else (cons (vector-ref row start-col)
                          (row->structs row
                                        (add1 start-col)
                                        (cdr entities)
                                        cache))]))
      
      ;; row->struct
      ;;     : (vector-of scheme-primitive)
      ;;       (list-of entity)
      ;;       (hash-table-of entity (hash-table-of integer persistent-struct))
      ;;    -> (values (list-of (U persistent-struct #f))
      ;;               integer)
      ;;
      ;; Loads a persistent-struct, created from the relevant amount of data stored from
      ;; start-col of row. The type of the persistent-struct is determined by the supplied 
      ;; entity.
      ;;
      ;; If the struct is already in the supplied cache, the cached version is returned.
      ;; Otherwise, the struct is created from scractch and cached before it is returned.
      ;;
      ;; The procedure returns two values: the struct, and the index in row at which the
      ;; ID of the next struct can be found.
      (define (row->struct row start-col entity cache)
        (unless (< start-col (vector-length row))
          (raise-exn exn:fail:snooze
            (format "Start column index too high: ~a ~a" start-col row)))
        (let* ([id      (vector-ref row start-col)]
               [end-col (+ start-col (length (entity-fields entity)))]
               [cached  (cache-ref cache entity id)])
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
      
      ;; cache-ref
      ;;     : (hash-table-of entity (hash-table-of integer persistent-struct))
      ;;       entity
      ;;       (U integer #f)
      ;;    -> (U persistent-struct #f)
      ;;
      ;; Tries to retrieve a persistent struct from the cache. Returns #f if
      ;; the struct is not cached.
      (define (cache-ref cache entity id)
        (if id
            (let ([subtable (cache-subtable cache entity)])
              (hash-table-get subtable id (lambda () #f)))
            #f))
      
      ;; cache-put!
      ;;     : (hash-table-of entity (hash-table-of integer persistent-struct))
      ;;       entity
      ;;       integer
      ;;       persistent-struct
      ;;    -> persistent-struct
      ;;
      ;; Caches and returns a (newly created) persistent struct.
      (define (cache-put! cache entity id struct)
        (define subtable 
          (cache-subtable cache entity))
        (hash-table-put! subtable id struct)
        struct)
      
      ;; cache-subtable 
      ;;     : (hash-table-of entity (hash-table-of integer persistent-struct))
      ;;    -> (hash-table-of integer persistent-struct)
      ;;
      ;; Utility procedure. Used in cache-ref and cache-put!.
      (define (cache-subtable cache entity)
        (hash-table-get 
         cache
         entity
         (lambda ()
           (let ([subtable (make-hash-table)])
             (hash-table-put! cache entity subtable)
             subtable))))
      
      ))
  
  )
 