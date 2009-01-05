(module extract-sig mzscheme
  
  (require (lib "unitsig.ss"))
  
  (provide extract^)
  
  (define-signature extract^
    (;; make-struct-extractor 
     ;;     : (list-of entity) 
     ;;    -> ((U (vector-of scheme-primitive) #f) -> (U (list-of (U persistent-struct scheme-primitive)) #f))
     ;;
     ;; Creates a procedure that extracts persistent-structs from a vector of scheme primitives.
     make-struct-extractor))
  
  )
 