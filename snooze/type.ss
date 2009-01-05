(module type mzscheme
  
  (require (lib "time.ss" "srfi" "19"))
  
  (require (file "base.ss"))
  
  (provide type?
           type-name
           type-null
           type:id
           type:revision
           type:text
           type:integer
           type:real
           type:symbol
           type:boolean
           type:time-tai)
  
  ;; struct type : symbol any
  ;;
  ;; The "null" attribute represents the null value of the attribute.
  ;; Currently, this is #f for all types.
  (define-struct type (name null) #f)
  
  ; These objects represent the types that can be specified for
  ; attributes of persistent structs:
  
  (define type:id       (make-type 'id       #f))
  (define type:revision (make-type 'revision #f))
  (define type:text     (make-type 'text     #f))
  (define type:integer  (make-type 'integer  #f))
  (define type:real     (make-type 'real     #f))
  (define type:symbol   (make-type 'symbol   #f))
  (define type:boolean  (make-type 'boolean  #f))
  (define type:time-tai (make-type 'time-tai #f)) ; as defined in SRFI 19

  )
