(module test-data mzscheme
  
  (require (lib "contract.ss")
           (lib "pregexp.ss")
           (lib "time.ss" "srfi" "19"))
  
  (require (file "base.ss")
           (file "era.ss")
           (file "persistent-struct.ss")
           (file "type.ss"))
  
  ; Field types ----------------------------------
 
  (define type:integer/1
    (set-type-initial type:integer 1))
 
  (define type:real/1
    (set-type-initial type:real 1))
  
  (define type:boolean/t
    (set-type-initial type:boolean #t))
   
  ; Persistent struct types ----------------------
  
  (define/provide-persistent-struct course 
    ((code   type:symbol) 
     (name   type:text)
     (value  type:integer/1)
     (rating type:real)
     (active type:boolean/t)
     (start  type:time-tai))) ; test pipelineless syntax
  
  (define/provide-persistent-struct programme 
    ((code type:symbol) 
     (name type:text))
    ())                       ; test pipelinesome syntax
  
  (define/provide-persistent-struct programme-structure 
    ((course-code    type:symbol) 
     (programme-code type:symbol) 
     (year           type:integer/1) 
     (compulsory     type:boolean/t)))
  
  ; Utility functions ----------------------------
  
  ;; string->time-tai : string -> time-tai
  ;;
  ;; String must be in "yyyy-mm-dd hh:mm:ss" format.
  (define (string->time-tai str)
    (date->time-tai (string->date (string-append str "+0000") "~Y-~m-~d ~H:~M:~S~z")))
  
  ;; string->time-tai : string -> time-utc
  ;;
  ;; String must be in "yyyy-mm-dd hh:mm:ss[+/-]hhmm" format.
  (define (string->time-utc str)
    (date->time-utc (string->date (string-append str "+0000") "~Y-~m-~d ~H:~M:~S~z")))
  
  ; Test data ------------------------------------
  
  (define time1 (string->time-tai "2001-01-01 01:01:01"))
  (define time2 (string->time-tai "2002-02-02 02:02:02"))
  (define time3 (string->time-tai "9999-01-01 01:01:01"))
  
  (define unstored-course 
    (make-course 'SBS-101 "Biology 101" 2 2.5 #t time1))
  
  (define stored-course 
    (let ([course (make-course 'SBS-101 "Biology 101" 2 2.5 #t time2)])
      (set-id! course 0)
      (set-revision! course 1000)
      course))
  
  (define unstored-programme-structure 
    (make-programme-structure 'SBS-101 'BIO1 1 #t))
  
  (define stored-programme-structure 
    (let ([structure (make-programme-structure 'SBS-101 'BIO1 1 #t)])
      (set-id! structure 0)
      (set-revision! structure 1000)
      structure))

  ; Provide statements --------------------------- 
  
  ; Persistent struct types provided above
  
  (provide type:integer/1
           type:real/1
           type:boolean/t
           time1
           time2
           time3
           unstored-course
           stored-course
           unstored-programme-structure
           stored-programme-structure)
  
  ;; yyy-mm-dd-hh-mm-ss-string : string -> boolean
  (define (yyyy-mm-dd-hh-mm-ss-string? str)
    (and (string? str) 
         (pregexp-match "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}" str)))
  
  (provide/contract
   (string->time-tai (-> yyyy-mm-dd-hh-mm-ss-string? time?))
   (string->time-utc (-> yyyy-mm-dd-hh-mm-ss-string? time?)))
  
  )