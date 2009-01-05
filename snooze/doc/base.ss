(module base mzscheme
  
  (require (lib "decode.ss" "scribble")
           (lib "eval.ss" "scribble")
           (lib "manual.ss" "scribble")
           (lib "scheme.ss" "scribble")
           (lib "struct.ss" "scribble")
           (lib "urls.ss" "scribble")
           (lib "icons.ss" "scribblings"))

  (require-for-label (file "db.ss"))

  ; URLs -----------------------------------------
  
  (define url:spgsql.plt
    "http://planet.plt-scheme.org/display.ss?package=spgsql.plt&owner=schematics")
  
  (define url:sqlite.plt
    "http://planet.plt-scheme.org/display.ss?package=sqlite.plt&owner=jaymccarthy")
  
  (define url:unlib.plt
    "http://planet.plt-scheme.org/display.ss?package=unlib.plt&owner=untyped")
  
  ; Procedures -----------------------------------
  
  ;; intmore : string -> margin-note
  ;;
  ;; Creates a margin note pointing to another section in the manual.
  ;; Pilfered from collects/scribblings/guide/guide-utils.ss... yuk yuk yuk.
  (define (intmore tag)
    (apply margin-note (decode-content (list finger (secref tag)))))
  
  ;; extmore : string string -> margin-note
  ;;
  ;; Creates a margin note pointing to an external web site.
  ;; Pilfered from collects/scribblings/guide/guide-utils.ss... yuk yuk yuk.
  (define (extmore url text)
    (apply margin-note (decode-content (list finger (link url text)))))

  ; Provide statements --------------------------- 
  
  (provide (all-from (lib "eval.ss" "scribble"))
           (all-from (lib "manual.ss" "scribble"))
           (all-from (lib "urls.ss" "scribble")))
           
  (provide (all-defined))
  
  (provide-for-label (all-from (file "db.ss")))

  )
