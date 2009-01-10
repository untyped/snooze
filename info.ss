(module info (lib "infotab.ss" "setup")
  (define name "unlib")

  (define compile-omit-files
    '("doc"))

  (define blurb 
    '((p "An Object Relational Mapping (ORM) system from Untyped. "
         "Snooze lets you define special MzScheme structs called " 
         (tt "persistent-structs") " and serialize them to an SQLite "
         "or PostgreSQL database.")))

  (define release-notes
    '("Initial PLaneT release."))

  (define primary-file "snooze.ss")
  
  (define url "http://svn.untyped.com/snooze/")

  (define version "351")

  (define doc.txt "doc.txt")
  
  (define categories '(devtools)) ; TODO : Fix these
  
  ; (define scribblings '(("doc/index.scrbl" (multi-page))))

  )
