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
    '("Added default values for fields and functional updating of persistent structs... "
      "and a little (just a little) documentation."))

  (define primary-file "snooze.ss")
  
  (define url "http://svn.untyped.com/snooze/")

  (define doc.txt "doc.txt")
  
  (define categories '(devtools)) ; TODO : Fix these
  
  (define scribblings '(("doc/index.scrbl" (multi-page))))

  )
