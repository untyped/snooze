(module info (lib "infotab.ss" "setup")
  (define name "snooze")

  (define compile-omit-files
    '("doc"))

  (define blurb 
    '((p "An Object Relational Mapping (ORM) system from Untyped. "
         "Snooze lets you define special MzScheme structs called " 
         (tt "persistent-structs") " and serialize them to an SQLite "
         "or PostgreSQL database.")))

  (define release-notes
    '((tt "save!") " and " (tt "delete!") " now return the (mutated) stuct passed to them."))

  (define primary-file "snooze.ss")
  
  (define url "http://svn.untyped.com/snooze/")

  (define doc.txt "doc.txt")
  
  (define categories '(devtools io))
  
  (define scribblings '(("doc/index.scrbl" (multi-page))))

  )
