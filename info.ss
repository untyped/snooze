#lang setup/infotab

(define name "snooze")

(define blurb 
  '((p "An Object Relational Mapping (ORM) system. Snooze lets you define special PLT structs called " 
       (tt "persistent-structs") " and serialize them to an SQLite or PostgreSQL database.")))

(define release-notes
  '((p "Changes:")
    (ul (li "argumentless " (tt "and") " and " (tt "or") " are supported;")
        (li "added extra functions to the check library;")
        (li "added extra information to " (tt "persistent-struct-info") ", supporting simple development of more advanced persistent struct related macros."))))

(define primary-file "main.ss")

(define url "http://svn.untyped.com/snooze/")

(define categories '(devtools io))

(define scribblings '(("scribblings/snooze.scrbl" (multi-page))))

(define required-core-version "4.0.2.5")

(define repositories '("4.x"))

