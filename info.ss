#lang setup/infotab

(define name "snooze")

(define blurb 
  '((p "An Object Relational Mapping (ORM) system. Snooze lets you define special PLT structs called " 
       (tt "persistent-structs") " and serialize them to an SQLite or PostgreSQL database.")))

(define release-notes
  '((p "Changes:")
    (ul (li "corrected typos in the quick start in the docs;")
        (li "updated the contract on the " (tt "make-database") " procedure for the SQLite backend, "
            "to match changes in the contract on the " (tt "open") " procedure in " (tt "sqlite.plt") "."))))

(define primary-file "main.ss")

(define url "http://svn.untyped.com/snooze/")

(define categories '(devtools io))

(define scribblings '(("scribblings/snooze.scrbl" (multi-page))))

(define required-core-version "4.0.2.5")

(define repositories '("4.x"))

