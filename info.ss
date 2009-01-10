#lang setup/infotab

(define name "snooze")

(define blurb 
  '((p "An Object Relational Mapping (ORM) system. "
       "Snooze lets you define special MzScheme structs called " 
       (tt "persistent-structs") " and serialize them to an SQLite "
       "or PostgreSQL database.")))

(define release-notes
  '((p "Fill me in!")))

(define primary-file "snooze.ss")

(define url "http://svn.untyped.com/snooze/")

(define categories '(devtools io))

(define scribblings '(("scribblings/snooze.scrbl" (multi-page))))

(define required-core-version "4.0.2.5")

(define repositories '("4.x"))


