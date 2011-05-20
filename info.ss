#lang setup/infotab

(define name "snooze")

(define blurb 
  '((p "Object Relational Mapping (ORM) for Racket. Snooze lets you define special structs called " 
       (tt "snooze-structs") " and serialize them to an SQLite or PostgreSQL database.")))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "complete me..."))))

(define primary-file "main.ss")

(define url "http://svn.untyped.com/snooze/")

(define categories '(devtools io))

;(define scribblings '(("scribblings/snooze.scrbl" (multi-page))))

(define required-core-version "4.2.2")

(define repositories '("4.x"))

(define compile-omit-paths
  '("autoplanet.ss"
    "build.ss"
    "planet"
    "planetdev"))