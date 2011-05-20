#lang setup/infotab

(define name "snooze")

(define blurb 
  '((p "An Object Relational Mapping (ORM) system. Snooze lets you define special PLT structs called " 
       (tt "persistent-structs") " and serialize them to an SQLite or PostgreSQL database.")))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "added connection pooling support to PostgreSQL back end;")
        (li "changed audit transaction timestamps to SRFI 19 " (tt "time-utcs") ";")
        (li "added " (tt "query-logger") " parameter;")
        (li "added " (tt "sql-list") " syntax."))))

(define primary-file "main.ss")

(define url "https://github.com/untyped/snooze/")

(define categories '(devtools io))

(define scribblings '(("scribblings/snooze.scrbl" (multi-page))))

(define required-core-version "4.2.2")

(define repositories '("4.x"))

