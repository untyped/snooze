#lang setup/infotab

(define name "snooze")

(define blurb 
  '((p "An Object Relational Mapping (ORM) system. Snooze lets you define special PLT structs called " 
       (tt "persistent-structs") " and serialize them to an SQLite or PostgreSQL database.")))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "simplified the query system:"
            (ul (li "the macro query language is now the preferred way of writing queries;")
                (li "entity names can now be used in queries;")
                (li "introduced " (tt "entity.attr") " syntax to refer to attributes;")
                (li "special attribute aliases are no longer required;")))
        (li "simplified accessing entity/attribute metadata:"
            (ul (li "the entity name " (tt "foo") " can now be used instead of " (tt "entity:foo") ";")
                (li "the syntax " (tt "(attr entity attr-id)") " has been added to replace "
                    (tt "attr:entity-attr-id") ";")))
        (li "enhancements to the quick-find system:"
            (ul (li "quick-find procedures now accept " (tt "#:what") ", " (tt "#:order") ", " 
                    (tt "#:limit") " and " (tt "#:offset") " arguments;")))
        (li "documented the " (tt "#:ssl") " option for PostgreSQL in the Quick Start, "
            "including a note about the lower speed of SSL-enabled connections;")
        (li "removed extraneous provides from " (tt "snooze.ss") "."))))

(define primary-file "main.ss")

(define url "http://svn.untyped.com/snooze/")

(define categories '(devtools io))

(define scribblings '(("scribblings/snooze.scrbl" (multi-page))))

(define required-core-version "4.0.2.5")

(define repositories '("4.x"))

