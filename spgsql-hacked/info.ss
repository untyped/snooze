;; Copyright 2000-2008 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module info (lib "infotab.ss" "setup")
  (define name "spgsql")
  (define compile-omit-files '("samples/sample1.ss"))
  (define blurb
    '("The spgsql library provides a high-level interface to PostgreSQL "
      "database servers. It is implemented in Scheme and requires "
      "no C client libraries."))
  (define primary-file "spgsql.ss")
  (define homepage "http://schematics.sourceforge.net/spgsql.html")
  (define version "5.3")
  (define doc.txt "doc.txt")
  (define categories '(net))
  (define can-be-loaded-with 'all)
  (define required-core-version "371.3")
  (define release-notes
    '("Spgsql 5.2 adds compatibility with PLT Scheme 3.99 "
      "and maintains compatibility with PLT Scheme 37X.")))
