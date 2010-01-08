;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module all-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 7))
           (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2 7))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 7)))
  (require "config.ss"
           "connection.ss"
           "sql-types.ss"
           "concurrent.ss")

  (provide all-tests)
  (define all-tests
    (test-suite "All spgsql tests"
      query-test
      sql-types-test
      concurrent-test)))

