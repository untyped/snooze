;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module all-tests mzscheme
  (require "bitbang.ss"
           "protocol2.ss"
           "connection.ss"
           "sql-types.ss"
           "exceptions.ss")
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  ;;(require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2)))

  (provide all-tests)
  (define all-tests
    (test-suite
     "All spgsql tests"
     bitbang-test
     protocol-test
     ;;protocol-xfail-test
     message-generator-test
     predicates-test
     connection-test
     extended-sql-test
     sql-types-test
     exception-test
     ))
  )
