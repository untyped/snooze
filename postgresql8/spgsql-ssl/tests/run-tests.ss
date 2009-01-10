(require
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
 (file "all-tests.ss"))

(print-struct #t)
(test/text-ui all-tests)