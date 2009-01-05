(module test-base mzscheme
  
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
  
  (require (file "base.ss"))

  ; Provide statements --------------------------- 
  
  (provide (all-from (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
           (all-from (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))))
  
  (provide (all-from (file "base.ss")))
  
  )


           