#lang scheme/base

(require (file "../test-base.ss")
         (file "../test-data.ss")
         (file "../test-util.ss")
         (file "annotation.ss"))

; Tests ------------------------------------------

(define annotation-tests
  (test-suite "annotation.ss"
    
    (test-equal? "ann:pretty-name : initial value"
      (entity-annotation entity:course ann:pretty-name)
      "course")
    
    (test-equal? "ann:pretty-name : set value"
      (begin (set-entity-annotation! entity:course ann:pretty-name "COURSE")
             (entity-annotation entity:course ann:pretty-name))
      "COURSE")
    
    (test-equal? "ann:pretty-name : overwrite value"
      (begin (set-entity-annotation! entity:course ann:pretty-name "Course")
             (entity-annotation entity:course ann:pretty-name))
      "Course")
    
    (test-case "ann:pretty-name : set value for entity and attribute"
      (set-attribute-annotation! attr:course-name ann:pretty-name "Course name")
      (check-equal? (entity-annotation entity:course ann:pretty-name) "Course")
      (check-equal? (attribute-annotation attr:course-name ann:pretty-name) "Course name"))))

; Provide statements -----------------------------

(provide annotation-tests)