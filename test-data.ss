#lang scheme/base

(require "test-base.ss")

(require "era/era.ss")

(define-entity person
  ([name type:string])
  #:table-name 'Person)

(define-entity pet
  ([owner-id type:integer #:column-name 'ownerID]
   [name     type:string])
  #:table-name 'Pet)

(define-entity course
  ([code   type:symbol]
   [name   type:string]
   [value  type:integer]
   [rating type:real]
   [active type:boolean]
   [start  type:time-tai])
  #:table-name 'Course)

; Provide statements -----------------------------

(provide (entity-out person)
         (entity-out pet)
         (entity-out course))
