#lang scheme/base
  
(require "snooze.ss")

(define-persistent-struct person
  ([name type:string])
  #:table-name 'Person)

(define-persistent-struct pet
  ([owner-id type:integer #:column-name 'ownerID]
   [name     type:string])
  #:table-name 'Pet)

(define-persistent-struct course
  ([code   type:symbol]
   [name   type:string]
   [value  type:integer]
   [rating type:real]
   [active type:boolean]
   [start  type:time-tai])
  #:table-name 'Course)

; Provide statements -----------------------------

(provide (persistent-struct-out person)
         (persistent-struct-out pet)
         (persistent-struct-out course))
