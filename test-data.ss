#lang scheme/base

(require "test-base.ss")

(require "era/era.ss")

(define-entity person
  ([name string])
  #:table-name 'people)

(define-entity pet
  ([owner-id integer #:column-name 'ownerID]
   [name     string])
  #:table-name 'pets)

(define-entity course
  ([code   symbol]
   [name   string]
   [value  integer]
   [rating real]
   [active boolean]
   [start  time-tai])
  #:table-name 'courses)

; Provide statements -----------------------------

(provide (entity-out person)
         (entity-out pet)
         (entity-out course))
