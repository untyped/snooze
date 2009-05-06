#lang scheme/base

(require "test-base.ss")

(require "main.ss")

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

; Procedures -------------------------------------

; -> void
(define (drop-all-tables)
  (when (table-exists? pet)    (drop-table pet))
  (when (table-exists? person) (drop-table person))
  (when (table-exists? course) (drop-table course))
  (for-each drop-table (table-names)))

; -> void
(define (recreate-test-tables)
  (if (table-exists? pet)
      (for-each delete! (find-all (sql (select #:from pet))))
      (create-table pet))
  (if (table-exists? person)
      (for-each delete! (find-all (sql (select #:from person))))
      (create-table person))
  (if (table-exists? course)
      (for-each delete! (find-all (sql (select #:from course))))
      (create-table course)))
  
; Provide statements -----------------------------

(provide (entity-out person)
         (entity-out pet)
         (entity-out course))

(provide/contract
 [drop-all-tables      (-> void?)]
 [recreate-test-tables (-> void?)])
