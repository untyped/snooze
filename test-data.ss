#lang scheme/base

(require "test-base.ss")

(require "main.ss")

(define-entity person
  ([name     string])
  #:table-name         'people
  #:pretty-name-plural "people"
  #:pretty-formatter
  (lambda (person)
    (person-name person)))

(define-entity pet
  ([owner    person]
   [name     string])
  #:table-name 'pets
  #:pretty-formatter
  (lambda (pet [include-owner? #f])
    (if include-owner?
        (if (pet-owner pet)
            (format "~a's pet ~a"
                    (person-name (pet-owner pet))
                    (pet-name pet))
            (format "Stray animal ~a"
                    (pet-name pet)))
        (pet-name pet))))

(define-entity course
  ([code     symbol]
   [name     string]
   [value    integer]
   [rating   real]
   [active?  boolean
             #:column-name 'active
             #:pretty-name "active flag"]
   [start    time-tai])
  #:table-name 'courses)

(define-entity tree-node
  ([parent tree-node]
   [value  string])
  #:table-name 'treenode)

; Procedures -------------------------------------

; -> void
(define (drop-all-tables)
  (when (table-exists? pet)    (drop-table pet))
  (when (table-exists? person) (drop-table person))
  (when (table-exists? course) (drop-table course))
  (for-each drop-table (table-names)))

; -> void
(define (recreate-test-tables)
  (when (table-exists? pet)
    (drop-table pet))
  (when (table-exists? person)
    (drop-table person))
  (when (table-exists? course)
    (drop-table course))
  (create-table course)
  (create-table person)
  (create-table pet))
  
; Provide statements -----------------------------

(provide (entity-out person)
         (entity-out pet)
         (entity-out course))

(provide/contract
 [drop-all-tables      (-> void?)]
 [recreate-test-tables (-> void?)])
