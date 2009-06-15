#lang scheme/base

(require "base.ss")

(require (prefix-in srfi: srfi/19)
         (unlib-in enumeration)
         "main.ss")

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
  ([code     symbol
             #:max-length  8
             #:allow-null? #f
             #:default 'code]
   [name     string
             #:max-length  128
             #:default "name"]
   [value    integer
             #:default 1]
   [rating   real]
   [active?  boolean
             #:column-name 'active
             #:pretty-name "active flag"]
   [start    time-tai
             #:default (srfi:current-time srfi:time-tai)])
  #:table-name 'courses)

(define-enum color (red black))

(define-entity tree-node
  ([parent tree-node]
   [color  enum #:values color]
   [value  string]))

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

(provide-entity/contract person)
(provide-entity/contract pet)
(provide-entity/contract course)
(provide-entity/contract tree-node)

(provide/contract
 [drop-all-tables      (-> void?)]
 [recreate-test-tables (-> void?)])
