#lang scheme/base

(require "base.ss")

(require (prefix-in srfi: srfi/19)
         (unlib-in enumeration)
         "main.ss")

(define-entity person
  ([name     string])
  #:plural people
  #:pretty-formatter
  (lambda (person)
    (person-name person)))

(define-entity pet
  ([owner    person]
   [name     string])
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
             #:default 1
             #:min-value 0
             #:max-value 5]
   [rating   real
             #:min-value 0.0
             #:max-value 1.0]
   [active?  boolean
             #:column-name 'active
             #:pretty-name "active flag"]
   [start    time-tai
             #:default (srfi:current-time srfi:time-tai)]))

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

(provide color)

(provide/contract/entities
 [drop-all-tables      (-> void?)]
 [recreate-test-tables (-> void?)]
 [entity               person]
 [entity               pet]
 [entity               course]
 [entity               tree-node])
