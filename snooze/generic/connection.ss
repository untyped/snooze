#lang scheme/base

(define-struct connection (back-end in-transaction?) #:transparent #:mutable)

(provide (struct-out connection))
