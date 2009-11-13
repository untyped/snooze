#lang scheme/base

(require (for-syntax scheme/base)
         "../base.ss")

(require "check.ss"
         "check-annotation.ss"
         "check-default.ss"
         "check-result.ss"
         "check-syntax.ss"
         (except-in "struct.ss"
                    make-entity
                    make-numeric-type
                    make-character-type
                    make-temporal-type
                    make-check-success
                    make-check-problem
                    make-check-warning
                    make-check-error
                    make-check-failure
                    make-check-fatal)
         "define-entity.ss"
         "provide-entity.ss"
         "snooze-struct.ss"
         "syntax-info.ss"
         "transaction.ss")

; Syntax -----------------------------------------

; snooze-struct snooze-struct -> boolean
(define-syntax snooze=?
  (make-rename-transformer #'snooze-struct-equal?))

; snooze-struct snooze-struct -> boolean
(define-syntax snooze-guid=?
  (make-rename-transformer #'snooze-struct-guid-equal?))

; snooze-struct snooze-struct -> boolean
(define-syntax snooze-data=?
  (make-rename-transformer #'snooze-struct-data-equal?))

; Provide statements ---------------------------

(provide snooze=?
         snooze-guid=?
         snooze-data=?
         (all-from-out "check.ss"
                       "check-annotation.ss"
                       "check-default.ss"
                       "check-result.ss"
                       "check-syntax.ss"
                       "struct.ss"
                       "define-entity.ss"
                       "provide-entity.ss"
                       "snooze-struct.ss"
                       "syntax-info.ss"
                       "transaction.ss"))

