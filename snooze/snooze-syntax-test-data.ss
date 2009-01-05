#lang scheme/base

(require mzlib/pretty)

(require "snooze-syntax.ss")

; The interface below won't work because snooze isn't an instance of snooze%.
; However, it will let us check whether the bindings get exported correctly
; to snooze-syntax-test.ss.

(define snooze #f)

(define-snooze-interface provided: snooze)

(provide-snooze-interface provided:)


