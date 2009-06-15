#lang scheme/base

(require "../base.ss")

(require "../common/connection.ss"
         "cached-struct.ss"
         "check.ss"
         "check-annotation.ss"
         "check-result.ss"
         "check-syntax.ss"
         (except-in "struct.ss"
                    make-check-success
                    make-check-problem
                    make-check-warning
                    make-check-error
                    make-check-failure
                    make-check-fatal))

; Checks -----------------------------------------

; guid -> (listof check-result)
(define (default-check-snooze-struct struct)
  (let ([entity (snooze-struct-entity struct)])
    (check/annotate ([ann:struct struct])
      (apply check-problems
             (for/list ([attr (in-list (cddr (entity-attributes entity)))]
                        [val  (in-list (cddr (snooze-struct-ref* struct)))])
               (check-attribute-value attr val))))))

; guid -> (listof check-result)
(define (default-check-old-snooze-struct struct)
  (check-problems))

; attribute any -> (listof check-result)
(define (check-attribute-value attr val)
  (check/annotate ([ann:attrs (list attr)])
    (let* ([name        (attribute-name attr)]
           [type        (attribute-type attr)]
           [allow-null? (type-allows-null? type)])
      (cond [(equal? val (type-null type))
             (if allow-null?
                 (check-pass)
                 (check-fail (format "~a: value is required."
                                     (string-titlecase (attribute-pretty-name attr)))))]
            [(boolean-type? type)
             (if (boolean? val)
                 (check-pass)
                 (check-fail (format "~a: must be a yes/no value."
                                     (string-titlecase (attribute-pretty-name attr)))))]
            [(integer-type? type)
             (if (integer? val)
                 (check-pass)
                 (check-fail (format "~a: must be~a a whole number."
                                     (string-titlecase (attribute-pretty-name attr))
                                     (if allow-null? " blank or" ""))))]
            [(real-type? type)
             (if (real? val)
                 (check-pass)
                 (check-fail (format "~a: must be~a a number."
                                     (string-titlecase (attribute-pretty-name attr))
                                     (if allow-null? " blank or" ""))))]
            [(string-type? type)
             (let ([max-length (character-type-max-length type)])
               (check-string attr allow-null? max-length val))]
            [(symbol-type? type)
             (let ([max-length (character-type-max-length type)])
               (check-string attr allow-null? max-length (symbol->string val)))]
            [(time-tai-type? type)
             (if (time-tai? val)
                 (check-pass)
                 (check-fail (format "~a: must be~a a date or time."
                                     (string-titlecase (attribute-pretty-name attr))
                                     (if allow-null? " blank or" ""))))]
            [(time-utc-type? type)
             (if (time-utc? val)
                 (check-pass)
                 (check-fail (format "~a: must be~a a date or time."
                                     (string-titlecase (attribute-pretty-name attr))
                                     (if allow-null? " blank or" ""))))]
            [(guid-type? type)
             (let ([entity (guid-type-entity type)])
               (if (and (guid? val) (eq? (guid-entity val) entity))
                   (if (snooze-struct-saved? val)
                       (check-pass)
                       (check-fail (format "~a: related record unsaved." (string-titlecase (attribute-pretty-name attr)))))
                   (let ([name (string-titlecase (entity-pretty-name entity))])
                     (check-fail (format "~a: must be~a~a."
                                         (string-titlecase (attribute-pretty-name attr))
                                         (if allow-null? " blank or" "")
                                         (if (memq (string-ref name 0) '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))
                                             (format "an ~a" name)
                                             (format "a ~a" name)))))))]))))

; Hooks ------------------------------------------

; entity -> (guid -> (listof check-result))
(define (make-default-save-check entity)
  default-check-snooze-struct)

; entity -> (guid -> (listof check-result))
(define (make-default-delete-check entity)
  default-check-old-snooze-struct)

; entity -> ((connection guid -> guid) connection guid -> guid)
(define (make-default-save-hook entity)
  (lambda (continue conn guid)
    (let ([results (check-snooze-struct guid)])
      (if (check-errors? results)
          (raise-exn exn:fail:snooze:check
            (format "failed validation: could not save ~s:~n~s"
                    guid
                    (pretty-format results))
            results)
          (continue conn guid)))))

; entity -> ((connection guid -> guid) connection guid -> guid)
(define (make-default-delete-hook entity)
  (lambda (continue conn guid)
    (let ([results (check-old-snooze-struct guid)])
      (if (check-errors? results)
          (raise-exn exn:fail:snooze:check
            (format "failed validation: could not delete ~s:~n~s"
                    guid
                    (pretty-format results))
            results)
          (continue conn guid)))))

; Helpers ----------------------------------------

; attribute boolean (U natural #f) (U string any) -> (listof check-result)
(define (check-string attr allow-null? max-length val)
  (if (and (string? val) (or (not max-length) (<= (string-length val) max-length)))
      (check-pass)
      (check-fail (format "~a: must be~a text~a."
                          (attribute-pretty-name attr)
                          (if allow-null? " blank or" "")
                          (if max-length (format " of ~a characters or less" max-length) " of any length")))))

; Provide statements -----------------------------

(provide/contract
 [default-check-snooze-struct     (-> guid? (listof check-result?))]
 [default-check-old-snooze-struct (-> guid? (listof check-result?))]
 [make-default-save-hook          (-> entity? (-> (-> connection? guid? guid?) connection? guid? guid?))]
 [make-default-delete-hook        (-> entity? (-> (-> connection? guid? guid?) connection? guid? guid?))])
