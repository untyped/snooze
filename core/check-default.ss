#lang scheme/base

(require "../base.ss")

(require (only-in scheme/list last)
         scheme/string
         (only-in srfi/1 drop-right)
         (unlib-in enumeration list)
         "../common/connection.ss"
         "../sql/sql.ss"
         "check.ss"
         "check-annotation.ss"
         "check-result.ss"
         "check-syntax.ss"
         "snooze-struct.ss"
         (except-in "struct.ss"
                    make-check-success
                    make-check-problem
                    make-check-warning
                    make-check-error
                    make-check-failure
                    make-check-fatal))

; Checks -----------------------------------------

; snooze-struct -> (listof check-result)
(define (default-check-snooze-struct struct)
  (let ([entity (snooze-struct-entity struct)])
    (check/annotate ([ann:struct struct])
      ; attribute constraints
      (apply check-problems
             (for/list ([attr (in-list (cddr (entity-attributes entity)))]
                        [val  (in-list (cddr (snooze-struct-ref* struct)))])
               (check-attribute-value attr val)))
      ; uniqueness constraints
      (apply check-problems
             (for/list ([unique (in-list (entity-uniqueness-constraints entity))])
               (check-uniqueness-constraint struct unique))))))

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
            [(guid-type? type)
             (let ([entity (guid-type-entity type)])
               (or (cond [(database-guid? val)
                          (and (eq? (guid-entity val) entity)
                               (check-pass))]
                         [(snooze-struct? val)
                          (and (eq? (snooze-struct-entity val) entity)
                               (if (snooze-struct-saved? val)
                                   (check-pass)
                                   (check-fail (format "~a: related record unsaved." (string-titlecase (attribute-pretty-name attr))))))]
                         [else (raise-type-error 'check-attribute-value "(U database-guid snooze-struct #f)" val)])
                   (check-fail (format "~a: must be~a~a."
                                       (string-titlecase (attribute-pretty-name attr))
                                       (if allow-null? " blank or" "")
                                       (if (memq (string-ref name 0) '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))
                                           (format " an ~a" name)
                                           (format " a ~a" name))))))]
            [(boolean-type? type)
             (if (boolean? val)
                 (check-pass)
                 (check-fail (format "~a: must be a yes/no value."
                                     (string-titlecase (attribute-pretty-name attr)))))]
            [(integer-type? type)
             (let ([min-value (numeric-type-min-value type)]
                   [max-value (numeric-type-max-value type)])
               (check-integer attr allow-null? min-value max-value val))]
            [(real-type? type)
             (let ([min-value (numeric-type-min-value type)]
                   [max-value (numeric-type-max-value type)])
               (check-real attr allow-null? min-value max-value val))]
            [(enum-type? type)
             (if (memq val (enum-type-values type))
                 (check-pass)
                 (check-fail (format "~a: must be~a one of the values: ~a."
                                     (string-titlecase (attribute-pretty-name attr))
                                     (if allow-null? " blank or" "")
                                     
                                     (string-join (map (cut format "~s" <>)
                                                       (if (enum-type-enum type)
                                                           (enum-pretty-values (enum-type-enum type))
                                                           (enum-type-values type)))
                                                  ", "))))]
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
            [(binary-type? type)
             (if (serializable? val)
                 (check-pass)
                 (check-fail (format "~a: must be serializable."
                                     (string-titlecase (attribute-pretty-name attr)))))]))))

; snooze-struct (cons attribute (listof attribute)) -> (listof check-result)
(define (check-uniqueness-constraint struct unique)
  (check/annotate ([ann:attrs unique])
    (check-problems
     (if (zero? (find-count-duplicates struct unique))
         (check-pass)
         (check-fail (format "Another ~a exists with the same ~a."
                             (entity-pretty-name (snooze-struct-entity struct))
                             (let ([names (map attribute-pretty-name unique)])
                               (cond [(list-ref? names 2)
                                      (format "~a, and ~a"
                                              (string-join (drop-right names 1) ", ")
                                              (last names))]
                                     [(list-ref? names 1)
                                      (format "~a and ~a"
                                              (car names)
                                              (cadr names))]
                                     [else (car names)]))))))))

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
            (format "failed validation: could not save ~s:~n~a"
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
            (format "failed validation: could not delete ~s:~n~a"
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
                          (if max-length
                              (format " of ~a characters or less" max-length)
                              " of any length")))))

; attribute boolean (U natural #f) (U string any) -> (listof check-result)
(define (check-integer attr allow-null? min-value max-value val)
  (if (and (integer? val) (or (not max-value) (<= val max-value)) (or (not min-value) (>= val min-value)))
      (check-pass)
      (check-fail (format "~a: must be~a a whole number~a."
                          (attribute-pretty-name attr)
                          (if allow-null? " blank or" "")
                          (if (or min-value max-value) 
                              (format " n, where ~an~a"
                                      (if min-value (format "~a <= " min-value) "")
                                      (if max-value (format " <= ~a" max-value) ""))
                              "")))))

(define (check-real attr allow-null? min-value max-value val)
  (if (and (real? val) (or (not max-value) (not (> val max-value))) (or (not min-value) (not (< val min-value))))
      (check-pass)
      (check-fail (format "~a: must be~a a number~a."
                          (attribute-pretty-name attr)
                          (if allow-null? " blank or" "")
                          (if (or min-value max-value) 
                              (format " n, where ~an~a"
                                      (if min-value (format "~a <= " min-value) "")
                                      (if max-value (format " <= ~a" max-value) ""))
                              "")))))

; snooze-struct (cons attribute (listof attribute)) -> natural
(define (find-count-duplicates struct attrs)
  (let-alias ([entity (snooze-struct-entity struct)])
    (let ([vals (map (cut snooze-struct-ref struct <>) attrs)])
      (if (ormap (lambda (attr val)
                   (equal? val (type-null (attribute-type attr))))
                 attrs
                 vals)
          ; The SQL spec states that NULL <> NULL - if any attributes are NULL
          ; the uniqueness constraint will never be violated:
          0
          (send (current-snooze) find-one
                (sql (select #:what  (count entity.guid)
                             #:from  entity
                             #:where ,(apply sql:and
                                             (if (snooze-struct-saved? struct)
                                                 (sql (<> entity.guid ,struct))
                                                 (sql #t))
                                             (map (lambda (attr val)
                                                    (sql:= (sql:alias entity attr) val))
                                                  attrs
                                                  vals)))))))))

; Provides ---------------------------------------

(provide/contract
 [default-check-snooze-struct     (-> snooze-struct? (listof check-result?))]
 [default-check-old-snooze-struct (-> snooze-struct? (listof check-result?))]
 [make-default-save-hook          (-> entity? (-> (-> connection? snooze-struct? snooze-struct?)
                                                  connection?
                                                  snooze-struct?
                                                  snooze-struct?))]
 [make-default-delete-hook        (-> entity? (-> (-> connection? snooze-struct? snooze-struct?)
                                                  connection?
                                                  snooze-struct?
                                                  snooze-struct?))])
