#lang scheme/base

(require "../base.ss"
         "../era/era.ss"
         "../era/annotation.ss"
         "check-combinator.ss"
         "check-combinator-syntax.ss"
         "result.ss"
         "util.ss")

; Procedures -------------------------------------

; snooze-struct entity [boolean] -> (listof check-result)
(define (check-struct/entity struct entity [pretty-messages? #t])
  (apply check-all
         (for/list ([attr (entity-attributes entity)])
           (check/annotate ([ann:attrs (list attr)])
             (check-struct/attribute struct attr pretty-messages?)))))

; snooze-struct attribute [boolean] -> (listof check-result)
(define (check-struct/attribute struct attr [pretty-messages? #t])
  ; type
  (define type
    (attribute-type attr))
  ; any
  (define value
    (struct-attribute struct attr))
  ; check-result
  (if (type-valid? type value)
      (check-pass)
      (if pretty-messages?
          (check-fail (format "~a must be ~a~a."
                              (attribute-annotation attr ann:pretty-name)
                              (if (type-allows-null? type) "blank or" "")
                              (cond [(boolean-type? type)  "a yes/no value."]
                                    [(integer-type? type)  "an integer value."]
                                    [(real-type? type)     "a numeric value."]
                                    [(string-type? type)   (format "text of length ~a or less." (character-type-max-length type))]
                                    [(symbol-type? type)   (format "text of length ~a or less." (character-type-max-length type))]
                                    [(time-tai-type? type) "a date and time."]
                                    [(time-utc-type? type) "a date and time."])))
          (check-fail (cond [(boolean-type? type)     "boolean"]
                            [(type-allows-null? type) (format "(U ~a #f)" (type-name type))]
                            [else                     (symbol->string (type-name type))])))))

; Provide statements -----------------------------

(provide/contract
 [check-struct/entity    (->* (snooze-struct? entity?)    (boolean?) (listof check-result?))]
 [check-struct/attribute (->* (snooze-struct? attribute?) (boolean?) (listof check-result?))])
