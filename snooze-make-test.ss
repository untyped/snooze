#lang scheme/base

(require "test-base.ss")

(require scheme/dict
         (unlib-in hash)
         "snooze-api.ss"
         "era/era.ss"
         (prefix-in real: "era/snooze-struct.ss"))

; Tests -------------------------------------------

; test-suite
(define snooze-make-tests
  (test-suite "snooze-make-tests"
    
    (test-case "make-person : returns a local guid"
      (recreate-test-tables/cache)
      (let* ([per     (make-person "Per")]
             [struct  (send (current-cache) cache-ref/local per)]
             [vanilla (send (current-cache) get-vanilla-guid per)])
        (check-pred guid-local? per)                          ; local guid
        (check-false vanilla)                                 ; no vanilla-guid
        (check-pred (entity-private-predicate person) struct) ; points to an unsaved person
        (check-false (real:snooze-struct-guid struct))        ; ... with no guid,
        (check-false (real:snooze-struct-id struct))          ; ... no id,
        (check-false (real:snooze-struct-revision struct))))  ; ... and no revision
    
    (test-case "make-person twice, same contents: check independence"
      (recreate-test-tables/cache)
      (let* ([per      (make-person "Per")]
             [struct   (send (current-cache) cache-ref/local per)]
             [vanilla  (send (current-cache) get-vanilla-guid per)]
             [per2     (make-person "Per")]
             [struct2  (send (current-cache) cache-ref/local per2)]
             [vanilla2 (send (current-cache) get-vanilla-guid per2)])
        ; per
        (check-pred guid-local? per)                           ; local guid
        (check-false vanilla)                                  ; no vanilla-guid
        (check-pred (entity-private-predicate person) struct)  ; points to an unsaved person
        (check-false (real:snooze-struct-guid struct))         ; ... with no guid,
        (check-false (real:snooze-struct-id struct))           ; ... no id,
        (check-false (real:snooze-struct-revision struct))     ; ... and no revision
        ; per2
        (check-pred guid-local? per2)                          ; also a local guid
        (check-false vanilla2)                                 ; no vanilla-guid
        (check-pred (entity-private-predicate person) struct2) ; points to an unsaved person
        (check-false (real:snooze-struct-guid struct2))        ; ... with no guid,
        (check-false (real:snooze-struct-id struct2))          ; ... no id,
        (check-false (real:snooze-struct-revision struct2))    ; ... and no revision
        ; DISTINCT! but with same contents
        (check-false (snooze-struct-eq? per per2))
        (check-false (eq? struct struct2))
        (check-true  (equal? per per2))
        (check-true  (equal? struct struct2))))
    
    (test-case "make-person and create an identical copy: ensure completely independent copies"
      (recreate-test-tables/cache)
      (let* ([per      (make-person "Per")]
             [per2     (person-set per #:name "Per")]
             [struct   (send (current-cache) cache-ref/local per)]
             [vanilla  (send (current-cache) get-vanilla-guid per)]
             [struct2  (send (current-cache) cache-ref/local per2)]
             [vanilla2 (send (current-cache) get-vanilla-guid per2)])
        ; per
        (check-pred guid-local? per)                           ; local guid
        (check-false vanilla)                                  ; no vanilla-guid
        (check-pred (entity-private-predicate person) struct)  ; points to an unsaved person
        (check-false (real:snooze-struct-guid struct))         ; ... with no guid,
        (check-false (real:snooze-struct-id struct))           ; ... no id,
        (check-false (real:snooze-struct-revision struct))     ; ... and no revision
        ; per2
        (check-pred guid-local? per2)                          ; also a local guid
        (check-false vanilla2)                                 ; no vanilla-guid
        (check-pred (entity-private-predicate person) struct2) ; points to an unsaved person
        (check-false (real:snooze-struct-guid struct2))        ; ... with no guid,
        (check-false (real:snooze-struct-id struct2))          ; ... no id,
        (check-false (real:snooze-struct-revision struct2))    ; ... and no revision
        ; DISTINCT! but with same contents
        (check-false (snooze-struct-eq? per per2))
        (check-false (eq? struct struct2))
        (check-true  (equal? per per2))
        (check-true  (equal? struct struct2))))
    
    (test-case "make-person twice : cache sizes are correct"
      (recreate-test-tables/cache)
      (with-cache
       (let ([per (make-person "Per")])
         (collect-garbage)
         (check-cache-size (list 1 0))
         (let ([per2 (make-person "Per2")])
           (collect-garbage)
           (check-cache-size (list 2 0))
           ;(list per2)
           )
         (collect-garbage)
         (check-cache-size (list 1 0))
         ;(list per)
         )))
    
    (test-case "make-person : minimal test case for Matthew Flatt"
      (recreate-test-tables/cache)
      (let ([per (make-person "Per")])
         (collect-garbage)
         (check-cache-size (list 1))
         ;per
         ))))

; Provide statements -----------------------------

(provide snooze-make-tests)