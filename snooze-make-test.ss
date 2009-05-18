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
        (check-false (real:struct-guid struct))               ; ... with no guid,
        (check-false (real:struct-id struct))                 ; ... no id,
        (check-false (real:struct-revision struct))))         ; ... and no revision
    
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
        (check-false (real:struct-guid struct))                ; ... with no guid,
        (check-false (real:struct-id struct))                  ; ... no id,
        (check-false (real:struct-revision struct))            ; ... and no revision
        ; per2
        (check-pred guid-local? per2)                          ; also a local guid
        (check-false vanilla2)                                 ; no vanilla-guid
        (check-pred (entity-private-predicate person) struct2) ; points to an unsaved person
        (check-false (real:struct-guid struct2))               ; ... with no guid,
        (check-false (real:struct-id struct2))                 ; ... no id,
        (check-false (real:struct-revision struct2))           ; ... and no revision
        ; DISTINCT! but with same contents
        (check-false (struct-eq? per per2))
        (check-false (eq? struct struct2))
        (check-true  (equal? per per2))
        (check-true  (equal? struct struct2))))
    
    (test-case "make-person, and save: creates a vanilla GUID and a local GUID, distinct from first."
      (recreate-test-tables/cache)
      (let* ([per      (make-person "Per")]
             [per2     (save! per)]
             [struct   (send (current-cache) cache-ref/local per)]
             [vanilla  (send (current-cache) get-vanilla-guid per)]
             [struct2  (send (current-cache) cache-ref/local per2)]
             [vanilla2 (send (current-cache) get-vanilla-guid per2)])
        ; per, having been remapped following save!
        (check-pred guid-local? per)                           ; still a local guid
        (check-not-false vanilla)                              ; but it now has a vanilla-guid
        (check-pred (entity-private-predicate person) struct)  ; still refers to a person, but now it's saved:
        (check-not-false (real:struct-guid struct))            ; ... it has a valid guid,
        (check-not-false (real:struct-id struct))              ; ... id,
        (check-not-false (real:struct-revision struct))        ; ... and revision.
        ; per2
        (check-pred guid-local? per2)                          ; per2 is a local guid...
        (check-pred (entity-private-predicate person) struct2) ; it also points to a saved person...
        (check-true (struct-eq? per per2))                     ; ... exactly the same one as per.
        (check-true (eq? struct struct2))                      ; (just to be sure)
        (check-false (eq? per per2))                           ; However, guids themselves are not eq?
        (check-not-false vanilla2)                             ; per2 also points to a vanilla guid ...
        (check-true (eq? vanilla vanilla2))))                  ; ... the same one as per    
    
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
        (check-false (real:struct-guid struct))                ; ... with no guid,
        (check-false (real:struct-id struct))                  ; ... no id,
        (check-false (real:struct-revision struct))            ; ... and no revision
        ; per2
        (check-pred guid-local? per2)                          ; also a local guid
        (check-false vanilla2)                                 ; no vanilla-guid
        (check-pred (entity-private-predicate person) struct2) ; points to an unsaved person
        (check-false (real:struct-guid struct2))               ; ... with no guid,
        (check-false (real:struct-id struct2))                 ; ... no id,
        (check-false (real:struct-revision struct2))           ; ... and no revision
        ; DISTINCT! but with same contents
        (check-false (struct-eq? per per2))
        (check-false (eq? struct struct2))
        (check-true  (equal? per per2))
        (check-true  (equal? struct struct2))))
    
    ))     

; Provide statements -----------------------------

(provide snooze-make-tests)