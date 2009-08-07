#lang scheme/base

(require schemeunit
         schemeunit/text-ui
         (only-in srfi/26 cut)
         (planet untyped/snooze:3)
         (planet untyped/snooze:3/postgresql8/postgresql8))

; This file contains a quick, self-contained setup for Snooze3
; The aim is to define the behaviour presented to the programmer,
; such that a quick-start guide can be generated easily, with
; the semantics of Snooze tightly defined in all circumstances.
; Contentious semantics are denoted with CONTROVERSIAL!

; Database creation and configuration ------------
(define my-db (make-postgresql8-database #:database "mydb" #:username "myuser"))
(current-snooze (make-snooze my-db))

; Configuration at the shell level (linux/mac)
; ~$ createdb mydb
; ~$ createuser myuser
; Shall the new role be a superuser? (y/n) n
; Shall the new role be allowed to create databases? (y/n) n
; Shall the new role be allowed to create more new roles? (y/n) n


; Entity definitions -----------------------------

(define-entity person
  ([name string #:allow-null? #f #:max-length 10])
  #:pretty-formatter
  (lambda (pers) (person-name pers)))

(define-entity pet
  ([name  string #:allow-null? #f #:max-length 10]
   [owner person #:allow-null? #f])
  #:pretty-formatter
  (lambda (pet) (format "~a [~a]" (pet-name pet) (format-person (pet-owner pet)))))

; Database recreation ----------------------------

; -> void
(define (recreate-tables)
  (map drop-table (list pet person))
  (map create-table (list person pet)))

; Tests ------------------------------------------
(define/provide-test-suite snooze-semantics-tests
  (recreate-tables)
  
  (test-suite "creating and deleting snooze-structs"
    (test-case "creating an unsaved snooze-struct"
      (let ([unsaved-struct (make-person/defaults #:name "Jon")])
        (check-false (snooze-struct-saved? unsaved-struct))
        (check-equal? (person-name unsaved-struct) "Jon")))
    
    (test-case "creating and saving a snooze-struct"
      (let ([saved-struct (save! (make-person/defaults #:name "Jon"))])
        (check-pred snooze-struct-saved? saved-struct)
        (check-equal? (person-name saved-struct) "Jon")))
    
    (test-case "creating an unsaved snooze-struct, then saving it"
      (let ([unsaved-struct (make-person/defaults #:name "Jon")]) ; create an unsaved struct
        (check-false (snooze-struct-saved? unsaved-struct))
        (check-equal? (person-name unsaved-struct) "Jon")
        (let ([saved-struct (save! unsaved-struct)])              ; save struct
          (check-pred snooze-struct-saved? saved-struct)
          (check-equal? (person-name saved-struct) "Jon")
          (check-pred snooze-struct-saved? unsaved-struct "original struct should also be saved (hence the \"!\")"))))
    
    (test-case "creating a saved snooze-struct, then deleting it"
      (let ([saved-struct (save! (make-person/defaults #:name "Jon"))])
        (check-pred snooze-struct-saved? saved-struct)
        (check-equal? (person-name saved-struct) "Jon")
        ; deleting the struct again 
        (let ([deleted-struct (delete! saved-struct)])
          (check-false (snooze-struct-saved? deleted-struct))
          (check-equal? (person-name deleted-struct) "Jon"))))
    
    (test-case "creating a snooze-struct with a foreign key reference to another snooze-struct"
      (let ([jon (make-person/defaults #:name "Jon")])
        (make-pet/defaults #:name "Garfield" #:owner jon)))
    
    (test-case "saving snooze-structs with FK references"
      (let* ([jon      (make-person/defaults #:name "Jon")]
             [garfield (make-pet/defaults #:name "Garfield" #:owner jon)])
        (check-exn exn:fail:snooze:check? (cut save! garfield))
        (check-not-exn (cut save! jon))
        (check-not-exn (cut save! garfield))))
    
    (test-case "deleting snooze-structs with FK references"
      (let* ([jon      (save! (make-person/defaults #:name "Jon"))]
             [garfield (save! (make-pet/defaults #:name "Garfield" #:owner jon))])
        (check-exn exn:fail:snooze? (cut delete! jon))
        (check-not-exn (cut delete! garfield))
        (check-not-exn (cut delete! jon)))))
  
  
  
  
  (test-suite "snooze-struct update"
    (test-case "updating an unsaved snooze-struct yields an independent copy"
      (let* ([jon (make-person/defaults #:name "Jon")]
             [bob (person-set jon #:name "Bob")])
        (check-not-equal? jon bob)
        (check-false (snooze-struct-saved? jon))
        (check-false (snooze-struct-saved? bob))
        (check-equal? (person-name jon) "Jon")
        (check-equal? (person-name bob) "Bob")))
    
    (test-case "updating an unsaved snooze-struct yields an independent copy under save!"
      (let* ([jon (make-person/defaults #:name "Jon")]
             [bob (person-set jon #:name "Bob")])
        (check-false (snooze-struct-saved? jon))
        (check-false (snooze-struct-saved? bob))
        (check-not-exn (cut save! jon))
        (check-not-exn (cut save! bob))
        (check-true  (snooze-struct-saved? jon))
        (check-true  (snooze-struct-saved? bob))
        (check-false (= (snooze-struct-id jon) (snooze-struct-id bob)))))
    
    (test-case "updating a saved snooze-struct yields an updated copy with the same database ID"
      (let* ([jon (save! (make-person/defaults #:name "Jon"))]
             [bob (person-set jon #:name "Bob")])
        (check-not-equal? jon bob)
        (check-true (snooze-struct-saved? jon))
        (check-true (snooze-struct-saved? bob))
        (check-true (= (snooze-struct-id jon) (snooze-struct-id bob)))
        (check-equal? (person-name jon) "Jon")
        (check-equal? (person-name bob) "Bob")))
    
    (test-case "updating a saved snooze-struct yields an updated snooze-struct with the same ID save!"
      (let* ([jon (save! (make-person/defaults #:name "Jon"))]
             [bob (save! (person-set jon #:name "Bob"))])
        (check-true  (snooze-struct-saved? jon))
        (check-true  (snooze-struct-saved? bob))
        (check-exn exn:fail:snooze? (cut save! jon))
        (check-not-exn (cut save! bob))
        (check-true  (= (snooze-struct-id jon)       (snooze-struct-id bob)))
        (check-false (= (snooze-struct-revision jon) (snooze-struct-revision bob)))))
    
    (test-case "two updated copies: first one saved prevents the second from saving"
      (let* ([jon (save! (make-person/defaults #:name "Jon"))]
             [bob (person-set jon #:name "Bob")]
             [sam (person-set jon #:name "Sam")])
        (check-not-exn (cut save! bob))
        (check-exn exn:fail:snooze? (cut save! sam))))
    
    
    
    
    (test-suite "snooze-struct queries"
      
      (test-case "select-one should return the first snooze-struct"
        (recreate-tables)
        (let ([jon   (save! (make-person/defaults #:name "Jon"))]
              [bob   (save! (make-person/defaults #:name "Bob"))]
              [q-jon (select-one #:from person)])
          (check-equal? jon q-jon)))
      
      (test-case "select-one with a #:where constraint should return the first matching snooze-struct"
        (recreate-tables)
        (let ([jon   (save! (make-person/defaults #:name "Jon"))]
              [bob   (save! (make-person/defaults #:name "Bob"))]
              [q-bob (select-one #:from person #:where (= person.name "Bob"))])
          (check-equal? bob q-bob)))
      
      (test-case "select-all should return a list of all snooze-structs"
        (recreate-tables)
        (let ([jon       (save! (make-person/defaults #:name "Jon"))]
              [bob       (save! (make-person/defaults #:name "Bob"))]
              [q-structs (select-all #:from person)])
          (check-true      (= (length q-structs) 2))
          (check-not-false (member jon q-structs))
          (check-not-false (member bob q-structs))))
      
      (test-case "select-all with a #:where constraint should return a list of only matching snooze-structs"
        (recreate-tables)
        (let ([jon1      (save! (make-person/defaults #:name "Jon"))]
              [jon2      (save! (make-person/defaults #:name "Jon"))]
              [bob1      (save! (make-person/defaults #:name "Bob"))]
              [bob2      (save! (make-person/defaults #:name "Bob"))]
              [q-structs (select-all #:from person #:where (= person.name "Bob"))])
          (check-true      (= (length q-structs) 2))
          (check-not-false (member bob1 q-structs))
          (check-not-false (member bob2 q-structs))))
      
      (test-case "select-all with ordering constraints should return a list of matching snooze-structs"
        (recreate-tables)
        (let ([jon       (save! (make-person/defaults #:name "Jon"))]
              [bob       (save! (make-person/defaults #:name "Bob"))]
              [q-structs (select-all #:from person #:order ((asc person.name)))])
          (check-equal? q-structs (list bob jon))))))
  
  
  
  
  (test-suite "foreign-key dereferencing"
    (test-case "foreign-key accessors return the snooze-struct referenced (unsaved structs)"
      (let* ([jon      (make-person/defaults #:name "Jon")]
             [garfield (make-pet/defaults #:name "Garfield" #:owner jon)])
        (check-equal?  jon (pet-owner garfield))
        (check-not-eq? jon (pet-owner garfield))))
    
    (test-case "foreign-key accessors return the snooze-struct referenced (saved independent structs)"
      (let* ([jon      (save! (make-person/defaults #:name "Jon"))]
             [garfield (make-pet/defaults #:name "Garfield" #:owner jon)])
        (check-equal?  jon (pet-owner garfield))
        (check-not-eq? jon (pet-owner garfield))))
    
    (test-case "foreign-key accessors return the snooze-struct referenced (saved independent/dependent structs)"
      (let* ([jon      (save! (make-person/defaults #:name "Jon"))]
             [garfield (save! (make-pet/defaults #:name "Garfield" #:owner jon))])
        (check-equal?  jon (pet-owner garfield))
        (check-not-eq? jon (pet-owner garfield))))
    
    ; CONTROVERSIAL!
    ; This is the "uniquing" case, where dereferencing a foreign-key always gets the database state.
    ; The alternative is to have the foreign-key reference "fixed" the first time it is dereferenced,
    ; with subsequent dereferences always returning the same struct.
    (test-case "CONTROVERSIAL!: foreign-key accessors retain references after update"
      ; Uniquing
      (let* ([jon      (save! (make-person/defaults #:name "Jon"))]
             [garfield (save! (make-pet/defaults #:name "Garfield" #:owner jon))])
        (check-equal? jon (pet-owner garfield))
        (let ([bob (save! (person-set jon #:name "Bob"))])
          (check-equal? bob (pet-owner garfield))))
      ; The alternative: fixed dereferencing
      #;(let* ([jon      (save! (make-person/defaults #:name "Jon"))]
               [garfield (save! (make-pet/defaults #:name "Garfield" #:owner jon))])
          (check-equal? jon (pet-owner garfield))
          (let ([bob (save! (person-set jon #:name "Bob"))])
            (check-equal? jon (pet-owner garfield)))))
    
    (test-case "foreign-key behaviour under update of dependent struct"
      (let* ([jon      (save! (make-person/defaults #:name "Jon"))]
             [bob      (save! (make-person/defaults #:name "Bob"))]
             [garfield (save! (make-pet/defaults #:name "Garfield" #:owner jon))])
        (check-equal? jon (pet-owner garfield))
        (let ([garf2 (save! (pet-set garfield #:owner bob))])
          (check-equal? jon (pet-owner garfield) "original struct should be unchanged")
          (check-equal? bob (pet-owner garf2)    "updated struct should reflect change.")))))
  
  
  
  (test-suite "eq? and equal? behaviour"))

(call-with-connection (cut run-tests snooze-semantics-tests))
