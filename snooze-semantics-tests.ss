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
  
  (test-suite "snooze-struct-saved?"
    (test-case "returns #f for unsaved snooze-structs"
      (check-false (snooze-struct-saved? (make-person/defaults #:name "Jon"))))
    
    (test-case "returns #t for saved snooze-structs"
      (let ([jon (save! (make-person/defaults #:name "Jon"))])
        (check-pred snooze-struct-saved? jon)
        (delete! jon)))
    
    (test-case "returns #f for deleted snooze-structs"
      (let ([jon (save! (make-person/defaults #:name "Jon"))])
        (check-pred snooze-struct-saved? jon)
        (delete! jon)
        (check-false (snooze-struct-saved? jon))))
    
    ; CONTROVERSIAL? we should have some kind of "snooze-struct-on-database?" predicate, too
    #;(test-case "CONTROVERSIAL! returns #f for copies of deleted snooze-structs"
        (let* ([jon (save! (make-person/defaults #:name "Jon"))]
               [bob (person-set jon #:name "Bob")])
          (check-pred snooze-struct-saved? jon)
          (delete! jon)
          (check-false (snooze-struct-saved? jon))
          (check-false (snooze-struct-saved? bob)))))
  
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
  
  
  
  (test-suite "post-save! and post-delete! queries"
    (test-case "save! several structs, run a query, same results expected"
      (recreate-tables)
      (let ([jon1 (save! (make-person/defaults #:name "Jon"))]
            [jon2 (save! (make-person/defaults #:name "Jon"))]
            [jon3 (save! (make-person/defaults #:name "Jon"))]
            [jon4 (save! (make-person/defaults #:name "Jon"))]
            [jon5 (save! (make-person/defaults #:name "Jon"))])
        (let ([jons (select-all #:from person #:where (= person.name "Jon"))])
          (check-true (= 5 (length jons)))
          (for ([pers (in-list jons)])
            (check-equal? (person-name pers) "Jon")))))
    
    (test-case "save! several structs, run a query, update saved struct, re-run query: different results expected"
      (recreate-tables)
      (let ([jon1 (save! (make-person/defaults #:name "Jon"))]
            [jon2 (save! (make-person/defaults #:name "Jon"))]
            [jon3 (save! (make-person/defaults #:name "Jon"))]
            [jon4 (save! (make-person/defaults #:name "Jon"))]
            [jon5 (save! (make-person/defaults #:name "Jon"))])
        (let ([jons (select-all #:from person #:where (= person.name "Jon"))])
          (check-true (= 5 (length jons)))
          (for ([pers (in-list jons)])
            (check-equal? (person-name pers) "Jon")))
        ; rename one to "Bob"
        (let ([bob (save! (person-set jon5 #:name "Bob"))])
          (let ([jons (select-all #:from person #:where (= person.name "Jon"))])
            (check-true (= 4 (length jons)))
            (for ([pers (in-list jons)])
              (check-equal? (person-name pers) "Jon")))
          (let ([bobs (select-all #:from person #:where (= person.name "Bob"))])
            (check-true (= 1 (length bobs)))
            (for ([pers (in-list bobs)])
              (check-equal? (person-name pers) "Bob"))))))
    
    (test-case "save! several structs, run a query, update saved struct, re-run query checks: different results expected"
      (recreate-tables)
      (let ([jon1 (save! (make-person/defaults #:name "Jon"))]
            [jon2 (save! (make-person/defaults #:name "Jon"))]
            [jon3 (save! (make-person/defaults #:name "Jon"))]
            [jon4 (save! (make-person/defaults #:name "Jon"))]
            [jon5 (save! (make-person/defaults #:name "Jon"))])
        (let ([jons (select-all #:from person #:where (= person.name "Jon"))])
          (check-true (= 5 (length jons)))
          (for ([pers (in-list jons)])
            (check-equal? (person-name pers) "Jon"))
          ; modify and re-run same tests
          (let ([bob (save! (person-set jon5 #:name "Bob"))])
            ; re-run previous tests, to ensure jons are still jons (local GUIDs)
            (check-true (= 5 (length jons)))
            (for ([pers (in-list jons)])
              (check-equal? (person-name pers) "Jon"))
            ; similar tests for bobs
            (let ([bobs (select-all #:from person #:where (= person.name "Bob"))])
              (check-true (= 1 (length bobs)))
              (for ([pers (in-list bobs)])
                (check-equal? (person-name pers) "Bob"))))))))
  
  
  
  
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
    ; This is the "uniquing" or "interning" case, where dereferencing a foreign-key always gets the database state.
    ; The alternative is to have the foreign-key reference "fixed" the first time it is dereferenced,
    ; with subsequent dereferences always returning the same struct.
    (test-case "CONTROVERSIAL!: foreign-key accessors retain references after update"
      (recreate-tables)
      ; Uniquing
      (let* ([jon      (save! (make-person/defaults #:name "Jon"))]
             [garfield (save! (make-pet/defaults #:name "Garfield" #:owner jon))]) ; original owner
        (check-equal? jon (pet-owner garfield))
        ; edit independent struct, but don't save
        (let ([bob (person-set (person-set jon) #:name "Bob")])                    ; edit owner, don't save - UNCHANGED!
          (check-equal? jon (pet-owner garfield))
          (let ([saved-bob (save! bob)])                                           ; save updated owner - CHANGED!
            (check-equal? saved-bob (pet-owner garfield)))))
      ; The alternative: fixed dereferencing
      #;(let* ([jon      (save! (make-person/defaults #:name "Jon"))] 
               [garfield (save! (make-pet/defaults #:name "Garfield" #:owner jon))]) ; original owner
          (check-equal? jon (pet-owner garfield))
          (let ([bob (person-set (person-set jon) #:name "Bob")])                    ; edit owner, don't save - UNCHANGED!
            (check-equal? jon (pet-owner garfield))
            (let ([saved-bob (save! bob)])                                           ; save updated owner - UNCHANGED!
              (check-equal? jon (pet-owner garfield))))))
    
    ; This one seems a little bizarre if uniquing is allowed. It implies that the behaviour
    ; of foreign-key dereferencing changes, when another copy of the same struct is saved.
    ; This makes me feel a bit queasy, since the behaviour of a single struct can't be predicted.
    ; Seems to fit better with the fixed-dereferencing model above.
    (test-case "CONTROVERSIAL! foreign-key behaviour under update of dependent struct"
      (let* ([jon      (save! (make-person/defaults #:name "Jon"))]
             [bob      (save! (make-person/defaults #:name "Bob"))]
             [garfield (save! (make-pet/defaults #:name "Garfield" #:owner jon))])
        (check-equal? jon (pet-owner garfield))
        (let ([garf2 (save! (pet-set garfield #:owner bob))])
          (check-equal? jon (pet-owner garfield) "original struct should be unchanged")
          (check-equal? bob (pet-owner garf2)    "updated struct should reflect change in database")))
      ; alternative
      #;(let* ([jon      (save! (make-person/defaults #:name "Jon"))]
               [bob      (save! (make-person/defaults #:name "Bob"))]
               [garfield (save! (make-pet/defaults #:name "Garfield" #:owner jon))])
          (check-equal? jon (pet-owner garfield))
          (let ([garf2 (save! (pet-set garfield #:owner bob))])
            (check-equal? bob (pet-owner garfield) "original struct should reflect change in database")
            (check-equal? bob (pet-owner garf2)    "updated struct should reflect change in database")))))
  
  
  
  
  (test-suite "functional update on cache"
    ; [1] establish cache
    ; [2] save some data into cache
    ; [3] update and save data in cache -> cache'
    ; [4] step back to original cache, and ensure data still correct and unchanged.
    )
  
  
  ; Thoughts on caching and transactions
  ; A) Caching here refers to a functional hashtable mapping saved struct GUIDs to snooze-structs. Unsaved
  ;    structs should not be stored in the cache; only saved structs should be present in the cache.
  ;    Each time save! or delete! is called, a new functional hashtable is created.
  ;    NOTE: save! and delete! should update the cache only, not the database, for thread safety.
  ;          Database commission should only occur at the end of a transaction.
  ; B) Foreign key references are stored as (U snooze-struct GUID), where:
  ;     - snooze structs are used for unsaved structs (be they modified version of saved structs, or entirely new).
  ;     - GUIDs are used when the struct is present in the cache.
  ;    Since structs are immutable, "setting" foreign-key references involves copying the original struct,
  ;    which is continuation safe.
  ; C) The dereferencing a foreign-key will always yield the latest version of a struct, as defined in the cache.
  ;    If the struct is not already in the cache, it will be fetched and cached.
  ; D) To modify a saved struct, we first fetch it into the cache, then take a local copy, stored in a variable.
  ;    Local copies do not enter the cache. To create a foreign-key reference from one struct to a local copy,
  ;    we simply overwrite the foreign-key field with the local copy.
  ; E) Saving a local copy (or unsaved struct) writes through to the cache.
  ;    PROBLEM:
  
  (let* ([santa   (make-person/defaults #:name "Santa")]
         [dasher  (make-pet/defaults #:name "Dasher"  #:owner santa)]
         [prancer (make-pet/defaults #:name "Prancer" #:owner santa)]
         [vixen   (make-pet/defaults #:name "Vixen"   #:owner santa)])
    (save! santa) ; functional update on cache...
    ; ... but what about those three references? Snooze needs to replace the references with the GUID,
    ; but can't do this without mutating the three referring structs. This leaves inconsistency in that either:
    ;   (i) the three struct FKs are not mutated, meaning that traversing them can yield inconsistent data;
    ;  (ii) the three struct FKs are mutated, meaning lots of work for Snooze, and no continuation safety
    ;
    ; ================ THIS IS WHY WE INTRODUCED THE LOCAL->VANILLA INDIRECTION! =====================
    ;
    ; The aim was to retain struct immutability, transferring the mutation to the _cache_ 
    ; This should preserve continuation safety on local variables, and facilitate easy update.
    )
    
  ; B) Caches must be established outside transactions. Functional treatment of the cache is of little use if
  ;    it doesn't exist in between transactions. Perhaps an argument for "with-cache"? We did have this already, however...
  
  (test-suite "eq? and equal? behaviour"))
