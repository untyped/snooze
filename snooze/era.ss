(module era mzscheme
  
  (require (lib "contract.ss"))
  
  (require (file "base.ss")
           (file "era-private.ss")
           (file "transaction.ss"))
  
  ; All of the functionality from era.ss in Snooze 2.0 is now in
  ; era-private.ss. This is to get around a cyclic dependency that
  ; would occur between roll-back-deltas! in transaction.ss and 
  ; set-revision! below.
  
  ; Procedures -----------------------------------
  
  ;; set-revision! : persistent-struct integer -> void
  ;;
  ;; Sets the revision number on a structure and records the change
  ;; in case of rollback.
  (define (set-revision! struct revision)
    (if (and (in-transaction?)
             (roll-back-persistent-structs?))
        (let ([old-revision (get-revision struct)])
          (begin0 (set-revision!/internal struct revision)
                  (record-delta! struct 'revision old-revision)))
        (set-revision!/internal struct revision)))
  
  ; Provide statements ---------------------------
  
  (provide (all-from-except (file "era-private.ss") set-revision!/internal)
           roll-back-persistent-structs?)
  
  (provide/contract
   [set-revision! (-> persistent-struct? (or/c integer? false/c) void?)])

  )