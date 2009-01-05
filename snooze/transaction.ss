(module transaction mzscheme
  
  (require (lib "contract.ss")
           (lib "cut.ss" "srfi" "26"))
  
  (require (planet "list.ss" ("untyped" "unlib.plt" 2))
           (planet "parameter.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "base.ss")
           (file "era-private.ss"))
  
  ; Here's the idea:
  ;
  ; - Every time you start a transaction, you create a frame in which you record
  ;   deltas to persistent-structs.
  ;
  ; - There is a parameter called current-frame that points to the frame you are
  ;   currently using to store roll-back information.
  ;
  ; - When you start a sub-transaction, you create a "child" frame of the current
  ;   frame, and set that to be the current-frame.
  ;
  ; - Rolling back a given transaction involves rolling back its frame and any
  ;   child frames that haven't already been rolled back.
  ;
  ; - Even if a sub-transaction completes successfully, its parent still keeps a
  ;   link to its frame. If the parent is rolled back, it can roll back the child.
  ;
  ; That's about it.
  
  ; Structure types ------------------------------
  
  ;; struct delta : (alist-of symbol any)
  ;;
  ;; Records changes to a structure in case they are rolled back.
  ;; As the struct is mutated, its old field values are recorded 
  ;; in the fields attribute of a delta.
  ;;
  ;; Changes to the struct can be rolled back by calling the 
  ;; roll-back! procedure and passing in the original struct and the
  ;; delta.
  (define-struct delta (fields) #f)
  
  ;; struct frame : (hash-table-of persistent-struct delta) (list-of frame) boolean
  ;;
  ;; The "deltas" field maps persistent structs to the old values 
  ;; of their fields.
  (define-struct frame (deltas children rolled-back?) #f)
  
  ;; current-frame : (parameter (U frame #f))
  (define current-frame (make-parameter #f (make-guard frame? "frame")))
  
  ;; roll-back-persistent-structs? : (parameter boolean)
  (define roll-back-persistent-structs? 
    (make-parameter 
     #t
     (lambda (value)
       (cond [(not (boolean? value))
              (raise-exn exn:fail:contract
                (format "Expected boolean, received: ~a" value))]
             [(current-frame)
              (raise-exn exn:fail:snooze:transaction
                "Cannot set value of roll-back-persistent-structs?: transaction is in progress.")]
             [else value]))))
  
  ; Constructors ---------------------------------
  
  ;; create-frame : -> frame
  (define (create-frame)
    (make-frame (make-hash-table) null #f))
  
  ; Accessors / mutators -------------------------
  
  ;; set-delta-field! : delta symbol any -> void
  (define (set-delta-field! delta field old-value)
    (unless (assoc field (delta-fields delta))
      (set-delta-fields! 
       delta
       (cons (cons field old-value) 
             (delta-fields delta)))))
  
  ;; frame-delta : frame struct -> delta
  ;;
  ;; Retrieves the delta stored for the specified struct. If no
  ;; matching delta is present, a new delta is created and stored.
  (define (frame-delta frame struct)
    (let ([deltas (frame-deltas frame)])
      (hash-table-get
       deltas 
       struct
       ; Failure thunk
       (lambda ()
         (define delta (make-delta null))
         (hash-table-put! deltas struct delta)
         delta))))
  
  ; Rolling back ---------------------------------
  
  ;; roll-back-delta! : persistent-struct delta -> void
  ;;
  ;; Rolls back all changes stored in the delta, including changes to
  ;; data attributes and changes to the revision of the struct.
  (define (roll-back-delta! struct delta)
    (let* ([fields        (delta-fields delta)]
           [revision      (assoc-value/default 'revision fields #f)])
      (when revision
        (set-revision!/internal struct revision))
      (set-attributes/alist! struct fields)))
    
  ; Public interface -----------------------------

  ;; in-transaction : -> boolean
  (define (in-transaction?)
    (if (current-frame) #t #f))
  
  ;; call-with-frame : frame (-> any) -> any
  ;;
  ;; Takes a new transaction frame as an argument, installs it as a child
  ;; of the current frame (if any), resets the current-frame parameter appropriately
  ;; and calls proc.
  (define (call-with-frame frame proc)
    (let ([parent (current-frame)])
      (when parent
        (set-frame-children! parent (cons frame (frame-children parent))))
      (parameterize ([current-frame frame])
        (proc))))
  
  ;; record-delta! : persistent-struct symbol any -> void
  (define (record-delta! struct field old-value)
    (let ([frame (current-frame)])
      (when frame
        (set-delta-field! (frame-delta frame struct) field old-value))))
  
  ;; roll-back-frame! : frame -> void
  ;;
  ;; Rolls back all deltas stored in the specified frame.
  (define (roll-back-frame! frame)
    (let ([children (frame-children frame)]
          [deltas   (frame-deltas frame)])
      ; Roll back the current frame:
      (hash-table-for-each
       (frame-deltas frame)
       (lambda (struct delta)
         (roll-back-delta! struct delta)))
      ; Mark it as rolled back so we don't try it again:
      (set-frame-rolled-back?! frame #t)
      ; Roll back child, if it exists and hasn't already been rolled back:
      (for-each (lambda (child)
                  (when (not (frame-rolled-back? child))
                    (roll-back-frame! child)))
                children)))
    
  ; Provide statements ---------------------------
  
  (provide roll-back-persistent-structs?)
  
  (provide/contract
   [rename create-frame make-frame (-> frame?)]
   [call-with-frame                (-> frame? procedure? any/c)]
   [in-transaction?                (-> boolean?)]
   [record-delta!                  (-> persistent-struct? symbol? any/c void?)]
   [roll-back-frame!               (-> frame? void?)])
  
  )
