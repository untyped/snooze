(module snooze-unit mzscheme
  
  (require (lib "class.ss")
           (lib "etc.ss")
           (lib "kw.ss")
           (lib "unitsig.ss")
           (lib "cut.ss" "srfi" "26"))
  
  (require (planet "gen.ss" ("untyped" "unlib.plt" 2))
           (planet "parameter.ss" ("untyped" "unlib.plt" 2))
           (planet "pipeline.ss" ("untyped" "unlib.plt" 2)))
  
  (require (file "base.ss")
           (file "db-sig.ss")
           (file "era.ss")
           (prefix q: (file "query-lang.ss"))
           (file "snooze-sig.ss"))
  
  (provide current-config
           snooze@)
  
  ; The backend-independent part of the main Snooze interface.
  ;
  ; ***** NOTE *****
  ; The "configuration" and "connection" types below are not concrete types:
  ; they are backend-specific and depend on the db^ unit used.
  ; ****************
  
  ;; current-config : (parameter config)
  (define current-config
    (make-parameter #f (make-guard identity "config")))
  
  ;; current-connection-cell : (thread-cell (U connection #f))
  ;;
  ;; A thread-cell to store a current connection based on the current-config
  ;; parameter. See the documentation for the current-connection procedure
  ;; below for more information.
  (define current-connection-cell
    (make-thread-cell #f))
  
  ;; snooze@ : db^ -> snooze^
  ;;
  ;; This unit, given a database backend, creates a set of Snooze interface functions.
  ;; Some of these functions delegate functionality directly to the backend, while others
  ;; provide convenient wrappers.
  ;;
  ;; ***** NOTE *****
  ;; Some functions have an optional connection argument. If this argument is not provided,
  ;; the functions default to a parameterized connection that is set up with the
  ;; (call-with-database ...) function or the equivalent syntax (with-database ...) defined
  ;; in snooze.ss.
  ;; ****************
  (define snooze@
    (unit/sig snooze^
      (import (db : db^))
      
      ; Public interface -------------------------
      
      ;; connect : config -> connection
      (define connect db:connect)
      
      ;; disconnect : connection -> void
      (define disconnect db:disconnect)
      
      ;; call-with-database : config thunk -> void
      (define (call-with-database config thunk)
        (dynamic-wind
         (lambda ()
           (let ([connection (connect config)])
             (thread-cell-set! current-connection-cell connection)))
         (lambda ()
           (thunk))
         (lambda ()
           (disconnect (thread-cell-ref current-connection-cell))
           (thread-cell-set! current-connection-cell #f))))
      
      ;; current-connection : -> connection
      ;;
      ;; Returns a connection based on the current-config parameter.
      ;;
      ;; One connection is stored per thread. If the thread is suspended or killed,
      ;; the connection is disconnected and set to #f. Connection management happens
      ;; inside the snooze unit below.
      (define (current-connection)
        ;(cond [(thread-cell-ref current-connection-cell)
        ;       => (lambda (connection)
        ;            connection)]
        ;      [(current-config)
        ;       => (lambda (config)
        ;            ; Set up a connection based on config and store it in current-connection-cell:
        ;            (let ([connection (connect config)]
        ;                  [my-thread  (current-thread)])
        ;              (printf "Creating ~a~n" connection)
        ;              (thread-cell-set! current-connection-cell connection)
        ;              ; Make sure that, when the current thread dies, the connection is disconnected:
        ;              (thread (lambda ()
        ;                        (sync (thread-dead-evt my-thread) (thread-suspend-evt my-thread))
        ;                        (printf "Killing ~a~n" connection)
        ;                        (thread-cell-set! current-connection-cell #f)
        ;                        (disconnect connection)))
        ;              ; Return the connection:
        ;              connection))]
        ;      [else (raise-exn exn:fail:snooze
        ;              (string-append "No current default connection. "
        ;                             "Use call-with-database to set up the default DB configuration, "
        ;                             "or specify the connection as an argument to the query."))])
        (let ([connection (thread-cell-ref current-connection-cell)])
          (if connection
              connection
              (raise-exn exn:fail:snooze
                (string-append "No current default connection. "
                               "Use call-with-database to set up the default DB configuration, "
                               "or specify the connection as an argument to the query.")))))
      
      ;; create-table : [connection] entity -> void
      (define create-table
        (case-lambda
          ((entity)
           (create-table (current-connection) entity))
          ((conn entity)
           (db:create-table conn entity))))
      
      ;; drop-table : [connection] entity -> void
      (define drop-table
        (case-lambda
          ((entity)
           (drop-table (current-connection) entity))
          ((conn entity)
           (db:drop-table conn entity))))
      
      ;; save! : [connection] persistent-struct -> integer
      (define save!
        (case-lambda
          ((struct)
           (save! (current-connection) struct))
          ((conn struct)
           (let* ([id              (get-id struct)]
                  [revision        (get-revision struct)]
                  [entity          (struct-entity struct)]
                  [pipeline        (append (entity-save-pipeline entity)
                                           (if id
                                               (entity-update-pipeline entity)
                                               (entity-insert-pipeline entity)))])
             ; We used to have the privilege of deciding whether or not we wanted
             ; a transaction around this lot. Since the addition of revisions, we
             ; pretty much don't have a choice anymore.
             (call-with-transaction
                 (lambda ()
                   ; Both insert-struct and update-struct return the structure's
                   ; new id (even though the id doesn't change in update-struct).
                   (if id
                       ; Check the revision number with the stored revision:
                       ; increment and save if they match, throw an exception
                       ; if they don't.
                       (begin (if (and revision (record-exists-with-revision? conn entity id revision))
                                  (begin (call-with-pipeline
                                          (append (entity-save-pipeline entity)
                                                  (entity-update-pipeline entity))
                                          (lambda (conn struct)
                                            ; UPdate the revision
                                            (set-revision! struct (add1 revision))
                                            ; Update the record and return its ID
                                            (db:update-record conn struct)
                                            (get-id struct))
                                          conn
                                          struct))
                                  (raise-exn exn:fail:snooze:revision
                                    "Structure has been revised since it was loaded from the database." 
                                    struct)))
                       ; Set the revision to 0, run the pipeline and return the ID of the newly saved struct.
                       (begin (set-revision! struct 0)
                              (call-with-pipeline
                               (append (entity-save-pipeline entity)
                                       (entity-insert-pipeline entity))
                               (lambda (conn struct)
                                 ; Insert the record and return the new ID
                                 ; NOTE: db:insert-record DOES NOT update the struct with the new ID post-save
                                 (let ([id (db:insert-record conn struct)])
                                   (set-id! struct id)
                                   id))
                               conn
                               struct)))))))))
      
      ;; delete! : [connection] persistent-struct -> void
      (define delete!
        (case-lambda
          ((struct)
           (delete! (current-connection) struct))
          ((conn struct)
           (unless (get-id struct)
             (raise-exn exn:fail:snooze
               (format "Cannot delete a struct that has not been saved to the database: ~a" struct)))
           ; Run delete pipeline
           (let* ([id       (get-id struct)]
                  [revision (get-revision struct)]
                  [entity   (struct-entity struct)])
             ; We used to have the privilege of deciding whether or not we wanted
             ; a transaction around this lot. Since the addition of revisions, we
             ; pretty much don't have a choice anymore.
             (call-with-transaction 
                 conn
               (lambda ()
                 ; Check the revision number with the stored revision:
                 ; delete if they match, throw an exception
                 ; if they don't.
                 (if (and revision (record-exists-with-revision? conn entity id revision))
                     (call-with-pipeline
                      (entity-delete-pipeline entity)
                      (lambda (conn struct)
                        (db:delete-record conn (struct-entity struct) (get-id struct))
                        ; The struct isn't in t he database any more, so it should
                        ; have an ID of #f.
                        (set-id! struct #f))
                      conn
                      struct)
                     (raise-exn exn:fail:snooze:revision
                       "Structure has been revised since it was loaded from the database." 
                       struct))))))))
      
      ;; find-gen : [connection] select -> result-generator
      ;;
      ;; The basic find function: returns a result generator.
      (define find-gen
        (case-lambda
          ((select)
           (find-gen (current-connection) select))
          ((conn select)
           (db:find-gen conn select))))
      
      ;; find-all : [connection] select -> (list-of result)
      ;;
      ;; Equivalent to (g:collect (find-gen ...))
      (define find-all
        (case-lambda
          ((select)
           (find-all (current-connection) select))
          ((conn select)
           (g:collect (find-gen conn select)))))
      
      ;; find-one : [connection] select -> (U result #f)
      ;;
      ;; Equivalent to (car (g:collect (find-gen ...)))
      (define find-one
        (case-lambda
          ((select)
           (find-one (current-connection) select))
          ((conn select)
           (let ([result ((find-gen conn select))])
             (if (g:end? result)
                 #f
                 result)))))
      
      ;; g:find : [connection] select -> result-generator
      ;;
      ;; Alias of find-gen.
      (define g:find find-gen)
      
      ;; call-with-transaction : connection thunk -> any
      ;;
      ;; A transaction is started and the thunk argument is
      ;; called. If the thunk is allowed to finish gracefully,
      ;; the transaction is committed. If, however, execution is
      ;; terminated via an exception or escape continuation,
      ;; the transaction is rolled back.
      (define call-with-transaction
        (case-lambda
          ((body)
           (call-with-transaction (current-connection) body))
          ((conn body)
           (db:call-with-transaction conn body))))
      
      ;; dump-sql 
      ;;     : select
      ;;       [#:output-port output-port]
      ;;       [#:format string]
      ;;    -> select
      ;;
      ;; Prints an SQL string to stdout as a side effect.
      (define dump-sql
        (lambda/kw (select 
                    #:key
                    [output-port (current-output-port)]
                    [format "~a~n"])
          (db:dump-sql select output-port format)))
      
      ;; find-by-id : [connection] entity (U integer #f) -> (U persistent-struct #f)
      (define find-by-id
        (case-lambda
          [(entity id)
           (find-by-id (current-connection) entity id)]
          [(conn entity id)
           (cond [(integer? id)
                  (let ([x (q:entity entity)])
                    (find-one (q:select #:from x #:where (q:= (q:attr x 'id) id))))]
                 [(not id)
                  #f]
                 [else (raise-exn exn:fail:snooze
                         (format "Expected (U integer #f), received ~s." id))])]))
      
      ; Helpers ----------------------------------
      
      ;; record-exists-with-revision? : connection entity integer integer -> boolean
      (define (record-exists-with-revision? conn entity id revision)
        (define x
          (q:entity entity))
        (define ans
          (find-one conn (q:select #:what  (q:attr x 'id)
                                   #:from  x
                                   #:where (q:and (q:= (q:attr x 'id) id)
                                                  (q:= (q:attr x 'revision) revision)))))
        (if ans #t #f))
      
      ))
  
  )
 