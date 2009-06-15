#lang scheme/base

(require "base.ss")

(require scheme/class
         "core/core.ss"
         "generic/connection.ss"
         "sql/sql.ss")

; API procedures ---------------------------------

; (-> any) -> any
(define (call-with-cache #:snooze [snooze (current-snooze)] thunk)
  (send snooze call-with-cache thunk))

; (-> any) -> any
(define (call-with-connection #:snooze [snooze (current-snooze)] thunk)
  (send snooze call-with-connection thunk))

; -> thunk
(define (connect #:snooze [snooze (current-snooze)])
  (send snooze connect))

; -> void
(define (disconnect #:snooze [snooze (current-snooze)])
  (send snooze disconnect))

; -> connection
(define (current-connection #:snooze [snooze (current-snooze)])
  (send snooze current-connection))

; entity -> void
(define (create-table #:snooze [snooze (current-snooze)] entity)
  (send snooze create-table entity))

; entity -> void
(define (drop-table #:snooze [snooze (current-snooze)] entity)
  (send snooze drop-table entity))

; guid -> guid
(define (save! guid)
  (send (guid-snooze guid) save! guid))

; guid -> guid
(define (delete! guid)
  (send (guid-snooze guid) delete! guid))

; guid (connection guid -> guid) -> guid
(define (insert/id+revision! guid on-save)
  (send (guid-snooze guid) insert/id+revision! guid on-save))

; guid (connection guid -> guid) -> guid
(define (update/id+revision! guid on-save)
  (send (guid-snooze guid) update/id+revision! guid on-save))

; guid (connection guid -> guid) -> guid
(define (delete/id+revision! guid on-delete)
  (send (guid-snooze guid) delete/id+revision! guid on-delete))

; select -> (U result #f)
(define (find-one #:snooze [snooze (current-snooze)] select)
  (send snooze find-one select))

; select -> (U result #f)
(define (find-all #:snooze [snooze (current-snooze)] select)
  (send snooze find-all select))

; select -> (U result #f)
(define (g:find #:snooze [snooze (current-snooze)] select)
  (send snooze g:find select))

; entity (U integer #f) -> (U guid #f)
(define (find-by-id #:snooze [snooze (current-snooze)] entity id)
  (send snooze find-by-id entity id))

; (-> ans) any ... -> ans
(define (call-with-transaction #:snooze [snooze (current-snooze)] thunk . args)
  (send/apply snooze call-with-transaction thunk args))

; query -> string
(define (query->string #:snooze [snooze (current-snooze)] query)
  (send snooze query->string query))

; select [#:output-port output-port] [#:format string] -> select
(define (debug-sql #:snooze      [snooze (current-snooze)]
                   #:output-port [out    (current-output-port)]
                   #:format      [fmt    "~a"]
                   select)
  (send snooze debug-sql #:output-port out #:format fmt select))

; -> (listof symbol)
(define (table-names #:snooze [snooze (current-snooze)])
  (send snooze table-names))

; (U entity symbol) -> boolean
(define (table-exists? #:snooze [snooze (current-snooze)] name)
  (send snooze table-exists? name))

; Convenience syntaxes ---------------------------

; (_ select-args ...)
(define-syntax-rule (select-one args ...)
  (find-one (sql (select args ...))))

; (_ select-args ...)
(define-syntax-rule (select-all args ...)
  (find-all (sql (select args ...))))

; (_ select-args ...)
(define-syntax-rule (g:select args ...)
  (g:find (sql (select args ...))))

; Provide statements -----------------------------

(provide current-snooze
         select-one
         select-all
         g:select)

(provide/contract
 [call-with-cache       (->* (procedure?) (#:snooze (is-a?/c snooze<%>)) any)]
 [call-with-connection  (->* (procedure?) (#:snooze (is-a?/c snooze<%>)) any)]
 [connect               (->* () (#:snooze (is-a?/c snooze<%>)) void?)]
 [disconnect            (->* () (#:snooze (is-a?/c snooze<%>)) void?)]
 [current-connection    (->* () (#:snooze (is-a?/c snooze<%>)) connection?)]
 [create-table          (->* (entity?) (#:snooze (is-a?/c snooze<%>)) void?)]
 [drop-table            (->* ((or/c entity? symbol?)) (#:snooze (is-a?/c snooze<%>)) void?)]
 [save!                 (-> guid? guid?)]
 [delete!               (-> guid? guid?)]
 [insert/id+revision!   (-> guid? (-> connection? guid? guid?) guid?)]
 [update/id+revision!   (-> guid? (-> connection? guid? guid?) guid?)]
 [delete/id+revision!   (-> guid? (-> connection? guid? guid?) guid?)]
 [find-one              (->* (query?) (#:snooze (is-a?/c snooze<%>)) any)]
 [find-all              (->* (query?) (#:snooze (is-a?/c snooze<%>)) (or/c null? pair?))]
 [g:find                (->* (query?) (#:snooze (is-a?/c snooze<%>)) procedure?)]
 [find-by-id            (->* (entity? natural-number/c) (#:snooze (is-a?/c snooze<%>)) (or/c guid? #f))]
 [call-with-transaction (->* (procedure?) (#:snooze (is-a?/c snooze<%>)) #:rest (listof procedure?) any)]
 [query->string         (->* (query?) (#:snooze (is-a?/c snooze<%>)) string?)]
 [debug-sql             (->* (query?) (#:snooze (is-a?/c snooze<%>) #:output-port output-port? #:format string?) query?)]
 [table-names           (->* () (#:snooze (is-a?/c snooze<%>)) (listof symbol?))]
 [table-exists?         (->* ((or/c entity? symbol?)) (#:snooze (is-a?/c snooze<%>)) boolean?)])
