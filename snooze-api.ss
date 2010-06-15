#lang scheme/base

(require "base.ss")

(require (for-syntax scheme/base)
         "core/core.ss"
         "common/connection.ss"
         "sql/sql.ss")

; API procedures ---------------------------------

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

; snooze-struct -> snooze-struct
(define (save! #:snooze [snooze (current-snooze)] struct)
  (send snooze save! struct))

; snooze-struct -> snooze-struct
(define (delete! #:snooze [snooze (current-snooze)] struct)
  (send snooze delete! struct))

; select -> (U result #f)
(define (find-one #:snooze [snooze (current-snooze)] select)
  (send snooze find-one select))

; select -> (U result #f)
(define (find-all #:snooze [snooze (current-snooze)] select)
  (send snooze find-all select))

; select -> (U result #f)
(define (g:find #:snooze [snooze (current-snooze)] select)
  (send snooze g:find select))

; entity (U integer #f) -> (U snooze-struct #f)
(define (find-by-id #:snooze [snooze (current-snooze)] entity id)
  (send snooze find-by-id entity id))

; database-guid -> (U snooze-struct #f)
(define (find-by-guid #:snooze [snooze (current-snooze)] guid)
  (send snooze find-by-guid guid))

; (listof database-guid) -> (listof snooze-struct)
(define (find-by-guids #:snooze [snooze (current-snooze)] guids)
  (send snooze find-by-guids guids))

; (listof snooze-struct) attribute -> (listof snooze-struct)
(define (load-related! #:snooze [snooze (current-snooze)] structs attr)
  (send snooze load-related! structs attr))

; (-> ans) any ... -> ans
(define (call-with-transaction #:snooze [snooze (current-snooze)] #:metadata [metadata null] thunk)
  (send snooze call-with-transaction #:metadata metadata thunk))

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

; (_ [#:snooze snooze-expr] expr ...)
(define-syntax (with-connection stx)
  (syntax-case stx ()
    [(_ #:snooze snooze expr ...) (syntax/loc stx (call-with-connection #:snooze snooze (lambda () expr ...)))]
    [(_ expr ...)                 (syntax/loc stx (call-with-connection (lambda () expr ...)))]))

; (_ [#:snooze snooze-expr] [#:metadata list-expr] expr ...)
(define-syntax (with-transaction stx)
  (syntax-case stx ()
    [(_ #:metadata metadata expr ...)                 (syntax/loc stx (call-with-transaction #:metadata metadata (lambda () expr ...)))]
    [(_ #:snooze snooze #:metadata metadata expr ...) (syntax/loc stx (call-with-transaction #:snooze snooze #:metadata metadata (lambda () expr ...)))]
    [(_ #:snooze snooze expr ...)                     (syntax/loc stx (call-with-transaction #:snooze snooze (lambda () expr ...)))]
    [(_ expr ...)                                     (syntax/loc stx (call-with-transaction (lambda () expr ...)))]))

; Provide statements -----------------------------

(provide current-snooze
         select-one
         select-all
         g:select
         with-connection
         with-transaction)

(provide/contract
 [call-with-connection  (->* (procedure?) (#:snooze (is-a?/c snooze<%>)) any)]
 [connect               (->* () (#:snooze (is-a?/c snooze<%>)) void?)]
 [disconnect            (->* () (#:snooze (is-a?/c snooze<%>)) void?)]
 [current-connection    (->* () (#:snooze (is-a?/c snooze<%>)) connection?)]
 [create-table          (->* (entity?) (#:snooze (is-a?/c snooze<%>)) void?)]
 [drop-table            (->* ((or/c entity? symbol?)) (#:snooze (is-a?/c snooze<%>)) void?)]
 [save!                 (->* (snooze-struct?)
                             (#:snooze (is-a?/c snooze<%>))
                             (and/c snooze-struct? snooze-struct-has-revision?))]
 [delete!               (->* (snooze-struct?)
                             (#:snooze (is-a?/c snooze<%>))
                             (and/c snooze-struct? (not/c snooze-struct-has-revision?)))]
 [find-one              (->* (query?) (#:snooze (is-a?/c snooze<%>)) any)]
 [find-all              (->* (query?) (#:snooze (is-a?/c snooze<%>)) (or/c null? pair?))]
 [g:find                (->* (query?) (#:snooze (is-a?/c snooze<%>)) procedure?)]
 [find-by-id            (->* (entity? natural-number/c) (#:snooze (is-a?/c snooze<%>)) (or/c snooze-struct? #f))]
 [find-by-guid          (->* (database-guid?) (#:snooze (is-a?/c snooze<%>)) (or/c snooze-struct? #f))]
 [find-by-guids         (->* ((listof database-guid?)) (#:snooze (is-a?/c snooze<%>)) (listof snooze-struct?))]
 [load-related!         (->* ((listof snooze-struct?) attribute?) (#:snooze (is-a?/c snooze<%>)) (listof snooze-struct?))]
 [call-with-transaction (->* (procedure?) (#:snooze (is-a?/c snooze<%>) #:metadata list?) any)]
 [query->string         (->* (query?) (#:snooze (is-a?/c snooze<%>)) string?)]
 [debug-sql             (->* (query?) (#:snooze (is-a?/c snooze<%>) #:output-port output-port? #:format string?) query?)]
 [table-names           (->* () (#:snooze (is-a?/c snooze<%>)) (listof symbol?))]
 [table-exists?         (->* ((or/c entity? symbol?)) (#:snooze (is-a?/c snooze<%>)) boolean?)])
