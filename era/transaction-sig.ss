#lang scheme/signature

; (parameter boolean)
;
; Determines whether persistent-structs should be backed up for
; restore if a transaction is rolled back. Defaults to #t.
; 
; Certain parts of Snooze (notably the audit trail system) set
; this to #f to perform privileged operations.
enable-transaction-backups?

; -> boolean
;
; This has been replaced by the in-transaction field in the connection structure.
;in-transaction?

; thunk any ... -> any
;
; Runs the supplied thunk in the scope of a new "transaction frame",
; which stores backups of persistent-structs. If the body fails to
; return gracefully, all structs are reverted to their previous states.
call-with-transaction-frame

; persistent-struct -> void
;
; Backs up a persistent-struct in the current transaction frame,
; ready for possible rollback later on.
;
; Does nothing if rollback-persistent-structs? is #f or a transaction
; is currently not in progress.
store-transaction-backup!
