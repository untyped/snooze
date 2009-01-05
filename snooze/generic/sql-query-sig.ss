#lang scheme/signature

; Snooze uses structures to represent the various aspects of a SELECT query in a DBMS-
; independent way. This signature provides functions for converting these structures into
; SQL. While must of the data manipulation language in SQL is common across DBMSs, subtle
; DBMS-specific variations may exist (particularly in the quoted forms of identifiers and
; data primitives - see sql-quote^).

; query -> string
query-sql
