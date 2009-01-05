#lang scheme/signature

; Creating, updating and dropping tables and records.
;
; DBMSs vary greatly in their support for the different features of Snooze: the SQL
; produced by these procedures will be quite DBMS specific.
;
; Note that a single Snooze entity may map to several tables (and potentially other 
; structures) in a database. The create-sql procedure returns a single SQL string that
; creates all of the necessary structures in the correct order. The drop-sql procedure
; returns SQL to drop all of the structures created by create-sql.

; entity -> string
create-sql

; entity -> string
drop-sql       

; persistent-struct [boolean] -> string
; 
; Returns the SQL needed to insert a persistent-struct in the database.
; Allocates an ID and revision number automatically by default. The optional
; boolean argument can be set to #t to allocate the ID and revision already 
; present in the struct. This feature is used by the audit trail system.
insert-sql

; (listof persistent-struct) [boolean] -> (listof string)
; 
; Returns the SQL needed to insert a list of persistent-structs in the database.
; Allocates IDs and revision numbers automatically by default. The optional
; boolean argument can be set to #t to allocate the IDs and revisions already 
; present in the struct. This feature is used by the audit trail system.
insert-multiple-sql

; persistent-struct -> string
update-sql

; guid -> string
delete-sql
