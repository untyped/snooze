#lang scheme/signature

; Conversion between Scheme and DBMS primitives.
; 
; This signature provides procedures for converting between Scheme and database primitives.
; Procedures in this file do NOT have a one-to-one mapping to Snooze field types: rather
; they are defined as needed as helpers for the definitions in type-unit.ss.
;
; "Escaping" data involves converting a Scheme primitive into a corresponding string for
; insertion into SQL. "Parsing" data involves converting the value from the result of a 
; query back into a Scheme primitive.
;
; Depending on the library used to actually connect to the database, the target of 
; escaping and the source of parsing may not necessarily be the same thing. Hence, 
; escaping and parsing are not necessarily reciprocal operations.
;
; A Scheme data type may be stored differently by different database backends. Snooze
; does not guarantee that data in one DBMS can be ported to another without conversion.
; Snooze currently does not provide software tools for migrating from database to
; database.

; type any -> string
escape-value

; type any -> any
parse-value

; (listof type) -> ((U (vectorof database-value) #f) -> (U (vectorof scheme-value) #f))
make-parser
