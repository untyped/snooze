#lang scheme/signature

; query output-port -> void
display-query

; (U expression boolean) (listof column) output-port -> void
display-distinct

; (listof column) (listof column) output-port -> void
display-what

; source (listof column) output-port -> void
;
; doesn't display the word "FROM".
display-from

; expression (listof column) output-port -> void
;
; Doesn't display the words "ON" or "WHERE".
display-expression

; (listof column) (listof column) output-port -> void
;
; Doesn't display the words "GROUP BY".
display-group

; (listof order) (listof column) output-port -> void
;
; Doesn't display the words "ORDER BY".
display-order

; (U expression #f) (listof column) output-port -> void
; display-having

