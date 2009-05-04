#lang scheme

(require scribble/decode
         scribble/eval
         scribble/manual
         scribble/scheme
         scribble/struct
         scribble/urls
         scribblings/icons
         (planet cce/scheme:4/planet)
         (planet untyped/unlib:3/scribble)
         (for-label scheme
                    (planet untyped/unlib:3/generator)
                    (planet untyped/unlib:3/gen)
                    "db.ss"))

; Helpers ----------------------------------------

; string
(define url:mirrors.plt 
  "http://planet.plt-scheme.org/display.ss?package=mirrors.plt&owner=untyped")
(define url:spgsql.plt 
  "http://planet.plt-scheme.org/display.ss?package=spgsql.plt&owner=schematics")
(define url:sqlite.plt 
  "http://planet.plt-scheme.org/display.ss?package=sqlite.plt&owner=jaymccarthy")
(define url:unlib.plt 
  "http://planet.plt-scheme.org/display.ss?package=unlib.plt&owner=untyped")

; -> eval
(define (make-snooze-eval)
  (define-eval snooze-eval 
    scheme
    (planet untyped/unlib:3/generator)
    (planet untyped/unlib:3/gen)
    ; It seems that the sandbox evaluator resolves files
    ; with respect to the current directory, not the
    ; directory in which this file is written.  Since we
    ; run planet from the directory above we have to tell
    ; the sandbox to find db.ss in the scribblings directory.
    "scribblings/db.ss")
  snooze-eval)

; string -> margin-note
;
; Creates a margin note pointing to another section in the manual.
; Pilfered from collects/scribblings/guide/guide-utils.ss... yuk yuk yuk.
(define (intmore tag)
  (apply margin-note (decode-content (list finger (secref tag)))))

; string string ... -> margin-note
;
; Creates a margin note pointing to an external web site.
; Pilfered from collects/scribblings/guide/guide-utils.ss... yuk yuk yuk.
(define (extmore url . texts)
  (apply margin-note (decode-content (list finger (apply link url texts)))))

; Provide statements --------------------------- 

(provide (all-from-out scheme
                       scribble/eval
                       scribble/manual
                       scribble/urls
                       (planet cce/scheme:4/planet)
                       (planet untyped/unlib:3/scribble))
         (all-defined-out)
         (for-label (all-from-out scheme
                                  (planet untyped/unlib:3/generator)
                                  (planet untyped/unlib:3/gen)
                                  "db.ss")))
