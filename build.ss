#!/usr/bin/env mzscheme -q
#lang scheme

(require scheme/runtime-path
         scheme/system)

; Configuration ----------------------------------

; string
(define plt-version "4.2.1.5")

; path
(define-runtime-path planet-path "planet")

; Helpers ----------------------------------------

; string (listof string) -> string
(define (make-command command args)
  (string-join (cons command args) " "))

; Tasks ------------------------------------------

(define (env)
  (putenv "PLTVERSION" plt-version)
  (putenv "PLTPLANETDIR" (path->string planet-path)))

(define (autoplanet)
  (env)
  (system "mzscheme autoplanet.ss"))

(define (envvars)
  (autoplanet)
  (let ([path (make-temporary-file "mzscheme-envvars-~a.sh")])
    (with-output-to-file path
      (lambda ()
        (printf #<<ENDSCRIPT
export PLTVERSION=~a
export PLTPLANETDIR="~a"

ENDSCRIPT
                plt-version
                (path->string planet-path)))
      #:exists 'replace)
    (display (path->string path))))

(define (compile . args)
  (autoplanet)
  (system/exit-code (make-command "mzc -v main.ss" args)))

(define (test-compile . args)
  (autoplanet)
  (system/exit-code (make-command "mzc -v run-tests.ss" args)))

(define (test . args)
  (test-compile)
  (system/exit-code (make-command "mzscheme run-tests.ss" args)))

(match (vector->list (current-command-line-arguments))
  [(list-rest "envvars"      args) (envvars)]
  [(list-rest "compile"      args) (apply compile args)]
  [(list-rest "test-compile" args) (apply test-compile args)]
  [(list-rest "test"         args) (apply test args)])
