#lang scheme

(require scheme/runtime-path
         (planet untyped/autoplanet:1))

(define-runtime-path dev-path
  "planetdev")

(remove-hard-links)
(install-local "untyped" "unlib.plt" 3 99 (build-path dev-path "unlib"))
