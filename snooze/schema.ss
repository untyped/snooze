#lang scheme/base

(require scheme/contract
         scheme/list
         "base.ss"
         "era/era.ss")

; Variables --------------------------------------

; (listof entity)
(define schema null)

; Procedures -------------------------------------

; -> (listof entity)
(define (schema-entities)
  schema)

; symbol -> (U entity #f)
(define (schema-entity name)
  (findf (lambda (entity)
           (eq? (entity-name entity) name))
         schema))

; entity -> void
(define (add-schema-entity! entity)
  (if (schema-entity (entity-name entity))
      (raise-exn exn:fail:snooze
        (format "Entity already exists in schema: ~a" (entity-name entity)))
      (set! schema (cons entity schema))))

; Provide statements -----------------------------

(provide/contract
 [schema-entities    (-> (listof entity?))]
 [schema-entity      (-> symbol? (or/c entity? false/c))]
 [add-schema-entity! (-> entity? void?)])
