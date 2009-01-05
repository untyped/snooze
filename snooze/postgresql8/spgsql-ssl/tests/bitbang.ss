;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module bitbang mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (require "../private/bitbang.ss")
  
  (provide bitbang-test)
  
  ;; invariantize : ('a -> void) (-> 'a) ('a 'a -> boolean) -> ('a -> void)
  ;; Given a writer, a reader, and a comparison function, produces a procedure
  ;; that write a value, reads it back in, and checks the two are the same.
  (define invariantize
    (lambda (writer reader f)
      (lambda (val)
        (let [(port (open-output-string))]
          (writer port val)
          (check f 
                  (reader (open-input-bytes (get-output-bytes port)))
                  val)))))
  ;; invariantize2
  ;; Like invariantize, but reader and writer take extra argument.
  (define invariantize2
    (lambda (writer reader f)
      (lambda (val param)
        (let [(port (open-output-bytes))]
          (writer port val param)
          (check f
                  (reader (open-input-bytes (get-output-bytes port)) param)
                  val)))))
  
  (define (n= a b)
    (not (= a b)))
  (define (nequal? a b)
    (not (equal? a b)))
  (define (string->vector s)
    (apply vector (string->list s)))
  
  (define bitbang-test
    (test-suite "Bitbang"
      (test-case "int16"
        (map (invariantize write-int16 read-int16 =)
             (list 0 1 65535))
        '(map (invariantize write-int16 read-int16 n=)
              (list 65536 74390 -1)))
      (test-case "int32"
        (map (invariantize write-int32 read-int32 =)
             (list 0 1 2 65535 65536 74930 4294967295))
        '(map (invariantize write-int32 read-int32 n=)
              (list -1 4294967296)))
      (test-case "tstring"
        (map (invariantize write-tstring read-tstring equal?)
             (list "" "a" "hi there"))
        (map (invariantize write-tstring read-tstring nequal?)
             (list "\0\0a" "goo\0ha")))
      (test-case "limstring"
        (map (invariantize2 
              (lambda (p v s) (write-limstring p s v))
              read-limstring
              equal?)
             (list "a" "hi" "foobar" "mes\0sse\n\0dup")
             (list 1 2 6 12))
        (map (invariantize2 
              (lambda (p v s) (write-limstring p s v))
              read-limstring
              nequal?)
             (list "foobar")
             (list 20)))))
  )