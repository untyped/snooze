;; Copyright 2000-2005 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module protocol2 mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (require "../private/bitbang.ss"
           "../private/protocol-structures.ss"
           "../private/protocol2.ss")
  
  (provide protocol-test
           protocol-xfail-test
           message-generator-test
           predicates-test)
  
  (define (n= a b)
    (not (= a b)))
  (define (nequal? a b)
    (not (equal? a b)))
  (define (string->vector s)
    (apply vector (string->list s)))
  
  (define-syntax roundtrip
    (syntax-rules ()
      [(roundtrip (in out next) (setup ...) expr ...)
       (let ()
         (define-values (in out) (make-pipe))
         (define pro (protocol2:new in out))
         (define mg (protocol2:reset pro))
         (define (next-message)
           (let [(msg (message-generator:current mg))]
             (set! mg (message-generator:next mg))
             msg))
         setup ...
         (close-output-port out)
         (let ([next (next-message)])
           expr ...))]))

  (define (test-round-trip msg)
    (define-values (stdin stdout) (make-pipe))
    (define pro (protocol2:new stdin stdout))
    (define mg (protocol2:reset pro))
    (define (next-message)
      (let [(msg (message-generator:current mg))]
        (set! mg (message-generator:next mg))
        msg))
    (protocol2:encode pro msg)
    (let [(msg2 (message-generator:current mg))]
      (set! mg (message-generator:next mg))
      (check-equal? msg msg2)))
  
  (define protocol-test
    (test-suite "Protocol - Encode/Decode"
      (test-case "AuthenticationOK"
        (roundtrip (stdin stdout R)
          [(write-char #\R stdout)
           (write-int32 stdout 0)]
          (check-pred Authentication? R)
          (check-eq? (Authentication-method R) 'ok)
          (test-round-trip R)))
      (test-case "AuthenticationKerberosV4"
        (roundtrip (stdin stdout R)
          [(write-char #\R stdout)
           (write-int32 stdout 1)]
          (check-pred Authentication? R)
          (check-eq? (Authentication-method R) 'kerberosV4)
          (test-round-trip R)))
      (test-case "AuthenticationKerberosV5"
        (roundtrip (stdin stdout R)
          [(write-char #\R stdout)
           (write-int32 stdout 2)]
          (check-pred Authentication? R)
          (check-eq? (Authentication-method R) 'kerberosV5)
          (test-round-trip R)))
      (test-case "AuthenticationUnencryptedPassword"
        (roundtrip (stdin stdout R)
          [(write-char #\R stdout)
           (write-int32 stdout 3)]
          (check-pred Authentication? R)
          (check-eq? (Authentication-method R) 'unencrypted-password)
          (test-round-trip R)))
      (test-case "AuthenticationEncryptedPassword"
        (roundtrip (stdin stdout R)
          [(write-char #\R stdout)
           (write-int32 stdout 4)
           (write-bytes #"AZ" stdout)]
          (check-pred AuthenticationEncryptedPassword? R)
          (check-eq? (Authentication-method R) 'encrypted-password)
          (check-equal? (AuthenticationEncryptedPassword-salt R) #"AZ")
          (test-round-trip R)))
      (test-case "BackendKeyData"
        (roundtrip (stdin stdout R)
          [(write-char #\K stdout)
           (write-int32 stdout 42)
           (write-int32 stdout 1089)]
          (check-equal? R (make-BackendKeyData 42 1089))
          (test-round-trip R)))
      (test-case "CompletedResponse"
        (roundtrip (stdin stdout R)
          [(write-char #\C stdout)
           (write-tstring stdout "TEST: All your base")]
          (check-equal? R (make-CompletedResponse "TEST: All your base"))
          (test-round-trip R)))
      (test-case "CursorResponse"
        (roundtrip (stdin stdout R)
          [(write-char #\P stdout)
           (write-tstring stdout "wooky")]
          (check-equal? R (make-CursorResponse "wooky"))
          (test-round-trip R)))
      (test-case "EmptyQueryResponse"
        (roundtrip (stdin stdout R)
          [(write-char #\I stdout)
           (write-tstring stdout "")]
          (check-pred EmptyQueryResponse? R)
          (test-round-trip R)))
      (test-case "ErrorResponse"
        (roundtrip (stdin stdout R)
          [(write-char #\E stdout)
           (write-tstring stdout "I'm angry")]
          (check-equal? R (make-ErrorResponse #f "I'm angry" #f))
          (test-round-trip R)))
      
      ;; Test also error parsing stuff
      
      (test-case "NoticeResponse"
        (roundtrip (stdin stdout R)
          [(write-char #\N stdout)
           (write-tstring stdout "You are a monkey")]
          (check-equal? R 
                         (make-NoticeResponse "NOTICE" "You are a monkey"))
          (test-round-trip R)))
      (test-case "NotificationResponse"
        (roundtrip (stdin stdout R)
          [(write-char #\A stdout)
           (write-int32 stdout 12)
           (write-tstring stdout "Dropped")]
          (check-equal? R (make-NotificationResponse 12 "Dropped"))
          (test-round-trip R)))
      (test-case "ReadyForQuery"
        (roundtrip (stdin stdout R)
          [(write-char #\Z stdout)]
          (check-equal? R (make-ReadyForQuery))
          (test-round-trip R)))
      (test-case "RowDescription"
        (roundtrip (stdin stdout R)
          [(write-char #\T stdout)
           (write-int16 stdout 1)
           (write-tstring stdout "one")
           (write-int32 stdout 4)
           (write-int16 stdout 7)
           (write-int32 stdout 1)]
          (check-equal? 
           R 
           (make-RowDescription (list (make-FieldInfo "one" 4 7 1))))
          (test-round-trip R)))
      (test-case "AsciiRow"
        "Not tested")
      (test-case "BinaryRow"
        "Not tested")
      (test-case "FunctionCall"
        "Not tested")))

  (define protocol-xfail-test
    (test-suite "Protocol - expect fail"
      (test-case "CopyInResponse"
        (roundtrip (stdin stdout R)
          [(write-char #\G stdout)]
          (check-equal? R (make-CopyInResponse))
          (test-round-trip R)))
      (test-case "CopyOutResponse"
        (roundtrip (stdin stdout R)
          [(write-char #\H stdout)
           (write-astring stdout (format "foo~n"))
           (write-astring stdout (format "fum~n"))
           (write-astring stdout (string #\\ #\. #\newline))]
          (check-equal? R (make-CopyOutResponse (list "foo" "fum")))
          (test-round-trip R)))
      (test-case "FunctionResultResponse"
        (roundtrip (stdin stdout R)
          [(write-char #\V stdout)
           (write-char #\G stdout)
           (write-int32 stdout 4)
           (write-astring stdout "whoo")
           (write-byte 0 stdout)]
          (check-equal? R (make-FunctionResultResponse "whoo"))))
      ;;(test-round-trip R))))
      (test-case "FunctionVoidResponse"
        (roundtrip (stdin stdout R)
          [(write-char #\V stdout)
           (write-byte 0 stdout)]
          (check-equal? R (make-FunctionVoidResponse))))
      ;;(test-round-trip R))))
      ))

  (define (setup-protocol message-stream)
    (let-values [((in out) (make-pipe))]
      (let [(p (protocol2:new in out))]
        (for-each (lambda (R) (protocol2:encode p R)) message-stream)
        p)))
  
  (define message-stream1
    (list (make-CursorResponse "foo")
          (make-CompletedResponse "waka-waka-waka")
          (make-EmptyQueryResponse "anything you want here, nobody listens...")
          (make-CompletedResponse "fill-in-the-blank")
          (make-ReadyForQuery)))
  
  (define message-stream2-segment1
    (list (make-CursorResponse "foo")
          (make-CompletedResponse "waka-waka-waka")
          (make-EmptyQueryResponse "anything you want here, nobody listens...")
          (make-CompletedResponse "fill-in-the-blank")
          (make-ReadyForQuery)))
  (define message-stream2-segment2
    (list (make-ErrorResponse "butter" "pecans" #f)
          (make-NotificationResponse 713 "the eagle has landed")
          (make-CompletedResponse "getting there")
          (make-CompletedResponse "")
          (make-ReadyForQuery)))
  (define message-stream2 
    (append message-stream2-segment1 message-stream2-segment2))
  
  (define message-generator-test
    (test-suite 
        "Protocol - Message Generators"
      (test-case "Reset preserves stream"
        (let positionloop [(n (length message-stream1))]
          (when (positive? n)
            (printf "!! positionloop ~s~n" n)
            (let* [(p (setup-protocol message-stream1))
                   (mg (protocol2:reset p))]
              (let messageloop [(messages null) (mg mg) (countdown n)]
                (when (zero? countdown) (protocol2:reset p))
                (let* [(R (message-generator:current mg))
                       (mg (message-generator:next mg))
                       (messages (cons R messages))]
                  (cond [(ReadyForQuery? R)
                         (check-equal? (reverse messages) 
                                        message-stream1)
                         (positionloop (sub1 n))]
                        [else (messageloop messages 
                                           mg
                                           (sub1 countdown))])))))))
      ))

  (define predicates-test
    (test-suite
     "Protocol - predicates"
     (test-case
      "constraint-error? correct"
      (check-pred constraint-error?
                  (make-ErrorResponse "Oh dear!" "A message" 23001))
      (check-false (constraint-error? (make-ErrorResponse "a" "b" 20001)))
      (check-false (constraint-error? 12)))))
  )
