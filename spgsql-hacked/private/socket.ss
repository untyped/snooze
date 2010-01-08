;; Copyright 2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

(module socket mzscheme
  (require (lib "etc.ss")
           (lib "foreign.ss"))
  (unsafe!)
  (provide unix-socket-connect)

  ;; The solaris code is untested (and thus disabled).

  ;; unix-socket-connect : pathlike -> input-port output-port
  ;; Connects to the unix domain socket associated with the given path.
  (define (unix-socket-connect path0)
    (begin-with-definitions
      (define path (check-pathlike 'unix-socket-connect path0))
      (define s (make-socket))
      (unless (positive? s)
        (error 'unix-socket-connect
               "failed to create socket"))
      (define addr (make-unix-sockaddr path))
      (define addrlen (+ (ctype-sizeof _short) (bytes-length path)))
      (define ce (_connect s addr addrlen))
      (unless (zero? ce)
        (_close s)
        (raise-user-error
         'unix-socket-connect
         "failed to connect socket to path: ~s" path))
      (with-handlers ([(lambda (e) #t)
                       (lambda (e)
                         (_close s)
                         (raise e))])
        (_make_fd_output_port s 'socket #f #f #t))))

  (define platform
    (let ([os (system-type 'os)]
          [machine (system-type 'machine)])
      (cond [(eq? os 'macosx) 'macosx]
            [(regexp-match #rx"Linux.*i.86" machine) 'linux86]
            [(regexp-match #rx"SunOS" machine) #f #;'solaris]
            [else #f])))

  (define AF_UNIX 1)
  (define SOCK_STREAM
    (case platform
      ((linux86 macosx) 1)
      ((solaris) 2)
      (else #f)))

  (define (make-socket)
    (unless (and AF_UNIX SOCK_STREAM)
      (raise-user-error
       'unix-socket-connect
       "unix-domain sockets not supported on this platform"))
    (_socket AF_UNIX SOCK_STREAM 0))

  (define _sockaddr_un_path_part
    (case platform
      ((linux86 solaris)
       (make-cstruct-type (build-list 108 (lambda (i) _byte))))
      ((macosx)
       (make-cstruct-type (build-list 104 (lambda (i) _byte))))
      (else
       ;; kluge: so that later definitions go through.
       _int)))

  (define-cstruct _sockaddr_un
    ([sun_family _short]
     [sun_path   _sockaddr_un_path_part]))

  (define-cstruct _macosx_sockaddr_un
    ([sun_len    _ubyte]
     [sun_family _ubyte]
     [sun_path   _sockaddr_un_path_part]))

  (define (ffi name type)
    (case platform
      ((linux86 solaris macosx)
       (get-ffi-obj name #f type (lambda () #f)))
      (else
       (lambda _ (error name "not supported")))))

  (define _socket
    (ffi "socket" (_fun _int _int _int -> _int)))
  (define _connect
    (ffi "connect"
         (case platform
           ((linux86 solaris)
            (_fun _int _sockaddr_un-pointer _int -> _int))
           ((macosx)
            (_fun _int _macosx_sockaddr_un-pointer _int -> _int)))))
  (define _close
    (ffi "close" (_fun _int -> _int)))
  (define _make_fd_output_port
    (ffi "scheme_make_fd_output_port"
         (_fun _int _scheme _bool _bool _bool -> _scheme)))

  (define (make-unix-sockaddr path)
    (case platform
      ((linux86 solaris)
       (make-sockaddr_un AF_UNIX path))
      ((macosx)
       (make-macosx_sockaddr_un (bytes-length path) AF_UNIX path))))

  (define (check-pathlike function path0)
    (unless (or (string? path0) (path? path0) (bytes? path0))
      (raise-type-error function
                        "path, string, or bytes"
                        path0))
    (let ([path
           (cond [(string? path0) (string->bytes/utf-8 path0)]
                 [(path? path0) (path->bytes path0)]
                 [(bytes? path0) path0])])
      (unless (< (bytes-length path) 100)
        (raise-type-error 'unix-socket-connect
                          "path (of less than 100 bytes)"
                          path0))
      path))
  )
