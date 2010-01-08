;; Copyright 2000-2007 Ryan Culpepper
;; Released under the terms of the modified BSD license (see the file
;; COPYRIGHT for terms).

;; This file provides all the necessary structures, objects, and functions
;; for using spgsql to open a connection to a PostgreSQL database.

(module spgsql mzscheme
  (require (lib "class.ss")
           "private/compat.ss"
           "private/interfaces.ss"
           "private/connection.ss"
           "private/sql-data.ss"
           "private/socket.ss")

  (provide connect
           (struct SimpleResult (command))
           (struct Recordset (info data))
           (struct FieldInfo (name))

           sql-null
           sql-null?

           (struct sql-date (year month day))
           (struct sql-time (hour minute second nanosecond tz))
           (struct sql-timestamp (year month day hour minute second nanosecond tz))

           ;; Deprecated: for backwards compatibility
           sql-timestamp-fraction
           set-sql-timestamp-fraction!

           sql-datetime->srfi-date
           srfi-date->sql-date
           srfi-date->sql-time
           srfi-date->sql-time-tz
           srfi-date->sql-timestamp
           srfi-date->sql-timestamp-tz

           format-sql
           concat-sql)

  (define connect
    (lambda/kw (#:key
                (server #f)
                (port #f)
                (socket #f)
                (input-port #f)
                (output-port #f)
                (user (raise-user-error 'connect "user argument is mandatory"))
                (database (raise-user-error 'connect "database argument is mandatory"))
                (password #f)
                (allow-cleartext-password? #f)
                (ssl 'no)
                (ssl-encrypt 'sslv2-or-v3)
                (mixin values)
                #:forbid-anything)
      (let ([connection-options
             (+ (if (or server port) 1 0)
                (if socket 1 0)
                (if (or input-port output-port) 1 0))])
        (when (> connection-options 1)
          (raise-user-error 'connect
                            (string-append
                             "cannot specify more than one of server/port, "
                             "socket, or input-port/output-port arguments"))))
      (when (or input-port output-port)
        (unless (and input-port output-port)
          (raise-user-error 'connect
                            "must give input-port and output-port arguments together")))
      (let ([c (new (mixin connection%)
                    (allow-cleartext-password? allow-cleartext-password?))])
        (send c set-ssl-options ssl ssl-encrypt)
        (cond [socket
               (let-values ([(in out) (unix-socket-connect socket)])
                 (send c attach-to-ports in out))]
              [input-port
               (send c attach-to-port input-port output-port)]
              [else
               (let ([server (or server "localhost")]
                     [port (or port 5432)])
                 (let-values ([(in out) (tcp-connect server port)])
                   (send c attach-to-ports in out)))])
        (send c start-connection-protocol database user password)
        (send c exec "SET standard_conforming_strings = on;")
        c)))
  
  #|
  ;; list-databases : connection% -> (listof string)
  (define (list-databases c)
    (send c query-list "select datname::text from pg_database"))

  ;; list-tables : connection% boolean -> (listof string)
  (define (list-tables c)
    (send c query-list
          "select tablename::text from pg_tables where tableowner != 'postgres'"))

  ;; list-views : connection -> (list-of string)
  (define (list-views c)
    (send c query-list
          "select viewname::text from pg_views where viewowner != 'postgres'"))

  ;; list-fields : connection% string -> (list-of string)
  (define (list-fields c table)
    (send c query-list
          (format-sql "select A.attname::text from pg_attribute A, pg_class T "
                      "where A.attrelid = T.relfilenode "
                      "and T.relname = ~a and A.attnum > 0"
                      [string table])))
  |#

  )
