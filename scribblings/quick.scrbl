#lang scribble/doc

@(require "base.ss")
          
@title[#:tag "quick"]{Quick start}

Thank you for trying Snooze. Read on to find out how you can get everything up and running on your system. If you have any questions please don't hesisitate to contact us; our email addresses are at the beginning of this manual.

We hope you enjoy Snooze!

-- @link["http://www.untyped.com"]{Untyped}

@section{Install a DBMS}

The first thing you'll need to do is download and install a DBMS. Snooze currently supports two database backends: SQLite 3.6+ and PostgreSQL 8.3+. Get hold of one of these, install it, configure it, and set up a blank database:

@itemize{
  @item{if you are using SQLite, creating a database is as simple as creating a file somewhere on your local filesystem;} 
  @item{if you are using PostgreSQL your will need to create a user with the necessary privileges and set up your @filepath{pg_hba.conf} to allow TCP/IP connections from localhost.}}

This is probably the hardest part of the setup process, and unfortunately it falls outside the scope of this document. If you are looking for a quick-and-easy setup for your system, you might want to follow the installation instructions for your platform on the @link["http://www.sqlite.org"]{SQLite web site}.

@section{Link the Snooze API}

Once have chosen your DBMS platform, you will need to create a Snooze interface in Scheme. This involves creating two @scheme[scheme/class] objects: one contains database-independent code, the other contains code specific to your database.

Create a blank file in your project directory called @filepath{db.ss} and enter the following template code:

@schememod[
  scheme/base

  (require (planet untyped/snooze:2)
           (planet untyped/snooze:2/<<DB>>/<<DB>>))
  
  (define-snooze-interface 
    (make-snooze (make-database (code:comment "TODO: arguments..."))))
    
  (provide (all-from-out (planet untyped/snooze:2))
           (snooze-interface-out))]

Change @litchar{<<DB>>} to @litchar{postgresql8} or @litchar{sqlite3} depending on your choice of DBMS.

@scheme[define-snooze-interface] is an unhygeinic macro that takes a snooze object as an argument and defines wrapper procedures for each of its important methods. @scheme[snooze-interface-out] provides those procedures to the rest of your application. All of the procedures and macros described in the rest of this manual are provided by @scheme[define-snooze-interface] or directly from @filepath{snooze.ss}.

All you need to do to complete @filepath{db.ss} is fill in the arguments to @scheme[make-database]. The arguments will be different depending on whether you are using SQLite or PostgreSQL:

@itemize{
  @item{if you are using SQLite, rewrite the call to @scheme[make-database] like this:

    @schemeblock[(make-database <<PATH>>)]

    where @litchar{<<PATH>>} is either a @scheme[path] to your database file or one of the special symbols @scheme[':memory:] (in-memory database) or @scheme[':temp:] (temporary file);}
        
  @item{if you are using PostgreSQL, rewrite the call to @scheme[make-database] like this:
  
    @schemeblock[(make-database #:server   "<<SERVER>>"
                                #:port     <<PORT>>
                                #:database "<<DATABASE>>"
                                #:username "<<USERNAME>>"
                                #:password "<<PASSWORD>>")]
        
    where:
    
    @itemize{
      @item{@litchar{<<SERVER>>} is the hostname of your database server (e.g. @litchar{localhost});}
      @item{@litchar{<<PORT>>} is the port number to connect on (usually 5432; note that this is an integer - not a string);}
      @item{@litchar{<<DATABASE>>} the name of the relevant database on your server;}
      @item{@litchar{<<USERNAME>>} is the username you want to connect with;}
      @item{@litchar{<<PASSWORD>>} is the password for this username (this is an optional argument).}}}}
      
Note: the PostgreSQL version of @schemeid[make-database] has additional keyword arguments to configure SSL if you need it.

@section{Start using Snooze}

Now you are ready to connect to a database and start storing data. Create a new module called @filepath{test.ss} in the same directory as @filepath{db.ss}. Edit this new module and enter the following test script:

@schememod[
  scheme
  
  (require "db.ss")
  
  (code:comment "Define a datatype that we can save to the database:")
  (define-persistent-struct person
    ([name   type:string]
     [age    type:integer]
     [gender type:symbol]))

  (code:comment "Print the people that are saved in the database:")
  (define (print-people)
    (define people (find-all (sql (select #:from person))))
    (printf "People in the database:~n")
    (if (null? people)
        (printf "    NONE~n")
        (for ([person people])
          (printf "    ~s~n" person))))
    
  (code:comment "Create some people (in memory):")
  (define-values (alice bob charlie)
    (values (make-person "Alice"   20 'F)
            (make-person "Bob"     25 'M)
            (make-person "Charlie" 30 'F)))

  (code:comment "Connect to the database:")
  (call-with-connection
    (lambda ()

      (code:comment "Create a database table to house person data:")      
      (create-table person)
          
      (code:comment "There aren't any people saved yet:")
      (print-people)
    
      (save! alice)
      (save! bob)
      (save! charlie)
      
      (code:comment "Now they're in the database:")
      (print-people)
      
      (delete! alice)
      (delete! bob)
      (delete! charlie)
      
      (code:comment "And now they're gone again:")
      (print-people)
      
      (code:comment "Okay, we're done. Drop the table:")
      (drop-table person)))]
      
Run the code with the following command line:

@commandline{mzscheme test.ss}

You should see the following output:

@verbatim[#<<ENDOUTPUT
  People in the database:
      NONE
  People in the database:
      #(struct:person "Alice" 20 'F)
      #(struct:person "Bob" 25 'M)
      #(struct:person "Charlie" 30 'F)
  People in the database:
      NONE
ENDOUTPUT
  ]

Several bits of code Snooze functionality are demonstrated in this example.
Click on the names below to see the definitions of the relevant procedures:

@itemize{
  @item{@scheme[define-persistent-struct] is a macro that defines a
    structure type that can be saved to the database: the type specifiers
    give Snooze hints about how to serialize and deserialize field values;}
  @item{@scheme[call-with-connection] sets up a default connection that is
    used by many Snooze API calls: the connection is established and
    terminated as control enters and leaves the argument thunk via
    continuation jumps or the natural flow of the program;}
  @item{@scheme[find-all] and @scheme[q:select] are part of the Snooze
    query mechanism (which has most of the expressiveness of full SQL);}
  @item{@scheme[create-table] and @scheme[drop-table] are used to issue
    the corresponding @litchar{CREATE TABLE} and @litchar{DROP TBALE} commands
    to the database;}
  @item{@scheme[save!] is used to save a persistent struct to the database
    and @scheme[delete!] is used to delete it again.}}

That completes this quick start guide. In @filepath{db.ss} you now have 
everything you need to start defining your own persistent struct types
and saving structs to your database. The remainder of this manual serves a 
reference for the various parts of Snooze.
