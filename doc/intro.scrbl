#lang scribble/doc

@require[(file "base.ss")]

@title{Quick Start}

Thank you for taking the time to try Snooze! Read on to find out how you can get everything up and running on your system. If you have any questions please don't hesisitate to contact us: our email addresses are at the beginning of this manual.

We hope you enjoy Snooze!

-- @link["http://www.untyped.com"]{The Untyped team}

@section{Install a DBMS}

The first thing you'll need to do is download and install a DBMS. Snooze currently supports two database backends: SQLite 3.x and PostgreSQL 8.x. Get hold of one of these, install it, configure it, and set up a blank database:

@itemize{
  @item{if you are using SQLite, creating a database is as simple as creating a file somewhere on your local filesystem;} 
  @item{if you are using PostgreSQL your will need to create a user with the necessary privileges and set up your @file{pg_hba.conf} appropriately.}}

This is probably the hardest part of the setup process: unfortunately, it falls outside the scope of this document. If you are looking for a quick-and-easy setup for your system, we recommend you follow the installation instructions for your platform on the @link["http://www.sqlite.org"]{SQLite web site}.

@section{Link the Snooze API}

Once have chosen your DBMS platform, you will need to create a Snooze interface in Scheme. This involves linking two Snooze modules together: one contains DBMS-independent code, the other contains code specific to your type of DBMS.

Create a blank file in your project directory called @file{db.ss} and enter the following template code:

@schememod[
  scheme

  (require (planet "snooze.ss" ("untyped" "snooze.plt" 1))
           (prefix db: (planet "<<DB>>.ss" ("untyped" "snooze.plt" 1) "<<DB>>")))
  
  (define-snooze-interface db:db@)
  
  (code:comment "TODO: Insert definition of configuration structure") 
    
  (provide (all-from (planet "snooze.ss" ("untyped" "snooze.plt" 1))))
  
  (provide (all-defined))]

Change @litchar{<<DB>>} to @litchar{postgresql8} or @litchar{sqlite3} depending on your choice of DBMS.

@scheme[define-snooze-interface] is an unhygeinic macro that does the aforementioned linking and defines the Snooze interface. All of the procedures and macros described in the rest of this manual are provided by this macro or directly from @file{snooze.ss}.

So far, @file{db.ss} doesn't actually do anything useful: you have only told Snooze about the @italic{type} of DBMS you are using, not the specific database(s). The next sections describes how to set up a @italic{configuration} for creating connections a specific databases.

@section{Create a database configuration}

To connect to a database with Snooze, you first create a @italic{configuration} struct containing the relevant details of the database. Once you have a configuration, you can use it to create as many actual @italic{connections} as you want. This section describes creating the configuration; the next section covers connecting to the database using the configuration.

Edit @file{db.ss} and add replace the line:

@schemeblock[
  (code:comment "TODO: Insert definition of configuration structure")]

with the following code:

@itemize{
  @item{if you are using SQLite, write:

    @schemeblock[
      (define config
        (db:make-config "<<FILENAME>>"))]
            
    where @litchar{<<FILENAME>>} is the path to your database file;}
        
  @item{if you are using PostgreSQL:
  
    @schemeblock[
      (define config
        (db:make-config "<<SERVER>>"
                        <<PORT>>
                        "<<DATABASE>>"
                        "<<USERNAME>>"
                        "<<PASSWORD>>"))]
        
    where:
    
    @itemize{
      @item{@litchar{<<SERVER>>} is the hostname of your database server (e.g. @litchar{localhost});}
      @item{@litchar{<<PORT>>} is the port number to connect on (usually 5432; note that this is an integer - not a string);}
      @item{@litchar{<<DATABASE>>} the name of the relevant database on your server;}
      @item{@litchar{<<USERNAME>>} is the username you want to connect with;}
      @item{@litchar{<<PASSWORD>>} is the password for this username (this is an optional argument).}}}}
      
Note that the PostgreSQL @schemeid[make-config] also keyword arguments to configure SSL if you are connecting across a network.

@section{Start using Snooze}

Now you are ready to connect to a database and start storing data. Create a new module called @file{test.ss} in the same directory as @file{db.ss}. Edit this new module and enter the following test script:

@schememod[
  scheme
  
  (require (file "db.ss"))
  
  (code:comment "Define a datatype that we can save to the database:")
  (define-persistent-struct person
    [name   type:text]
    [age    type:integer]
    [gender type:symbol])
    
  (code:comment "Print the people that are saved in the database:")
  (define (print-people)
    (define people (find-all (q:select #:from entity:person)))
    (printf "People in the database:~n")
    (if (null? people)
        (printf "    NONE~n")
        (for-each (lambda (person)
                    (printf "    ~s~n" person))
                  people)))
    
  (code:comment "Connect to the database:")
  (call-with-database config
    (lambda ()

      (code:comment "Create a database table to house person data:")      
      (create-table person)
      
      (code:comment "Create some people (in memory):")
      (define-values (alice bob charlie)
        (values (make-person "Alice"   20 'F)
                (make-person "Bob"     25 'M)
                (make-person "Charlie" 30 'F)))
    
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
      (drop-table person)
    
      ))]
      
Compile and run the code with the following command lines:

@commandline{mzc -k test.ss}
@commandline{mzscheme -mtv test.ss}

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

We can see several bits of code Snooze functionality in this example.
Click on the names below to see the definitions of the relevant procedures:

@itemize{
  @item{@scheme[define-persistent-struct] is a macro that defines a
    structure type that can be saved to the database: the type specifiers
    give Snooze hints about how to serialize and deserialize field values;}
  @item{@scheme[call-with-database] sets up a default connection that is
    used by many Snooze API calls: the connection is established and
    terminated as control enters and leaves the argument thunk via
    continuation jumps or the natural flow of the program;}
  @item{@scheme[find-all] and @scheme[q:select] are part of the Snooze
    query mechanism (which has most of the expressiveness of full SQL);}
  @item{@scheme[create-table] and @scheme[drop-table] are used to issue
    the corresponding @litchar{CREATE TABLE} and @litchar{DROP TBALE} SQL
    to the database;}
  @item{@scheme[save!] is used to save a persistent struct to the database,
    and @scheme[delete!] is used to delete the corresponding database
    record.}}

That completes this quick start guide. In @file{db.ss} you now have 
everything you need to start defining your own persistent struct types
and saving structs to your database. The rest of this manual is a 
reference for the various libraries provided via this file.