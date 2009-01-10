#lang scribble/doc

@begin[
  (require (lib "manual.ss" "scribble"))
  (require-for-label (file "../snooze.ss"))]

@title{Introduction}

Snooze is an Object Relational Mapping (ORM) library similar to ActiveRecord or Hibernate. It provides a mapping from structures in PLT Scheme to rows in a relational database.

Snooze currently supports SQLite 3.x and PostgreSQL 8.x database back-ends.

@section{Quick start}

To get up and running with Snooze:

@itemize{

  @item{Get hold of SQLite 3.x or PostgreSQL 8.x and set up a database that Snooze can access:
  
    @itemize{
      @item{if you are using SQLite, create a database file somewhere on your local filesystem;}
      @item{if you are using PostgreSQL, make sure you have created a user with the necessary privileges and set up your @file{pg_hba.conf} appropriately.}}
      
      }

  @item{Create a module containing the Snooze bindings for your DBMS:
  
    @itemize{
      @item{In your Scheme project, create a file called @file{db.ss}.}
      @item{Fill this file with the following template code:
  	
        @schememod[mzscheme
        
            (require (planet "snooze.ss" ("untyped" "snooze.plt" 1))
                     (prefix db: (planet "<<DB>>.ss" ("untyped" "snooze.plt" 1) "<<DB>>")))
          
            (define-snooze-interface db:db@)
            
            (code:comment "TODO: Insert definition of configuration structure") 
            
            (code:comment "TODO: Insert call-with-my-database procedure")
            
            (provide (all-from (planet "snooze.ss" ("untyped" "snooze.plt" 1)))
                     (all-defined))]
            
            where @litchar{<<DB>>} is one of @litchar{postgresql8} or @litchar{sqlite3}, depending on your choice of database back-end.

            @file{snooze.ss} provides definitions for all the DBMS-independent parts of Snooze. @file{<<DB>>.ss} provides a DBMS-dependent unit that can be linked to the DBMS-independent code to create a complete Snooze implementation. The @scheme[define-snooze-interface] form is an unhygenic macro that does the linking and defines the DBMS-dependent part of the Snooze interface.}}
            
            }
    
  @item{Create a configuration structure for your database. Edit @file{db.ss} and add the following code after the @scheme[define-snooze-interface] line:
    
    @itemize{
    
      @item{if you are using SQLite:
      
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
          @item{@litchar{<<PASSWORD>>} is the password for this username (this is an optional argument).}}
          
        there are also keyword arguments indicating whether you want to use SSL and which version you want to use.}}}
      
    @item{Create a convenient procedure for connecting to your database. Edit @file{db.ss} and add the following code after the definition for @scheme[config]:
    
      @schemeblock[
        ;; call-with-my-database : (-> a) -> a
        (define (call-with-my-database thunk)
          (call-withdatabase config thunk))]}

  @item{Download and install Snooze from planet by compiling @file{db.ss} with the command:
  
    @commandline{mzc -k db.ss}}}

If the last step works without errors, then congratulations! You are now ready to use Snooze. Just wrap a call to @scheme[call-with-my-database] around any code that uses your database and all your calls to @scheme[save!], @scheme[delete!], @scheme[find-all], @scheme[find-one] and @scheme[g:find] will use the connection automatically.

@section{Front- and back-end code}

TODO

@defform[
  (define-snooze-interface backend-unit)]
