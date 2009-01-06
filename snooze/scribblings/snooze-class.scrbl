#lang scribble/doc

@(require "base.ss")

@(define snooze-eval (make-snooze-eval))
          
@title[#:tag "snooze-object"]{Connecting to, querying and updating databases}

@(declare-exporting (planet untyped/snooze))

Snooze applications interact with databases via @scheme[scheme/class] object that implement the @scheme[snooze<%>] interface. Each object establishes connections to a single database.

Snooze objects guarantee thread safety: multiple threads can connect concurrently to the same object without running into problems. This means that each object can actually maintain more than one database connection concurrently, although only one connection is ever visible in any given thread. This is illustrated by the following code sample:

@schememod[scheme/base
  
  (require (planet untyped/snooze:2/snooze)
           (planet untyped/snooze:2/sqlite3/sqlite3))
  
  (code:comment "snooze%")
  (code:comment "")           
  (code:comment "Interface to the SQLite database in \"mydata.db\":")
  (define mydata (make-snooze (make-database (build-path "mydata.db"))))
  
  (code:comment "(persistent-struct (U string #f))")
  (define-persistent-struct person
    ([name type:string]))
    
  (code:comment "-> void")
  (code:comment "")           
  (code:comment "Connects to the database and inserts a record:")
  (define (save-new-person)
    (send mydata call-with-connection
          (lambda ()
            (send mydata save! (make-person "Dave")))))
  
  (code:comment "These threads won't interfere with one another:")          
  (thread save-new-person)
  (thread save-new-person)
  (thread save-new-person)]
  
@section{Object oriented interface}

@defproc[(make-snooze [database database<%>]) snooze<%>]{Creates a Snooze object to talk to the specified @scheme[database]. Snooze objects implement the @scheme[snooze<%>] interface, which contains methods for connecting to, querying, updating and maintaining the @scheme[database].}

@definterface[snooze<%> ()]{

  @defmethod[(call-with-connection [thunk (-> any)]) any]{Establishes a connection to the database and maintains it for the dynamic extent of @scheme[thunk]. The connection is closed when control is transferred outside of @scheme[thunk] via a continuation jump, an exception or a graceful return. The connection is re-established when control passes into @scheme[thunk] via a continuation jump.
  
  Only one connection may be opened at a time in each thread. Child threads do not inherit their parents' connections. @scheme[exn:fail:snooze] is raised if a connection is already open when control is transferred into @scheme[thunk].}

  @defmethod[(current-connection) (U connection? #f)]{Returns the current connection, or @scheme[#f] if no connection is established.}
  
  The remaining methods require a connection to be open when they are called: @scheme[exn:fail:snooze] is raised in all cases if this is not the case.

  @defmethod[(find-all [query query?]) (listof result)]{@scheme[find-all] retrieves a list of all matching @italic{results} from the database. @schemeid[conn] is an optional connection: if omitted, the default connection from @scheme[call-with-database] is used.

  @intmore{select} @schemeid[query] is a @schemeid[select] statement, as returned by @scheme[sql:select]. It determines both the data retrieved and the type of each @schemeid[result].}

  @defmethod[(find-one [query query?]) (U result #f)]{Similar to @scheme[find-all], but only returns the first result found. If no results are found, returns @scheme[#f] instead.}

  @defmethod[(g:find [query query?]) (gen-> result)]{@intmore{generators} Similar to @scheme[find-all], but returns a @italic{generator} of results. This is the most general query mechanism offered by Snooze: generators allow you to manipulate results one at a time in a functional manner, without wasting lots of memory on intermediate lists.}

  @defmethod[(save! [struct persistent-struct?]) persistent-struct?]{Inserts or updates the database record for the supplied @scheme[struct]:
  
  If @scheme[struct] has an @scheme[id] of @scheme[#f], @scheme[save!] assumes that no corresponding database record exists. It sets the @scheme[revision] field to @scheme[0] and uses an SQL @scheme{INSERT} statement to insert a new database record. Finally, @scheme[save!] sets the @scheme[id] to the primary key of the record and returns the mutated @scheme[struct].

  If @scheme[struct] already has an integer @scheme[id] when @scheme[save!] is called, the behaviour is different. First, @scheme[save!] checks the database to make sure the stored revision number matches the revision number in @scheme[struct]. It then increments the @scheme[revision] and uses an SQL @scheme{UPDATE} statement to update the database record with the new information. Finally, @scheme[save!] returns the mutated @scheme[struct].

  @scheme[save!] raises @scheme[exn:fail:snooze:revision] if a revision number check fails. This normally indicates that @scheme[struct] has been concurrently loaded and saved by another thread.}
  
  @defmethod[(delete! [struct persistent-struct?]) persistent-struct?]{Deletes the database record for @scheme[struct] and sets its @scheme[id] and @scheme[revision] to @scheme[#f]. Returns the mutated @scheme[struct] to allow the programmer to chain the call with calls to other procedures.

  @scheme[delete!] raises @scheme[exn:fail:snooze] if no database record exists, and @scheme[exn:fail:snooze:revision] if the @scheme[revision] in @scheme[struct] does not match the revision number stored in the database.}
  
  @defmethod[(create-table [entity entity?]) void?]{Issues an SQL @scheme{CREATE TABLE} statement to create a database table for the supplied @scheme[entity]. Raises @scheme[exn:fail:snooze] if the table already exists or cannot be created.}

  @defmethod[(drop-table [entity entity?]) void?]{Issues an SQL @scheme{DROP TABLE} statement to delete the database table for the supplied @scheme[entity]. Does nothing if no table is present. Raises @scheme[exn:fail:snooze] if the table exists and cannot be dropped.}
  
  @defmethod[(table-names) (listof symbol?)]{Returns a list of the names of the tables defined in the database. If the database supports multiple namespaces, only tables in the standard or public namespace are returned.}
  
  @defmethod[(table-exists? [table (U entity? symbol?)]) boolean?]{Checks to see if a table exists for the supplied entity or table name. Note that if the argument is a symbol it refers to a @italic{table name} rather than an entity name.}
    
  } @;end snooze<%>

@section{Procedural interface}

The @scheme[define-snooze-interface] macro is provided for convenience, to convert the object oriented database interface above into a more Schemely procedural interface.

@defform*[((define-snooze-interface snooze-object)
           (define-snooze-interface prefix-id snooze-object))]{
           
Defines a procedure for each of the methods in @scheme[snooze<%>], such that each procedure calls the matching method in the @scheme[snooze-object]. The optional @scheme[prefix-id] can be used to add a prefix to each identifier to avoid naming collisions. For example:
           
@schemeblock[
  (define db1-snooze
    (make-snooze (sqlite:make-database (build-path "db1.sqlite"))))    
  
  (define db2-snooze
    (make-snooze (sqlite:make-database (build-path "db2.sqlite"))))    

  (define-snooze-interface db1-snooze)
  (define-snooze-interface alt: db2-snooze)
  
  (code:comment "Connect to \"db1.sqlite\" and perform some operations:")
  (call-with-connection
   (lambda ()
     (code:comment "... ")))
  
  (code:comment "Connect to \"db2.sqlite\" and perform some operations:")
  (alt:call-with-connection
   (lambda ()
     (code:comment "... ")))]}
     
@defform*[((snooze-interface-out)
           (snooze-interface-out prefix-id))]{Expands into a set of @scheme[provide] forms for the functional Snooze interface defined by @scheme[(define-snooze-interface)] or @scheme[(define-snooze-interface prefix-id)].}

@section[#:tag "save+delete"]{Saving and deleting structures}

When a persistent structure is first created, it has no corresponding record in the database. A record is saved (inserted or updated) with a call to the @scheme[save!] method or procedure, and deleted with a call to the @scheme[delete!] method or procedure.

@scheme[save!] updates the database record for @scheme[struct] and sets its @scheme[id] and @scheme[revision] appropriately. It returns the mutated @scheme[struct] to allow the programmer to chain the call with calls to other procedures.

If @scheme[struct] has an @scheme[id] of @scheme[#f], @scheme[save!] assumes that no corresponding database record exists. It sets the @scheme[revision] field to @scheme[0] and uses an SQL @tt{INSERT} statement to insert a new database record. Finally, @scheme[save!] sets the @scheme[id] to the primary key of the record and returns the mutated @scheme[struct].

If @scheme[struct] already has an integer @scheme[id] when @scheme[save!] is called, the behaviour is different. First, @scheme[save!] checks the database to make sure the stored revision number matches the revision number in @scheme[struct]. It then increments the @scheme[revision] and uses an SQL @tt{UPDATE} statement to update the database record with the new information. Finally, @scheme[save!] returns the mutated @scheme[struct].

@scheme[save!] raises @scheme[exn:fail:snooze:revision] if a revision number check fails. This is useful because it allows the programmer to detect and avoid concurrent updates.

Finally, @scheme[delete!] uses an SQL @tt{DELETE} statement to delete the corresponding record from the database. @scheme[delete!] sets the @scheme[id] and @scheme[revision] of the struct to @scheme[#f] and then returns it.

The lifecycle of the @scheme[id] and @scheme[revision] fields is summarised in the code snippet below:

@italic{Note: The repeated calls to @scheme[call-with-connection] are only necessary to get Scribble to print the results of each statement: normal application programs should be able to all of this interaction with a single connection.}

@italic{Extra note: The printed values in this example may not be correct. This is because of the way Scribble renders example blocks. A solution for this issue is being worked on.}

@interaction[
  #:eval snooze-eval
  
  (code:comment "Define a new persistent struct type:")
  (define-persistent-struct person
    ([name type:string]))
    
  (code:comment "Create a DB table for this new type:")
  (call-with-connection
   (lambda ()
     (create-table entity:person)))
     
  (code:comment "Create a struct: initially it has no corresponding DB record:") 
  (define person (make-person "Dave"))
  person
  
  (code:comment "Insert a DB record and set the struct's ID and revision:")
  (call-with-connection
   (lambda ()
     (save! person)))
    
  (code:comment "Update the record and increment the revision:")
  (set-person-name! person "Noel")
  (call-with-connection
   (lambda ()
     (save! person)))
    
  (code:comment "Deleted the record and set the ID and revision to #f:")
  (call-with-connection
   (lambda ()
     (delete! person)))
     
  (code:comment "Finally, delete the DB table to clean up:")
  (call-with-connection
   (lambda ()
     (drop-table entity:person)))]
