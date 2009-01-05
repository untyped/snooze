; Snooze v3 example
; =================
; By  Dave Gurnell (dave at untyped dot com)
; 
; This self-contained example shows off some of the core parts of Snooze v3:
;
;   - defining the Snooze interface
;   - defining a persistent structure type
;   - saving structures to the database
;   - querying the database for structures
;   - deleting structures from the database
;
; other features of Snooze not demonstrated here include:
;
;   - fancy queries:
;     - aggregate functions
;     - inner, outer, left and right joins
;     - sorting and grouping
;     - limiting and offsetting
;   - generator combinators
;     - "projecting" results for simpler iteration
;   - connecting to multiple databases
;   - the Unlib "check" library (for data validation and error reporting)
;
; Lots of things below are simplified and are not the "whole truth".
; Please feel free to contact the author if you have questions or for more 
; information.
;
; Full documentation for Snooze is forthcoming.
(module example mzscheme
  
  (require (lib "snooze.ss" "snooze")
           (prefix postgresql8: (lib "postgresql8.ss" "snooze" "postgresql8")))
  
  ; Snooze interface -----------------------------
  
  ; The core Snooze interface is divided into two parts:
  ; 
  ;   - a DBMS-independent part, required directly from "snooze.ss"
  ;
  ;   - a DBMS-dependent part, defined in the file "snooze-unit.ss", 
  ;     which has to be linked against a particular database backend
  ;     with the define-snooze-interface macro below
  ;
  ; Snooze currently supports two DBMS backends: 
  ;   - SQLite 3 (via Jay McCarthy's sqlite.plt package)
  ;
  ;     Make sure you compile SQLite with thread safety enabled if you want to use this
  ;     backend from multiple Scheme threads.
  ; 
  ;   - PostgreSQL 8 (via Ryan Culpepper's spgsql.plt package)
  (define-snooze-interface postgresql8:db@)
  
  ;; main : -> void
  ;;
  ;; This procedure represents the main entry point for the application.
  ;; 
  ;; It sets up a database connection, which is stored in a parameter and
  ;; used for the rest of the program.
  ;; 
  ;; The rest of the program creates some students, lists them, deletes a
  ;; few and lists the remaining ones.
  (define (main)
    ; Enter your DB connection arguments here:
    (with-database (postgresql8:make-config "hostname" 5432 "database" "username" "password" #:ssl 'no)
      (create-students!)
      (print-students)
      (delete-students!)
      (print-students)))
  
  ; Data structures ------------------------------
  
  ; The next statement defines two things:
  ;   - a data structure, "student", that can be saved and loaded to/from the database
  ;   - a metadata structure, "entity:student", that is used to refer to the data type in queries
  
  ; The database table has the following structure (this can be generated automatically 
  ; using the create-table procedure in snooze-unit.ss, but see below for further advice):
  ;
  ;     CREATE TABLE student (
  ;         id INTEGER PRIMARY KEY,
  ;         revision INTEGER DEFAULT 0,
  ;         "family-name" TEXT,
  ;         "given-names" TEXT,
  ;         "year" INTEGER);
  ;
  ; Snooze uses an "id" column as a primary key in every table. This cannot be changed.
  ; IDs are generated automatically as new structures are saved to the database.
  ;
  ; Similarly, Snooze uses the "revision" column to guard against some concurrent update
  ; problems. It is initialised automatically and checked and incremented on every save.
  ; You should never have to worry about it.
  ;
  ; While Snooze will generate the database table for you automatically, we recommend
  ; you tweak the SQL yourself. TEXT fields, for example, use a lot of storage space
  ; and query time, and can't be ORDERed on in SELECT queries. Any textual column
  ; type will do for a type:text attribute, including CHARACTERs and VARCHARs of
  ; any length. You can also use other tools like default values and constraints.
  ;
  ; Field types include:
  ;
  ;     type:integer - signed integers (NULL values represented using #f)
  ;     type:text - string values (NULL values represented using #f)
  ;     type:symbol - symbols stored as strings in the DB (NULL values represented using #f)
  ;     type:boolean - boolean values (NULL cannot be represented at the Scheme level)
  ;
  ; Snooze does not care about the order of columns in the database: it references everything
  ; by name. However, table and column names *must* be the same as the relevant Scheme
  ; identitifers: there is no name mapping at present. This can mean some awkwardness at the
  ; SQL level, although Snooze provides a Scheme-based abstraction of SQL so this isn't as
  ; cumbersome as it initially sounds.
  
  ;; student : (struct (U string #f)
  ;;                   (U string #f)
  ;;                   (U string #f)
  ;;                   (U integer #f))
  ;;
  ;; entity:student : entity
  ;;
  ;; student?            : any -> boolean
  ;; student-id          : student -> (U integer #f)
  ;; student-revision    : student -> (U integer #f)
  ;; student-family-name : student -> (U string #f)
  ;; ...
  ;; set-student-family-name! : student string -> void
  ;; ...
  (define-persistent-struct student 
    ([family-name        type:text]
     [given-names        type:text]
     [programme          type:text]
     [year               type:integer]))
  
  ; Procedures -----------------------------------
  
  ; The Snooze query language is introduced below. This is a simple abstraction of SQL that
  ; provides constructs for manipulating data at the struct/entity level.
  ;
  ; Snooze exports all of its query language with a "q:" prefix to avoid name clashes with
  ; R5RS Scheme and certain SRFIs. There is currently no way to customise this prefix, although
  ; you can always copy and modify the code in snooze.ss.
  
  ;; find-students : -> (list-of student)
  (define (find-students)
    ; q:student : q:entity
    ;
    ; Think of this as an alias for a particular student in the query below.
    ; You need this level of indirection to do queries like:
    ;
    ;    (q:select #:what (list q:student1 q:student2) ...)
    ;
    ; that simultaneously alias a table under two different names.
    (define q:student
      (q:entity entity:student))
    ; q:select creates a SELECT data structure that can be used and re-used
    ; to interrogate the database:
    ;
    ;     q:select : #:what (list-of (U q:attr q:aggregate q:entity))
    ;                #:from (U q:entity q:join)
    ;                [#:where q:expr]
    ;                [#:order (list-of q:order)]
    ;                [#:group (list-of (U q:attr q:entity))]
    ;                [#:limit integer]
    ;                [#:offset integer]
    ;             -> select
    ; 
    ; find-gen is the basic query mechanism. It takes a select structure and returns a
    ; "generator procedure", that returns a row of data at a time until it reaches
    ; the end of the table, when it returns the unique value g:end:
    ;
    ;     find-gen : select -> (-> (U (list-of any) g:end))
    ; 
    ; We use the gen-> type to conveniently refer to generators in contracts:
    ;
    ;     (gen-> a) == (-> (U a g:end))
    ;
    ;     find-gen : select -> (gen-> (list-of (U persistent-struct any)))
    ; 
    ; Generators are a general mechanism provided as part of Unlib. Generators can be
    ; manipulated with combinators that perform analogous functions to the list
    ; combinators in SRFI1. See generator.ss and gen.ss in Unlib for more detail.
    ; 
    ; The items returned by a result generator are packaged up according to the 
    ; contents of the #:what clause of the select statement. For example:
    ;
    ;     (find-gen (q:select #:what q:student...))
    ;
    ; returns a generator that generates single students before reaching g:end:
    ;
    ;     (gen-> (U student #f))
    ; 
    ; Other examples:
    ;
    ;     (find-gen (q:select #:what (list (q:field q:student 'family-name)
    ;                                      (q:field q:student 'year)) ...))
    ;
    ;       ==> (gen-> (list (U string #f) (U integer #f)) 
    ;
    ;     (find-gen (q:select #:what (list q:student1 q:student2) ...))
    ;
    ;       ==> (gen-> (list (U student #f) (U student #f)))
    ;
    ; The #:what argument is usually a (list-of (U q:entity q:attr q:aggregate)), but
    ; it can also be a single (U q:entity q:attr q:aggregate). Snooze works out whether
    ; it should generate lists or single results appropriately.
    ;
    ; Two generator combinators are of particular use: g:map creates a generator B whose return
    ; values are related to a source generator A by some function f:A -> b:
    ;
    ;     g:map : (a -> b) (gen-> a) -> (gen-> b)
    ;
    ; g:collect takes a generator and calls it repeatedly, collecting its non-g:end results
    ; into a list:
    ;
    ;     g:collect : (gen-> a) -> (list-of a)
    (g:collect (g:find (q:select #:what q:student
                                 #:from q:student))))
  
  ;; g:students/programme : string -> (list-of student)
  ;;
  ;; This procedure returns a generator rather than a list. This lets us iterate over
  ;; a large amount of data without storing it all in memory (although caching mechanisms
  ;; in Snooze do consume some memory to increase speed if a row is generated more than once).
  (define (g:students/programme programme)
    ; q:student : q:entity
    (define q:student
      (q:entity entity:student))
    ; Return (gen-> student)
    (g:find (q:select #:what  q:student
                      #:from  q:student
                      #:where (q:= (q:attr q:student 'programme) programme))))
  
  ;; create-students! : -> void
  ;;
  ;; Newly created structures have an id and revision of #f
  ;; until they have been saves to the database.
  ;;
  ;; The save! procedure saves a structure and mutates its
  ;; id and revision fields appropriately.
  (define (create-students!)
    (save! (make-student "Gurnell" "Dave" "Programming" 1))
    (save! (make-student "Welsh"   "Noel" "Programming" 2))
    (save! (make-student "Jadud"   "Matt" "Programming" 3))
    (save! (make-student "Bloggs"  "Fred" "Mathematics" 1))
    (save! (make-student "Bloggs"  "Sue"  "Mathematics" 2))
    (save! (make-student "Doe"     "John" "Mathematics" 3)))
  
  ;; delete-students! : -> void
  ;;
  ;; The delete! procedure deletes a structure from the database
  ;; and sets its id and revision back to #f.
  ;;
  ;; Here we are using the g:for-each combinator to apply the
  ;; delete! function to each result generated by g:students/programme.
  (define (delete-students!)
    (g:for-each delete! (g:students/programme "Mathematics")))
  
  ;; print-students : -> void
  ;;
  ;; This procedure uses conventional for-each to iterate through the results
  ;; from find-students. We could use a generator and g:map or g:for-each to
  ;; do a similar sort of thing.
  (define (print-students)
    (printf "Printing students:~n")
    (for-each (lambda (student)
                (printf "   ~a~n" student))
              (find-students)))

  )