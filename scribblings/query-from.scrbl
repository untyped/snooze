#lang scribble/doc

@(require "base.ss")

@title[#:tag "from"]{Data sources, result types and the @scheme[#:from] clause}

The @scheme[#:from] clause specifies the @italic{source} of a query's data.
A source can be one of the following:

@itemize{
  @item{an entity or entity alias;}
  @item{an alias of another query;}
  @item{a join over other sources;}}

The from clause also determines the default @italic{result type} of the query.
The three @italic{find} procedures combine results in different ways:

@itemize{
  @item{@scheme[find-one] returns @scheme[(U result #f)];}
  @item{@scheme[find-all] returns @scheme[(listof result)];}
  @item{@scheme[g:find] returns @scheme[(gen-> result)].}}

So, for example, if a query @scheme[q] has a result type of @scheme[person],
@scheme[(find-all q)] will have a return a @scheme[(listof person)].

@intmore["what"] The default result type of a query can be overridden using a 
@scheme[#:what] clause.

@section{Entities}

The simplest @scheme[#:from] clause is a single entity. For example:

@schemeblock[
  (code:comment "(listof person)")
  (code:comment "Find all people in the database:")
  (find-all (sql (select #:from person)))]

Single-entity queries have results consisting of a single persistent struct.
For example, the query above has a result type of @scheme[person].

@section{Joins}

@extmore["http://en.wikipedia.org/wiki/Join_(SQL)"]{Joins on Wikipedia}
Joins let you combine data from several entities into a single query.
Snooze supports four types of join. The semantics of different join types are 
beyond the scope of these document. See @link["http://en.wikipedia.org/wiki/Join_(SQL)"]{Wikipedia's}
page on joins for more information.

@defform[(inner source1 source2 on)]{
Creates an @italic{inner join} or @scheme{equi-join} between two sources, 
@scheme[source1] and @scheme[source2]. The @schemeid[on] argument is a 
Boolean-valued expression specifying the join criteria.

For example, given appropriate persistent struct definitions:

@schemeblock[
  (code:comment "(listof (list person pet))")
  (code:comment "Find all people who have pets plus their pets:")
  (find-all
   (sql (select #:from (inner person
                              pet
                              (= person.pet-id pet.id)))))]

is equivalent to:

@verbatim{
  SELECT person.*, pet.*
  FROM person INNER JOIN pet ON person.petID = pet.id;}}

@defform[(left source1 source2 on)]{
Creates a @italic{left join} between two sources, @scheme[source1] and 
@scheme[source2]. The @schemeid[on] argument is a Boolean-valued expression 
specifying the join criteria.

For example, given appropriate persistent struct definitions:

@schemeblock[
  (code:comment "(listof (list person (U pet #f)))")
  (code:comment "Find all people, plus their pets wherever applicable:")
  (find-all
   (sql (select #:from (left person
                             pet
                             (= person.pet-id pet.id)))))]

is equivalent to:

@verbatim{
  SELECT person.*, pet.*
  FROM person LEFT JOIN pet ON person.petID = pet.id;}}

@defform[(right source1 source2 on)]{
Creates a @italic{right join} between two sources, @scheme[source1] and 
@scheme[source2]. The @schemeid[on] argument is a Boolean-valued expression 
specifying the join criteria.

For example, given appropriate persistent struct definitions:

@schemeblock[
  (code:comment "(listof (list (U pet #f) person))")
  (code:comment "Find all people, plus their pets wherever applicable:")
  (find-all
   (sql (select #:from (right pet
                              person
                              (= person.pet-id pet.id)))))]

is equivalent to:

@verbatim{
  SELECT pet.*, person.* 
  FROM pet RIGHT JOIN person ON person.petID = pet.id;}}

@defform[(outer source1 source2)]{
Creates a @italic{cross join} between two sources, @scheme[source1] and 
@scheme[source2]. The @schemeid[on] argument is a Boolean-valued expression 
specifying the join criteria.

For example:

@schemeblock[
  (code:comment "(listof (list (U pet #f) person))")
  (code:comment "Find all people and all pets:")
  (find-all (sql (select #:from (outer person pet))))]
  
is equivalent to:

@verbatim{
  SELECT person.*, pet.*
  FROM person, pet;}}
  
The result type of a join is a @scheme[flatten]@schemeidfont{ed} list of the 
result types of the join arguments. For example:

@schemeblock[(sql (select #:from (outer employee employer)))]

has a result type of @scheme[(list employee employer)], while:

@schemeblock[(sql (select #:from (outer (outer employee employer) role)))]

has a result type of @scheme[(list employee employer role)].

@section{Subqueries}

One query can use another query as a data source. This is useful when joining
sources together, when some query operations should be applied to only one of
the sources in the join.

For example, the following query uses the @scheme[#:limit] clause to select the 
first 10 employers in the database, and joins the employers to their employees.
The @scheme[#:limit] cannot be specified in the outer query because there may be
more than one employee per employer:

@schemeblock[
(find-all
 (sql (select
       #:from (left (select #:from  employer
                            #:limit 10
                            #:order ((asc employer.name)))
                    employee
                    (= employer.id
                       employee.employer-id)))))]

Ignoring DBMS-specific rules regarding aliases, this query is roughly equivalent
to the following SQL:

@verbatim{
SELECT employer.*, employee.*
FROM (SELECT *
      FROM employer
      ORDER BY name ASC
      LIMIT 10)
     LEFT JOIN employee
     ON employer.id = employee.employerID;}
