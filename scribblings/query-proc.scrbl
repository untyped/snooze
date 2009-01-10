#lang scribble/doc

@(require "base.ss")
        
@title[#:tag "query-procedures"]{Query procedures}

@(declare-exporting (planet untyped/snooze/snooze))

@scheme[snooze<%>] supplies three @italic{query methods} that retrieve data in
different ways:

@itemize{
  @item{@schemeidfont{find-all} retrieves a list of all matching @italic{results} from the database.}
  @item{@schemeidfont{find-one} is like @schemeidfont{find-all} but returns the first result found. If no results are found, returns @scheme[#f] instead.}
  @item{@intmore{generators} @schemeidfont{g:find} is like @schemeidfont{find-all} but returns a @italic{generator} of the results found.}}

All of these procedures take a @scheme[select] statement as an argument. These statements are created using the Snooze query language, a combinator library and syntax layer that mirrors SQL in Scheme.
