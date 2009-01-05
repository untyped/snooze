#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "guid"]{Globally unique identifiers (GUIDs)}

@italic{GUIDs} are structures that represent unique references to records stored in the database. They are used internally in Snooze code to refer to records that may or may not be present in the database. Application programmers wil probably not fine these useful.

@defproc[(struct-guid [struct persistent-struct?]) guid?]{
Returns a @scheme[guid] structure that uniquely references @scheme[struct] in the database. Guids are used inside Snooze, but may not be so useful to application programmers.}
