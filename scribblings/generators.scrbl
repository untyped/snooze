#lang scribble/doc

@(require (file "base.ss"))
          
@title[#:tag "generators"]{Working with generators}

@italic{Generators} are a lightweight iteration mechanism similar to
the @italic{ports} of R5RS Scheme and the @italic{streams} of SRFI 40.
They form a convenient, lightweight way of iterating through and 
modifying large datasets such as those returned by Snooze's 
@scheme[g:find] procedure.

Generators are defined in the Unlib package on PLaneT, and are reprovided by
@filepath{snooze.ss}. See the documentation in Unlib for more information: @other-manual['(planet untyped/unlib:3/gen)].
