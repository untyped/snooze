#lang scribble/doc

@(require "base.ss")

@title[#:tag "modules"]{Snooze modules}

The majority of Snooze is provided in two parts: a @italic{DBMS independent part} provided by @filepath{snooze.ss} and a @italic{DBMS specific part} provided by a separate module such as @filepath{sqlite3/sqlite3.ss}.

@section{DBMS independent code}

Require the following module for the DBMS independent part of Snooze:

@defmodule[(planet untyped/snooze:2)]{
Compatible with the @scheme[scheme] and @scheme[scheme/base] languages.}

We recommend you require and re-provide this interface from a single module in your application. For example:

@schememod[
  scheme/base
  (require (planet untyped/snooze:2))
  (provide (all-from-out (planet untyped/snooze:2)))]

@section{DBMS specific code}

Snooze currently supports the DBMS back-ends listed below, although support for more back-ends is forthcoming. Choose one of the following modules depending on which back-end you are using:

@subsection{SQLite}

@defmodule[(planet untyped/snooze:2/sqlite3/sqlite3)]{
SQLite 3.x specific definitions. This module uses Jay McCarthy's @link[url:sqlite.plt]{sqlite.plt} package to create database connections.

You must make sure your SQLite installation is compiled with thread safety enabled if you wish to access a single database from multiple concurrent Scheme threads.}

@subsection{PostgreSQL}

@defmodule[(planet untyped/snooze:2/postgresql8/postgresql8)]{
PostgreSQL 8.x specific definitions. This module uses Ryan Culpepper's @link[url:spgsql.plt]{spgsql.plt} package to create database connections.

You must make sure your PostgreSQL installation has a @filepath{pg_hba.conf} file that allows TCP/IP connections from appropriate client machines (@tt{localhost} will normally suffice).}

@section{Audit trail code}

@bold{This feature should be considered experimental: it may be removed or changed in the future.}

Snooze has an audit trail feature that can be used to log and roll back changes to a database. Require it form the following module:

@defmodule[(planet untyped/snooze:2/audit/audit)]{
Compatible with the @tt{scheme} and @tt{scheme/base} languages. See the module source code for documentation.}
