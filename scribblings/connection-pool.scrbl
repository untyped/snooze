#lang scribble/doc

@(require "base.ss")

@title{Connection Pooling}

Snooze implements a connection pool as a mixin to to the @scheme[snooze] object. This section describes how the connection pool operates and may be customised. For convenience the back-end constructors take additional arguments to specify if the back-end should use a connection pool with default parameters. See the back-end specific documentation for how to do this.

Most databases allow only a limited number of open connections, as each connection consumes resources. Furthermore, opening a new connection can take significant time. For these reasons it is useful to keep around a pool of open connections that are shared between all database clients. The disadvantage of connection pooling is that the pool may run out of connections, forcing clients to wait for a free connection even though the database might be capable of handling increased load. The Snooze connection pool attempts to dynamically balance these competing forces, allocating and release connections between a maximum and minimum number to respond to load.

The numbers of connections in the pool is bounded by @scheme[min-connections] and @scheme[max-connections] and controlled by @scheme[connection-time-limit].

Three variables are used to make the decision to allocate or release connections:

@itemlist{
  @item{The number of free connections}        
  @item{The rate of change of free connections}
  @item{The acceleration in the number of free connections}
}
