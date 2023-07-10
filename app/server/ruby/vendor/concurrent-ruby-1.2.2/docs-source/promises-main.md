Promises is a new framework unifying former tools `Concurrent::Future`,
`Concurrent::Promise`, `Concurrent::IVar`, `Concurrent::Event`,
`Concurrent.dataflow`, `Delay`, and `TimerTask` of concurrent-ruby. It
extensively uses the new synchronization layer to make all the methods
*lock-free* (with the exception of obviously blocking operations like `#wait`,
`#value`, etc.). As a result it lowers danger of deadlocking and offers
better performance.

It provides similar tools as other promise libraries do, users coming from
other languages and other promise libraries will find the same tools here
(probably named differently though). The naming conventions were borrowed
heavily from JS promises.
  
This framework, however, is not just a re-implementation of other promise
library, it draws inspiration from many other promise libraries, adds new
ideas, and is integrated with other abstractions like actors and channels.

Therefore it is likely that user will find a suitable solution for a problem in
this framework. If the problem is simple user can pick one suitable
abstraction, e.g. just promises or actors. If the problem is complex user can
combine parts (promises, channels, actors) which were designed to work together
well to a solution. Rather than having to combine fragilely independent tools.

This framework allows its users to:

-   Process tasks asynchronously
-   Chain, branch, and zip the asynchronous tasks together
    -   Therefore, to create directed acyclic graph (hereafter DAG) of tasks
-   Create delayed tasks (or delayed DAG of tasks)
-   Create scheduled tasks (or delayed DAG of tasks)
-   Deal with errors through rejections
-   Reduce danger of deadlocking
-   Control the concurrency level of tasks
-   Simulate thread-like processing without occupying threads
    -   It allows to create tens of thousands simulations on one thread 
        pool
    -   It works well on all Ruby implementations
-   Use actors to maintain isolated states and to seamlessly combine 
    it with promises
-   Build parallel processing stream system with back 
    pressure (parts, which are not keeping up, signal to the other parts of the 
    system to slow down).

**The {file:doc/promises.out.md guide} is best place to start with promises.**

# Main classes

The main public user-facing classes are {Concurrent::Promises::Event} and
{Concurrent::Promises::Future} which share common ancestor
{Concurrent::Promises::AbstractEventFuture}.

**{Concurrent::Promises::AbstractEventFuture}:** 
> {include:Concurrent::Promises::AbstractEventFuture}

**{Concurrent::Promises::Event}:** 
> {include:Concurrent::Promises::Event}

**{Concurrent::Promises::Future}:** 
> {include:Concurrent::Promises::Future}

