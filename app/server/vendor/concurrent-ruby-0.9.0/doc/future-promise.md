# Futures and Promises

New implementation added in version 0.8 differs from previous versions and has little in common. 
{Future} represents a value which will become {#completed?} in future, it'll contain {#value} if {#success?} or a {#reason} if {#failed?}. It cannot be directly completed, there are implementations of abstract {Promise} class for that, so {Promise}'s only purpose is to complete a given {Future} object. They are always constructed as a Pair even in chaining methods like {#then}, {#rescue}, {#then_delay}, etc. 

There is few {Promise} implementations:

-   OuterPromise - only Promise used by users, can be completed by outer code. Constructed with {Concurrent::Next.promise} helper method.
-   Immediate - internal implementation of Promise used to represent immediate evaluation of a block. Constructed with {Concurrent::Next.future} helper method.
-   Delay - internal implementation of Promise used to represent delayed evaluation of a block. Constructed with {Concurrent::Next.delay} helper method.
-   ConnectedPromise - used internally to support {Future#with_default_executor}

