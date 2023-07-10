# Intermediate report

I started working on the project 6. Dec. 
Since then I have worked on Throttle, Cancellation, Channel, and Actor.
I did not yet start working on the planned job stealing pool.

The code is pushed in <https://github.com/ruby-concurrency/concurrent-ruby/pull/791> 
and the generated documentation for this branch can be found at 
<http://blog.pitr.ch/concurrent-ruby/master/index.html>.

## Throttle

The Throttle implementation originally had special APIs 
to interact with other abstractions like promises. 
However it was impractical and the API felt cumbersome.
Therefore the Throttle was finalized with much smaller API surface.
Capacity can be still directly acquired from the Throttle 
and then released.

The more common usage of the Throttle is with a proxy executor 
`a_throttle.on(Concurrent.global_io_executor)`. 
Anything executed on the proxy executor will be throttled and 
execute on the given executor. There can be more than one proxy executors.
All abstractions which execute tasks have option to specify executor, 
therefore the proxy executor can be injected to any abstraction 
throttling its concurrency level.

The abstraction is ready for release. 
For more details see the documentation 
<http://blog.pitr.ch/concurrent-ruby/master/Concurrent/Throttle.html>.

## Cancellation

The Cancellation abstraction provides cooperative cancellation.

The Cancellation abstraction was originally consisting of 2 classes,
during its finalization it was however simplified 
to be just a combination of Cancellation object 
and an origin which is regular Event or Future,
which improves compose-ability greatly. 
Any Event or Future can be easily turned into cancellation.    

The standard methods `Thread#raise` of `Thread#kill` available in Ruby
are very dangerous (see linked the blog posts bellow).
Therefore concurrent-ruby provides an alternative.
* <https://jvns.ca/blog/2015/11/27/why-rubys-timeout-is-dangerous-and-thread-dot-raise-is-terrifying/>
* <http://www.mikeperham.com/2015/05/08/timeout-rubys-most-dangerous-api/>
* <http://blog.headius.com/2008/02/rubys-threadraise-threadkill-timeoutrb.html>

It provides an object which represents a cancellation event 
which can be shared between tasks.
The task has to get the reference to the object 
and periodically cooperatively check that it is not cancelled.

The abstraction is ready for release. 
For more details see the documentation 
<http://blog.pitr.ch/concurrent-ruby/master/Concurrent/Cancellation.html>.

## Channel

The channel implementation is inspired by Go. 
However this implementation is more flexible. 
It has 3 major operations pop, push and select as expected.
Where each operation has 3 variants. 
`try_(pop|push|select)` which never blocks and returns always immediately.
`(pop|push|select)` which blocks current thread until it can be done 
or until it times out.
`(pop|push|select)_op` which returns Future representing the operation,
which can be easily composed with other asynchronous tasks.

The abstraction is ready for release. 
For more details see the documentation
<http://blog.pitr.ch/concurrent-ruby/master/Concurrent/Promises/Channel.html>. 

## Actor

I've refreshed my knowledge about Erlang actors 
and started working on the implementation, 
which will match the Erlangs behaviour.
(The goal is to make possible to port OTP later, not part of this project.)  
Originally, I have planned to only implement the process using
Simulated process implemented by `Future#run`. 
However that makes the body of the actors most complex, 
therefore I started to considering to implement 3 modes
to give more freedom to the users.
- Backing each actor by a its thread. 
  Offers simpler body of the actor and can be used 
  if the number of these actors is limited and they are long-running.
- Stack-less. Each message calls a method on the actor. 
  Can run on a thread pool, 
  therefore the number of the actors is not limited. 
  They can be short-lived as well. 
  However since they are stack-less they are not suitable 
  for complicated actor bodies which often change behavior.
- Simulated process. No limitations but it is harder to write 
  the bodies of the actors for users.      

I will see what modes remaining time allows me to implement.  

