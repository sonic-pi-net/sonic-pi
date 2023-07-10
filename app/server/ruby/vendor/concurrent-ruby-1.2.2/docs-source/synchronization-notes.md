# Concurrent Ruby Notes

## Locks

Concurrent Ruby also has an internal extension of `Object` called
`LockableObject`, which provides same synchronization primitives as Java's
Object: `synchronize(&block)`, `wait(timeout = nil)`,
`wait_until(timeout = nil, &condition)`, `signal`, `broadcast`. This class is
intended for internal use in `concurrent-ruby` only and it does not support
subclassing (since it cannot protect its lock from its children, for more
details see [this article](http://wiki.apidesign.org/wiki/Java_Monitor)). It has
minimal interface to be able to use directly locking available on given
platforms.

For non-internal use there is `Lock` and `Condition` implementation in
`Synchronization` namespace, a condition can be obtained with `new_condition`
method on `Lock`. So far their implementation is naive and requires more work.
API is not expected to change.

## Method names conventions

Methods starting with `ns_` are marking methods that are not using
synchronization by themselves, they have to be used inside synchronize block.
They are usually used in pairs to separate the synchronization from behavior and
to allow to call methods in the same object without double locking.

``` ruby
class Node
  # ...
  def left
    synchronize { ns_left }
  end  

  def right
    synchronize { ns_right }
  end  

  def to_a
    # avoids double locking
    synchronize { [ns_left, ns_right] }
  end    

  private

  def ns_left
    @left
  end

  def ns_right
    @right
  end
  # ...
end
```
## Piggybacking

Any write executed before volatile write based on program-order is visible to
the volatile read as well, which allows
[piggybacking](http://stackoverflow.com/questions/8769570/volatile-piggyback-is-this-enough-for-visiblity).
Because it creates synchronizes-with (JMM term) order between volatile write
and read, which participates in creating happens-before order.

This trick is used in some of the abstractions, to avoid unnecessary
synchronization or volatile declarations.
