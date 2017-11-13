FAQ
===

**But I still don't understand why I should care?**

As mentioned earlier, persistent data structures perform a
copy whenever they are modified meaning there is never any
chance that two threads could be modifying the same instance
at any one time. And, because they are very efficient copies,
you don't need to worry about using up gobs of memory in the
process.

Even if threading isn't a concern, because they're immutable,
you can pass them around between objects, methods, and
functions in the same thread and never worry about data
corruption; no more defensive calls to `Object#dup`!


**What's the downside--there's always a downside?**

There's a potential performance hit when compared with MRI's
built-in, native, hand-crafted C-code implementation of Hash.

For example:

``` ruby
hash = Hamster::Hash.empty
(1..10000).each { |i| hash = hash.put(i, i) }
  # => 0.05s
(1..10000).each { |i| hash.get(i) }
  # => 0.008s
```

vs.

``` ruby
hash = {}
(1..10000).each { |i| hash[i] = i }
  # => 0.004s
(1..10000).each { |i| hash[i] }
  # => 0.001s
```

The previous comparison wasn't really fair. Sure, if all you
want to do is replace your existing uses of `Hash` in single-
threaded environments then don't even bother. However, if you
need something that can be used efficiently in concurrent
environments where multiple threads are accessing--reading AND
writing--the contents things get much better.

A more realistic comparison might look like:

``` ruby
hash = Hamster::Hash.empty
(1..10000).each { |i| hash = hash.put(i, i) }
  # => 0.05s
(1..10000).each { |i| hash.get(i) }
  # => 0.008s
```

versus

``` ruby
hash = {}
(1..10000).each { |i| hash = hash.dup; hash[i] = i }
  # => 19.8s

(1..10000).each { |i| hash[i] }
  # => 0.001s
```

What's even better -- or worse depending on your perspective
-- is that after all that, the native `Hash` version still
isn't thread-safe and still requires some synchronization
around it slowing it down even further.

The Hamster version on the other hand was unchanged from the
original whilst remaining inherently thread-safe, and 3 orders
of magnitude faster.

**You still need synchronisation so why bother with the copying?**

Well, I could show you one but I'd have to re-write/wrap most
Hash methods to make them generic, or at the very least write
some application-specific code that synchronized using a `
Mutex` and ... well ... it's hard, I always make mistakes,
I always end up with weird edge cases and race conditions so,
I'll leave that as an exercise for you :)

And don't forget that even if threading isn't a concern for
you, the safety provided by immutability alone is worth it,
not to mention the lazy implementations.
