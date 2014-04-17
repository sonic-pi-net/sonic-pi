Hamster
-------

  - [![Version](https://badge.fury.io/rb/hamster.png)](https://rubygems.org/gems/hamster)
  - [![Climate](https://codeclimate.com/github/hamstergem/hamster.png)](https://codeclimate.com/github/hamstergem/hamster)
  - [![Build](http://img.shields.io/travis-ci/hamstergem/hamster.png)](https://travis-ci.org/hamstergem/hamster)
  - [![Dependencies](https://gemnasium.com/hamstergem/hamster.png)](https://gemnasium.com/hamstergem/hamster)
  - [![Coverage](http://img.shields.io/coveralls/hamstergem/hamster.png)](https://coveralls.io/r/hamstergem/hamster)
  - [![License](http://img.shields.io/license/MIT.png?color=green)](http://opensource.org/licenses/MIT)

Efficient, immutable, and thread-safe collection classes for Ruby.

Hamster started out as an implementation of [Hash Array Mapped Tries][HAMT] for
Ruby. It has since expanded to implementations of other [Persistent Data
Structures][PDS] like `Set`, `List`, `Stack`, `Queue`, and `Vector`.

Hamster collections are **immutable**. Whenever you modify a Hamster
collection, the original is preserved and a modified copy is returned. This
makes them inherently thread-safe and sharable. For an interesting
perspective on why immutability itself is inherently a good thing, you might
like to take a look at Matthias Felleisen"s [Function Objects presentation][FO].

Hamster collection classes remain space efficient by making use of some very
well understood and very simple techniques that enable sharing between copies.

Hamster collections are almost always closed under a given operation. That is,
whereas Ruby"s collection methods always return arrays, Hamster collections
will return an instance of the same class wherever possible.

And lastly, Hamster lists are lazy -- where Ruby"s language constructs permit
-- making it possible to, among other things, process "infinitely large" lists.
(Note: Ruby 1.9 supports a form of laziness using Enumerator. However, they"re
implemented using Fibers which unfortunately can"t be shared across threads.)

Hamster started out as a spike to prove a point and has since
morphed into something I actually use. My primary concern has
been to round out the functionality with good test coverage
and clean, readable code.

Performance is pretty good -- especially with lazy lists --
but there are some things which may blow the stack due to a
lack of Tail-Call-Optimisation in Ruby.

Documentation is sparse but I've tried as best I can to write
specs that read as documentation. I've also tried to alias
methods as their `Enumerable` equivalents where
possible to ease code migration.

[HAMT]: http://lampwww.epfl.ch/papers/idealhashtrees.pdf
[PDS]: http://en.wikipedia.org/wiki/Persistent_data_structure
[FO]: http://www.ccs.neu.edu/home/matthias/Presentations/ecoop2004.pdf


Contact
=======

  - GitHub:   http://github.com/harukizaemon/hamster
  - RubyGems: https://rubygems.org/gems/hamster


Using
=====

Most Hamster classes support an API that resembles their
standard library counterpart with the caveat that any
modification returns a new instance.

Once installed, all that remains is to make the
collection classes available in your code:

``` ruby
require "hamster"
```

Or if you prefer to only pull in certain collection types:

``` ruby
require "hamster/list"
require "hamster/stack"
require "hamster/queue"
require "hamster/hash"
require "hamster/set"
require "hamster/vector"
```

**Hash**

Constructing a Hamster `Hash` is almost as simple as a
regular one:

``` ruby
person = Hamster.hash(:name => "Simon", :gender => :male)
  # => {:name => "Simon", :gender => :male}
```

Accessing the contents will be familiar to you:

``` ruby
person[:name]
  # => "Simon"
person.get(:gender)
  # => :male
```

Updating the contents is a little different than you are
used to:

``` ruby
friend = person.put(:name, "James")
  # => {:name => "James", :gender => :male}
person
  # => {:name => "Simon", :gender => :male}
friend[:name]
  # => "James"
person[:name]
  # => "Simon"
```

As you can see, updating the hash returned a copy leaving
the original intact. Similarly, deleting a key returns
yet another copy:

``` ruby
male = person.delete(:name)
  # => {:gender => :male}
person
  # => {:name => "Simon", :gender => :male}
male.key?(:name)
  # => false
person.key?(:name)
  # => true
```

Hamster's `Hash` doesn't provide an assignment (`Hash#[]=`)
method. The reason for this is simple yet irritating: Ruby
assignment methods always return the assigned value, no
matter what the method itself returns. For example:

``` ruby
counters = Hamster.hash(:odds => 0, :evens => 0)
counters[:odds] += 1
  # => 1
```

Because of this, the returned copy would be lost thus
making the construct useless. Instead `Hash#put` accepts a
block instead of an explicit value so we can still do
something similar:

``` ruby
counters.put(:odds) { |value| value + 1 }
  # => {:odds => 1, :evens => 0}
```

Or more succinctly:

``` ruby
counters.put(:odds, &:next)
  # => {:odds => 1, :evens => 0}
```

**List**

Hamster `List` have a head -- the value of the item at the
head of the list -- and a tail -- containing the remaining
items:

``` ruby
list = Hamster.list(1, 2, 3)
list.head
  # => 1
list.tail
  # => Hamster.list(2, 3)
```

To add to a list, you use `List#cons`:

``` ruby
original = Hamster.list(1, 2, 3)
copy = original.cons(0)
  # => Hamster.list(0, 1, 2, 3)
```

Notice how modifying a list actually returns a new list.
That's because Hamster `List` are immutable. Thankfully,
just like Hamster `Set` and `Hash`, they're also very
efficient at making copies!

`List` is, where possible, lazy. That is, it tries to defer
processing items until absolutely necessary. For example,
given a crude function to detect prime numbers:

``` ruby
def prime?(number)
  2.upto(Math.sqrt(number).round) do |integer|
    return false if (number % integer).zero?
  end
  true
end
```

The following code will only call `#prime?` as many times as
necessary to generate the first 3 prime numbers between 10,000
and 1,000,000:

``` ruby
Hamster.interval(10_000, 1_000_000).filter do |number|
  prime?(number)
end.take(3)
  # => 0.0009s
```

Compare that to the conventional equivalent which needs to
calculate all possible values in the range before taking the
first three:

``` ruby
(10000..1000000).select do |number|
  prime?(number)
end.take(3)
  # => 10s
```

Besides `Hamster.list` there are other ways to construct lists:

  - `Hamster.interval(from, to)` creates a lazy list
    equivalent to a list containing all the values between
    `from` and `to` without actually creating a list that big.

  - `Hamster.stream { ... }` allows you to creates infinite
    lists. Each time a new value is required, the supplied
    block is called. To generate a list of integers you
    could do:

    ``` ruby
    count = 0
    Hamster.stream { count += 1 }
    ```

  - `Hamster.repeat(x)` creates an infinite list with `x` the
    value for every element.

  - `Hamster.replicate(n, x)` creates a list of size `n` with
    `x` the value for every element.

  - `Hamster.iterate(x) { |x| ... }` creates an infinite
    list where the first item is calculated by applying the
    block on the initial argument, the second item by applying
    the function on the previous result and so on. For
    example, a simpler way to generate a list of integers
    would be:

    ``` ruby
    Hamster.iterate(1) { |i| i + 1 }
    ```

    or even more succinctly:

    ``` ruby
    Hamster.iterate(1, &:next)
    ```

You also get `Enumerable#to_list`> so you can slowly
transition from built-in collection classes to Hamster.

And finally, you get `IO#to_list` allowing you to lazily
processes huge files. For example, imagine the following
code to process a 100MB file:

``` ruby
File.open("my_100_mb_file.txt") do |file|
  lines = []
  file.each_line do |line|
    break if lines.size == 10
    lines << line.chomp.downcase.reverse
  end
end
```

How many times/how long did you read the code before it became
apparent what the code actually did? Now compare that to the
following:

``` ruby
File.open("my_100_mb_file.txt") do |file|
  file.map(&:chomp).map(&:downcase).map(&:reverse).take(10)
end
```

Unfortunately, though the second example reads nicely, it
takes around 13 seconds to run (compared with 0.033 seconds
for the first) even though we're only interested in the first
10 lines! However, using a little `#to_list` magic, we
can get the running time back down to 0.033 seconds!

``` ruby
File.open("my_100_mb_file.txt") do |file|
  file.to_list.map(&:chomp).map(&:downcase).map(&:reverse).take(10)
end
```

How is this even possible? It's possible because `IO#to_list`
creates a lazy list whereby each line is only ever read and
processed as needed, in effect converting it to the first
example without all the syntactic, imperative, noise.


Installing
==========

Add this line to your application"s Gemfile:

    gem "hamster", "~> 1.0"

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install hamster


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
hash = Hamster.hash
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
hash = Hamster.hash
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


Contributing
============

  1. Fork it
  2. Create your feature branch (`git checkout -b my-new-feature`)
  3. Commit your changes (`git commit -am "Add some feature"`)
  4. Push to the branch (`git push origin my-new-feature`)
  5. Create new Pull Request


Licensing
=========

Copyright (c) 2009-2010 Simon Harris

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
