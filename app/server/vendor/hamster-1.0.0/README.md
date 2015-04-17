Hamster
=======

  - [![Quality](http://img.shields.io/codeclimate/github/hamstergem/hamster.svg?style=flat-square)](https://codeclimate.com/github/hamstergem/hamster)
  - [![Coverage](http://img.shields.io/codeclimate/coverage/github/hamstergem/hamster.svg?style=flat-square)](https://codeclimate.com/github/hamstergem/hamster)
  - [![Build](http://img.shields.io/travis-ci/hamstergem/hamster.svg?style=flat-square)](https://travis-ci.org/hamstergem/hamster)
  - [![Dependencies](http://img.shields.io/gemnasium/hamstergem/hamster.svg?style=flat-square)](https://gemnasium.com/hamstergem/hamster)
  - [![Downloads](http://img.shields.io/gem/dtv/hamster.svg?style=flat-square)](https://rubygems.org/gems/hamster)
  - [![Tags](http://img.shields.io/github/tag/hamstergem/hamster.svg?style=flat-square)](http://github.com/hamstergem/hamster/tags)
  - [![Releases](http://img.shields.io/github/release/hamstergem/hamster.svg?style=flat-square)](http://github.com/hamstergem/hamster/releases)
  - [![Issues](http://img.shields.io/github/issues/hamstergem/hamster.svg?style=flat-square)](http://github.com/hamstergem/hamster/issues)
  - [![License](http://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat-square)](http://opensource.org/licenses/MIT)
  - [![Version](http://img.shields.io/gem/v/hamster.svg?style=flat-square)](https://rubygems.org/gems/hamster)
  - [![Discuss](http://img.shields.io/badge/discuss-join%20gitter-brightgreen.svg?style=flat-square)](https://gitter.im/hamstergem/hamster)

Efficient, immutable, and thread-safe collection classes for Ruby.

Hamster provides 6 [Persistent Data
Structures][PDS]: [`Hash`][HASH-DOC], [`Vector`][VECTOR-DOC], [`Set`][SET-DOC], [`SortedSet`][SORTED-SET-DOC], [`List`][LIST-DOC], and [`Deque`][DEQUE-DOC] (which works as an immutable queue or stack).

Hamster collections are **immutable**. Whenever you modify a Hamster
collection, the original is preserved and a modified copy is returned. This
makes them inherently thread-safe and shareable. At the same time, they remain
CPU and memory-efficient by sharing between copies. 

While Hamster collections are immutable, you can still mutate objects stored
in them. We recommend that  you don't do this, unless you are sure you know 
what you are doing. Hamster collections are thread-safe and can be freely 
shared between threads, but you are responsible for making sure that the 
objects stored in them are used in a thread-safe manner.

Hamster collections are almost always closed under a given operation. That is,
whereas Ruby's collection methods always return arrays, Hamster collections
will return an instance of the same class wherever possible.

Where possible, Hamster collections offer an interface compatible with Ruby's
built-in `Hash`, `Array`, and `Enumerable`, to ease code migration. Also, Hamster methods accept regular Ruby collections as arguments, so code which uses `Hamster` can easily interoperate with your other Ruby code.

And lastly, Hamster lists are lazy, making it possible to (among other things)
process "infinitely large" lists.

[PDS]: http://en.wikipedia.org/wiki/Persistent_data_structure
[HASH-DOC]: http://rubydoc.info/github/hamstergem/hamster/master/Hamster/Hash
[SET-DOC]: http://rubydoc.info/github/hamstergem/hamster/master/Hamster/Set
[VECTOR-DOC]: http://rubydoc.info/github/hamstergem/hamster/master/Hamster/Vector
[LIST-DOC]: http://rubydoc.info/github/hamstergem/hamster/master/Hamster/List
[SORTED-SET-DOC]: http://rubydoc.info/github/hamstergem/hamster/master/Hamster/SortedSet
[DEQUE-DOC]: http://rubydoc.info/github/hamstergem/hamster/master/Hamster/Deque


Using
=====

To make the collection classes available in your code:

``` ruby
require "hamster"
```

Or if you prefer to only pull in certain collection types:

``` ruby
require "hamster/hash"
require "hamster/vector"
require "hamster/set"
require "hamster/sorted_set"
require "hamster/list"
require "hamster/deque"
```

<h2>Hash <span style="font-size:0.7em">(<a href="http://rubydoc.info/github/hamstergem/hamster/master/Hamster/Hash">API Documentation</a>)</span></h2>

Constructing a Hamster `Hash` is almost as simple as a regular one:

``` ruby
person = Hamster.hash(name: "Simon", gender: :male)
# => Hamster::Hash[:name => "Simon", :gender => :male]
```

Accessing the contents will be familiar to you:

``` ruby
person[:name]                       # => "Simon"
person.get(:gender)                 # => :male
```

Updating the contents is a little different than you are used to:

``` ruby
friend = person.put(:name, "James") # => Hamster::Hash[:name => "James", :gender => :male]
person                              # => Hamster::Hash[:name => "Simon", :gender => :male]
friend[:name]                       # => "James"
person[:name]                       # => "Simon"
```

As you can see, updating the hash returned a copy leaving
the original intact. Similarly, deleting a key returns
yet another copy:

``` ruby
male = person.delete(:name)         # => Hamster::Hash[:gender => :male]
person                              # => Hamster::Hash[:name => "Simon", :gender => :male]
male.key?(:name)                    # => false
person.key?(:name)                  # => true
```

Since it is immutable, Hamster's `Hash` doesn't provide an assignment
(`Hash#[]=`) method. However, `Hash#put` can accept a block which
transforms the value associated with a given key:

``` ruby
counters.put(:odds) { |value| value + 1 } # => Hamster::Hash[:odds => 1, :evens => 0]
```

Or more succinctly:

``` ruby
counters.put(:odds, &:next)         # => {:odds => 1, :evens => 0}
```

This is just the beginning; see the [API documentation][HASH-DOC] for details on all `Hash` methods.


<h2>Vector <span style="font-size:0.7em">(<a href="http://rubydoc.info/github/hamstergem/hamster/master/Hamster/Vector">API Documentation</a>)</span></h2>

A `Vector` is an integer-indexed collection much like an immutable `Array`. Examples:

``` ruby
vector = Hamster.vector(1, 2, 3, 4) # => Hamster::Vector[1, 2, 3, 4]
vector[0]                           # => 1
vector[-1]                          # => 4
vector.set(1, :a)                   # => Hamster::Vector[1, :a, 3, 4]
vector.add(:b)                      # => Hamster::Vector[1, 2, 3, 4, :b]
vector.insert(2, :a, :b)            # => Hamster::Vector[1, 2, :a, :b, 3, 4]
vector.delete_at(0)                 # => Hamster::Vector[2, 3, 4]
```

Other `Array`-like methods like `#select`, `#map`, `#shuffle`, `#uniq`, `#reverse`,
`#rotate`, `#flatten`, `#sort`, `#sort_by`, `#take`, `#drop`, `#take_while`,
`#drop_while`, `#fill`, `#product`, and `#transpose` are also supported. See the
[API documentation][VECTOR-DOC] for details on all `Vector` methods.


<h2>Set <span style="font-size:0.7em">(<a href="http://rubydoc.info/github/hamstergem/hamster/master/Hamster/Set">API Documentation</a>)</span></h2>

A `Set` is an unordered collection of values with no duplicates. It is much like the Ruby standard library's `Set`, but immutable. Examples:

``` ruby
set = Hamster.set(:red, :blue, :yellow) # => Hamster::Set[:red, :blue, :yellow]
set.include? :red                       # => true
set.add :green                          # => Hamster::Set[:red, :blue, :yellow, :green]
set.delete :blue                        # => Hamster::Set[:red, :yellow]
set.superset? Hamster.set(:red, :blue)  # => true
set.union([:red, :blue, :pink])         # => Hamster::Set[:red, :blue, :yellow, :pink]
set.intersection([:red, :blue, :pink])  # => Hamster::Set[:red, :blue]
```

Like most Hamster methods, the set-theoretic methods `#union`, `#intersection`, `#difference`, and `#exclusion` (aliased as `#|`, `#&`, `#-`, and `#^`) all work with regular Ruby collections, or indeed any `Enumerable` object. So just like all the other Hamster collections, `Hamster::Set` can easily be used in combination with "ordinary" Ruby code.

See the [API documentation][SET-DOC] for details on all `Set` methods.


<h2>SortedSet <span style="font-size:0.7em">(<a href="http://rubydoc.info/github/hamstergem/hamster/master/Hamster/SortedSet">API Documentation</a>)</span></h2>

A `SortedSet` is like a `Set`, but ordered. You can do everything with it that you can
do with a `Set`. Additionally, you can get the `#first` and `#last` item, or retrieve
an item using an integral index:

``` ruby
set = Hamster.sorted_set('toast', 'jam', 'bacon') # => Hamster::SortedSet["bacon", "jam", "toast"]
set.first                                         # => "bacon"
set.last                                          # => "toast"
set[1]                                            # => "jam"
```

You can also specify the sort order using a block:

``` ruby
Hamster.sorted_set('toast', 'jam', 'bacon') { |a,b| b <=> a }
Hamster.sorted_set('toast', 'jam', 'bacon') { |str| str.chars.last }
```

See the [API documentation][SORTED-SET-DOC] for details on all `SortedSet` methods.


<h2>List <span style="font-size:0.7em">(<a href="http://rubydoc.info/github/hamstergem/hamster/master/Hamster/List">API Documentation</a>)</span></h2>

Hamster `List`s have a "head" (the value at the front of the list),
and a "tail" (a list of the remaining items):

``` ruby
list = Hamster.list(1, 2, 3)
list.head                    # => 1
list.tail                    # => Hamster.list(2, 3)
```

To add to a list, you use `List#cons`:

``` ruby
original = Hamster.list(1, 2, 3)
copy = original.cons(0)      # => Hamster.list(0, 1, 2, 3)
```

Notice how modifying a list actually returns a new list.
That's because Hamster `List`s are immutable. Thankfully,
just like other Hamster collections, they're also very
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

You also get `Enumerable#to_list` so you can slowly
transition from built-in collection classes to Hamster.

And finally, you get `IO#to_list` allowing you to lazily
process huge files. For example, imagine the following
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

See the API documentation for details on all [`List`][LIST-DOC] methods.


<h2>Deque <span style="font-size:0.7em">(<a href="http://rubydoc.info/github/hamstergem/hamster/master/Hamster/Deque">API Documentation</a>)</span></h2>

A `Deque` (or "double-ended queue") is an ordered collection, which allows you to push and pop items from both front and back. This makes it perfect as an immutable stack *or* queue. Examples:

``` ruby
deque = Hamster.deque(1, 2, 3) # => Hamster::Deque[1, 2, 3]
deque.first                    # 1
deque.last                     # 3
deque.pop                      # => Hamster::Deque[1, 2]
deque.push(:a)                 # => Hamster::Deque[1, 2, 3, :a]
deque.shift                    # => Hamster::Deque[2, 3]
deque.unshift(:a)              # => Hamster::Deque[:a, 1, 2, 3]
```

Of course, you can do the same thing with a `Vector`, but a `Deque` is more efficient. See the API documentation for details on all [`Deque`][DEQUE-DOC] methods.


Installing
==========

Add this line to your application's Gemfile:

    gem "hamster", "~> 1.0"

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install hamster


Contributing
============

  1. Fork it
  2. Create your feature branch (`git checkout -b my-new-feature`)
  3. Commit your changes (`git commit -am "Add some feature"`)
  4. Push to the branch (`git push origin my-new-feature`)
  5. Create new Pull Request


Other Reading
=============

- The structure which is used for Hamster's `Hash` and `Set`: [Hash Array Mapped Tries][HAMT]
- An interesting perspective on why immutability itself is inherently a good thing: Matthias Felleisen's [Function Objects presentation][FO].
- The Hamster [FAQ](FAQ.md)
- [Changelog](CHANGELOG.md)

[HAMT]: http://lampwww.epfl.ch/papers/idealhashtrees.pdf
[FO]: http://www.ccs.neu.edu/home/matthias/Presentations/ecoop2004.pdf


Code of Conduct
===============

As contributors and maintainers of this project, we pledge to respect all people who contribute through reporting issues, posting feature requests, updating documentation, submitting pull requests or patches, and other activities.

We are committed to making participation in this project a harassment-free experience for everyone, regardless of level of experience, gender, gender identity and expression, sexual orientation, disability, personal appearance, body size, race, age, or religion.

Examples of unacceptable behavior by participants include the use of sexual language or imagery, derogatory comments or personal attacks, trolling, public or private harassment, insults, or other unprofessional conduct.

Project maintainers have the right and responsibility to remove, edit, or reject comments, commits, code, wiki edits, issues, and other contributions that are not aligned to this Code of Conduct. Project maintainers who do not follow the Code of Conduct may be removed from the project team.

Instances of abusive, harassing, or otherwise unacceptable behavior may be reported by opening an issue or contacting one or more of the project maintainers.

This Code of Conduct is adapted from the [Contributor Covenant](http:contributor-covenant.org), version 1.0.0, available at [http://contributor-covenant.org/version/1/0/0/](http://contributor-covenant.org/version/1/0/0/)


Licensing
=========

Copyright (c) 2009-2014 Simon Harris

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
