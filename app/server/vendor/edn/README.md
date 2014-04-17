# edn-ruby

[![Build Status](https://secure.travis-ci.org/relevance/edn-ruby.png)](http://travis-ci.org/relevance/edn-ruby)


&copy; 2012 Relevance Inc

**edn-ruby** is a Ruby library to read and write [edn][edn] (extensible data notation), a subset of Clojure used for transferring data between applications, much like JSON, YAML, or XML.

## Installation

Add this line to your application's Gemfile:

    gem 'edn'

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install edn

## Usage

To read a string of **edn**:

```ruby
EDN.read('[1 2 {:foo "bar"}]')
```

To convert a data structure to an **edn** string:

```ruby
data.to_edn
```

By default, this will work for strings, symbols, numbers, arrays, hashes, sets, nil, Time, and boolean values.

### Multiple element reading

If you have a string of **edn** that contains multiple forms, you can create an `EDN::Reader`, which extends `Enumerable`.

```ruby
r = EDN::Reader.new('[1 2 3] {:a 1 :b 2}')

r.read #=> [1, 2, 3]
r.read #=> {:a => 1, :b => 2}
r.read #=> RuntimeError: EDN::Reader is out of string!

r.each do |form|
  p form
end

#=> [1, 2, 3]
#=> {:a => 1, :b => 2}

r.count #=> 2
```

### Value Translations

**edn** has some different terminology, and some types that do not map cleanly to Ruby.

**NOTE**: Comments requested on the following.

In **edn**, you have _keywords_, which look like Ruby symbols and have the same meaning and purpose. These are converted to Ruby symbols.

You have **edn** _symbols_, which generally reflect variable names, but have several purposes. We parse these and return `EDN::Type::Symbol` values for them, as they are not directly portable into Ruby. To create an **edn** symbol in Ruby, call `EDN::Type::Symbol.new` or `EDN.symbol` with a string argument, or use the convenience unary operator `~` like so: `~"elf/rings"`.

You have _vectors_, which map to Ruby arrays, and _lists_, which are linked lists in Clojure. We map these to `EDN::Type::List` values, which are type-compatible with arrays. To create an **edn** list in Ruby, call `EDN::Type::List.new` or `EDN.list` with all arguments to go in the list. If you have an array, you will use the splat operator, like so: `EDN.list(*[1, 2, 3])`. You can also use the `~` unary operator like so: `~[1, 2, 3]`.

**edn** has character types, but Ruby does not. These are converted into one-character strings.

### Tagged Values

The interesting part of **edn** is the _extensible_ part. Data can be be _tagged_ to coerce interpretation of it to a particular data type. An example of a tagged data element:

```
#wolf/pack {:alpha "Greybeard" :betas ["Frostpaw" "Blackwind" "Bloodjaw"]}
```

The tag (`#wolf/pack`) will tell any consumers of this data to use a data type registered to handle `wolf/pack` to represent this data.

The rules for tags from the [**edn** README][README] should be followed. In short, custom tags should have a prefix (the part before the `/`) designating the user that created them or context they are used in. Non-prefixed tags are reserved for built-in tags.

There are two tags built in by default: `#uuid`, used for UUIDs, and `#inst`, used for an instant in time. In `edn-ruby`, `#inst` is converted to a Time, and Time values are tagged as `#inst`. There is not a UUID data type built into Ruby, so `#uuid` is converted to an instance of `EDN::Type::UUID`.

Tags that are not registered generate a struct of the type `EDN::Type::Unknown` with the methods `tag` and `value`.

### Registering a New Tag For Reading

To register a tag for reading, call the method `EDN.register` with a tag and one of the following:

- A block that accepts data and returns a value.
- A lambda that accepts data and returns a value.
- A class that has an `initialize` method that accepts data.

Examples:

```ruby
EDN.register("clinton/uri") do |uri|
  URI(uri)
end

EDN.register("clinton/date", lambda { |date_array| Date.new(*date_array) })

class Dog
  def initialize(name)
    @name = name
  end
end

EDN.register("clinton/dog", Dog)
```

### Writing Tags

Writing tags should be done as part of the class's `.to_edn` method, like so:

```ruby
class Dog
  def to_edn
    ["#clinton/dog", @name.to_edn].join(" ")
  end
end
```

`EDN` provides a helper method, `EDN.tagout`:

```ruby
class Dog
  def to_edn
    EDN.tagout("clinton/dog", @name)
  end
end
```

This method calls `.to_edn` on the second argument and joins the arguments appropriately.

Other examples are:
```
EDN.tagout("wolf/pack", {:alpha=>"Greybeard", :betas=>["Frostpaw", "Blackwind", "Bloodjaw"]})
 => "#wolf/pack {:alpha \"Greybeard\", :betas [\"Frostpaw\" \"Blackwind\" \"Bloodjaw\"]}"

class Range
  def to_edn
    EDN.tagout("ruby/range", [self.begin, self.end, self.exclude_end?])
  end
end

(0..9).to_edn
=> "#ruby/range [0 9 false]"
```


## Metadata

Certain elements of **edn** can have *metadata*. Metadata is a map of values about the element, which must follow specific rules.

* Only symbols, lists, vectors, maps, and sets can have metadata. Tagged elements *cannot* have metadata.
* Metadata keys must be symbols, keywords, or strings.

Metadata can be expressed in one of the following three ways:

* Via a map. The element is prefixed with a map which has a caret (`^`) prefixed to it, like so: `^{:doc "This is my vector" :rel :temps} [98.6 99.7]`.
* Via a keyword. The element is prefixed with a keyword, also prefixed by a caret: `^:awesome #{1 2 \c}`. This results in the key `:awesome` being set to `true`, as if the metadata was: `^{:awesome true} #{1 2 \c}`.
* Via a symbol. The element is prefixed with a symbol, also prefixed by a caret: `^Boolean "true"`. This results in the key `:tag` being set to the symbol, as if the metadata was: `^{:tag Boolean} "true"`. This is used in Clojure to indicate the Java type of the element. In other **edn** implementations, it may be ignored or used differently.

More than one piece of metadata can be applied to an element. Metadata is applied to the next element appearing after it, so in the case of `^:foo ^{:bar false} [1 2]`, the metadata would be, in total, `^{:foo true, :bar false}`. Note that `^:foo` is applied to the element `[1 2]` with the metadata `^{:bar false}` applied to it. Because of this, key collisions are resolved *right-to-left*.

## Contributors

* Clinton N. Dreisbach (@crnixon)
* Michael Ficarra (@michaelficarra)
* Andrew Forward (@aforward)
* Gabriel Horner (@cldwalker)

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

[edn]: https://github.com/edn-format/edn
[README]: https://github.com/edn-format/edn/blob/master/README.md
