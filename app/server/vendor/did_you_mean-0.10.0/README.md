# did_you_mean [![Gem Version](https://badge.fury.io/rb/did_you_mean.svg)](https://rubygems.org/gems/did_you_mean) [![Build Status](https://travis-ci.org/yuki24/did_you_mean.svg?branch=master)](https://travis-ci.org/yuki24/did_you_mean)

'Did you mean?' experience in Ruby. No, Really.

**For those who are still using 0.6.0, 0.7.0 and 0.8.0, please upgrade to the latest version (0.9.6) as they have a serious bug with Ruby 2.1.3 and 2.1.4 installed on Mac OS X.**

## Installation

Add this line to your application's Gemfile:

```ruby
gem 'did_you_mean', group: [:development, :test]
```

## Examples

### NameError

#### Correcting a Misspelled Method Name

```ruby
class User
  attr_accessor :first_name, :last_name

  def to_s
    "#{f1rst_name} #{last_name}" # f1rst_name ???
  end
end

user.to_s
# => NameError: undefined local variable or method `f1rst_name' for #<User:0x0000000928fad8>
#
#     Did you mean? #first_name
#
```

#### Correcting a Misspelled Class Name

```ruby
class Book
  class TableOfContents
    # ...
  end
end

Book::TableofContents # TableofContents ???
# => NameError: uninitialized constant Book::TableofContents
#
#     Did you mean? Book::TableOfContents
#
```

#### Suggesting an instance variable name

```ruby
@full_name = "Yuki Nishijima"
first_name, last_name = full_name.split(" ")
# => NameError: undefined local variable or method `full_name' for main:Object
#
#     Did you mean? @full_name
#
```

### NoMethodError

```ruby
# In a Rails controller:
params.with_inddiferent_access
# => NoMethodError: undefined method `with_inddiferent_access' for {}:Hash
#
#     Did you mean? #with_indifferent_access
#
```

## 'Did You Mean' Experience is Everywhere

_did\_you\_mean_ gem automagically puts method suggestions into the error message. This means you'll have the "Did you mean?" experience almost everywhere:

![Did you mean? on BetterErrors](https://raw.githubusercontent.com/yuki24/did_you_mean/master/doc/did_you_mean_example.png)

## Support

_did\_you\_mean_ gem supports the following implementations:

 * MRI 1.9.3, 2.0.0, 2.1.x, 2.2.x and ruby-head
 * JRuby 1.7.21, 9.0.0.0 and jruby-head
 * Rubinius 2.4.1 and 2.5.8

## Contributing

1. Fork it (http://github.com/yuki24/did_you_mean/fork)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

## License

Copyright (c) 2015 Yuki Nishijima. See MIT-LICENSE for further details.
