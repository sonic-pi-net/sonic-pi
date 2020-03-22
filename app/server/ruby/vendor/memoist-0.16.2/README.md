Memoist
=============

[![Build Status](https://travis-ci.org/matthewrudy/memoist.svg?branch=master)](https://travis-ci.org/matthewrudy/memoist)

Memoist is an extraction of ActiveSupport::Memoizable.

Since June 2011 ActiveSupport::Memoizable has been deprecated.
But I love it,
and so I plan to keep it alive.

Usage
-----

Just extend with the Memoist module

```ruby
require 'memoist'
class Person
  extend Memoist

  def social_security
    puts "execute!"
    decrypt_social_security
  end
  memoize :social_security
end

person = Person.new

person.social_security
# execute!
# => (returns decrypt_social_security)

person.social_security
# => (returns the memoized value)
```

And person.social_security will only be calculated once.

Every memoized function (which initially was not accepting any arguments) has a ```(reload)```
argument you can pass in to bypass and reset the memoization:

```ruby
def some_method
  Time.now
end
memoize :some_method
```

Calling ```some_method``` will be memoized, but calling ```some_method(true)``` will rememoize each time.

You can even memoize method that takes arguments.

```ruby
class Person
  def taxes_due(income)
    income * 0.40
  end
  memoize :taxes_due
end
```

This will only be calculated once per value of income.

You can also memoize class methods.

```ruby
class Person

  class << self
    extend Memoist
    def with_overdue_taxes
      # ...
    end
    memoize :with_overdue_taxes
  end

end
```

When a sub-class overrides one of its parent's methods and you need to memoize both. 
Then you can use the `:identifier` parameter in order to help _Memoist_ distinguish between the two.

```ruby
class Clock
  extend Memoist
  def now
     "The time now is #{Time.now.hour} o'clock and #{Time.now.min} minutes"
  end
  memoize :now
end

class AccurateClock < Clock
  extend Memoist
  def now
    "#{super} and #{Time.now.sec} seconds"
  end
  memoize :now, :identifier => :accurate_clock
end
```


Reload
------

Each memoized function comes with a way to flush the existing value.

```ruby
person.social_security       # returns the memoized value
person.social_security(true) # bypasses the memoized value and rememoizes it
```

This also works with a memoized method with arguments

```ruby
person.taxes_due(100_000)       # returns the memoized value
person.taxes_due(100_000, true) # bypasses the memoized value and rememoizes it
```

If you want to flush the entire memoization cache for an object

```ruby
person.flush_cache   # returns an array of flushed memoized methods, e.g. ["social_security", "some_method"]
```

Authors
===========

Everyone who contributed to it in the rails repository.

* Joshua Peek
* Tarmo Tänav
* Jeremy Kemper
* Eugene Pimenov
* Xavier Noria
* Niels Ganser
* Carl Lerche & Yehuda Katz
* jeem
* Jay Pignata
* Damien Mathieu
* José Valim
* Matthew Rudy Jacobs

Contributing
============

1. Fork it ( https://github.com/matthewrudy/memoist/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

License
=======

Released under the [MIT License](http://www.opensource.org/licenses/MIT), just as Ruby on Rails is.
