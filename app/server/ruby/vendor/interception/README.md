
Interception (intercept + exception) allows you to intercept all exceptions as they are
raised.

Installation
============

As with all rubygems, use gem to install:

```shell
gem install interception
```

Or, if you're using bundler:

```ruby
source 'https://rubygems.org'
gem 'interception'
```

Usage
=====

Add and remove listeners. They'll be called whenever an exception is raised (whether or
not it would later be rescued) with both the exception object, and the binding from which
it was raised. The binding can be used to discover further information about the context.

```ruby
require 'interception'
listener = lambda{ |exception, binding|
  puts "raised: #{exception.inspect}"
}

Interception.listen(listener)

begin
  raise "oopsy"
rescue => exception
  puts "rescued: #{exception.inspect}"
end

raise "daisy"

Interception.unlisten(listener)
```

In the common case that you want to listen to events for the duration of a block, you can
also pass that block to listen:

```ruby
require 'interception'
def log_exceptions(&block)
  Interception.listen(block) do |exception, binding|
    puts "raised: #{exception.inspect} from #{binding.eval("__method__")}"
  end
end

def hello
  raise "oopsy"
rescue => exception
  puts "rescued: #{exception.inspect} in #{__method__}"
  raise "daisy"
end

log_exceptions do
  hello
end
```

Your listen block is run as through it were called by `Kernel#raise`, so try not to raise
an exception from within it, or you will loose the original exception.

Known bugs
==========

* On rubinius we don't catch some low-level exceptions (like `ZeroDivisionError`).
* On MRI-1.8.7, the binding sometimes has the wrong value for `self`.
* The Interception versions prior to `0.4` **do not** support MRI-2.1.0. `>= 0.4` does support it.

Meta-fu
=======

Interception is released under the MIT license (see `LICENSE.MIT` for details).
Contributions and bug reports are welcome.
