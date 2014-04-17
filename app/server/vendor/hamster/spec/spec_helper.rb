require "coveralls"
Coveralls.wear! do
  add_filter "/spec/"
end

require "pry"
require "rspec"

def fixture(name)
  File.read(fixture_path(name))
end

def fixture_path(name)
  File.join("spec", "fixtures", name)
end

if RUBY_ENGINE == "ruby"
  def calculate_stack_overflow_depth(n)
    calculate_stack_overflow_depth(n + 1)
  rescue SystemStackError
    n
  end
  STACK_OVERFLOW_DEPTH = calculate_stack_overflow_depth(2)
else
  STACK_OVERFLOW_DEPTH = 16_384
end

class DeterministicHash
  attr_reader :hash

  def initialize(value, hash)
    @value = value
    @hash = hash
  end

  def to_s
    @value.to_s
  end

  def inspect
    @value.inspect
  end
end
