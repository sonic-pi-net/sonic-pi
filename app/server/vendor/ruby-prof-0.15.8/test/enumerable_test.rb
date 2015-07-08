#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

# --  Test for bug
# http://github.com/rdp/ruby-prof/issues#issue/12

class EnumerableTest < TestCase
  def test_enumerable
    result = RubyProf.profile do
      3.times {  [1,2,3].any? {|n| n} }
    end
    methods = if RUBY_VERSION >= "2.2.0"
      %w(EnumerableTest#test_enumerable Integer#times Array#any?)
    else
      %w(EnumerableTest#test_enumerable Integer#times Enumerable#any? Array#each)
    end
    assert_equal(methods, result.threads.first.methods.map(&:full_name))
  end
end
