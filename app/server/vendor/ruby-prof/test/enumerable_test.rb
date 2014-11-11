#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

# --  Test for bug
# http://github.com/rdp/ruby-prof/issues#issue/12

class EnumerableTest < Test::Unit::TestCase
  def test_enumerable
    result = RubyProf.profile do
      3.times {  [1,2,3].any? {|n| n} }
    end
    assert_equal(result.threads.first.methods.length, 4)
  end
end
