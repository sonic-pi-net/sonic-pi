#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path("../test_helper", __FILE__)

class BlockMethodTest < TestCase
  def setup
    # Need to use wall time for this test due to the sleep calls
    RubyProf::measure_mode = RubyProf::WALL_TIME
  end

  def test_block
    result = RubyProf.profile do
      1.times { RubyProf::C1.new.hello }
    end

    # Methods called
    #  Kernel#sleep
    #  <Class::BasicObject>#allocate
    #  #{RubyProf.parent_object}#inizialize
    #  RubyProf::C1#hello
    #  Class#new
    #  Integer#times
    #  BlockMethodTest#test_block

    methods = result.threads.first.methods.sort.reverse
    assert_equal(RubyProf.ruby_2? ? 6 : 7, methods.length)

    # Check times
    assert_equal("BlockMethodTest#test_block", methods[0].full_name)
    assert_in_delta(0.2, methods[0].total_time, 0.02)
    assert_in_delta(0.0, methods[0].wait_time, 0.02)
    assert_in_delta(0.0, methods[0].self_time, 0.02)

    assert_equal("Integer#times", methods[1].full_name)
    assert_in_delta(0.2, methods[1].total_time, 0.02)
    assert_in_delta(0.0, methods[1].wait_time, 0.02)
    assert_in_delta(0.0, methods[1].self_time, 0.02)

    assert_equal("RubyProf::C1#hello", methods[2].full_name)
    assert_in_delta(0.2, methods[2].total_time, 0.02)
    assert_in_delta(0.0, methods[2].wait_time, 0.02)
    assert_in_delta(0.0, methods[2].self_time, 0.02)

    assert_equal("Kernel#sleep", methods[3].full_name)
    assert_in_delta(0.2, methods[3].total_time, 0.01)
    assert_in_delta(0.0, methods[3].wait_time, 0.01)
    assert_in_delta(0.2, methods[3].self_time, 0.01)

    assert_equal("Class#new", methods[4].full_name)
    assert_in_delta(0.0, methods[4].total_time, 0.01)
    assert_in_delta(0.0, methods[4].wait_time, 0.01)
    assert_in_delta(0.0, methods[4].self_time, 0.01)

    # the timing difference between #initialize and #allocate is so small
    # that we cannot rely on #initialize appearing first.
    # so switch them, if necessary
    if methods[5].full_name =~ /#allocate/
      methods[5], methods[6] = methods[6], methods[5]
    end

    assert_equal("#{RubyProf.parent_object}#initialize", methods[5].full_name)
    assert_in_delta(0.0, methods[5].total_time, 0.01)
    assert_in_delta(0.0, methods[5].wait_time, 0.01)
    assert_in_delta(0.0, methods[5].self_time, 0.01)

    unless RubyProf.ruby_2?
      assert_equal("<Class::#{RubyProf.parent_object}>#allocate", methods[6].full_name)
      assert_in_delta(0.0, methods[6].total_time, 0.01)
      assert_in_delta(0.0, methods[6].wait_time, 0.01)
      assert_in_delta(0.0, methods[6].self_time, 0.01)
    end
  end
end
