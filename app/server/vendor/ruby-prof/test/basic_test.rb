#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class BasicTest < Test::Unit::TestCase
  def setup
    # Need to use wall time for this test due to the sleep calls
    RubyProf::measure_mode = RubyProf::WALL_TIME
  end

  def start
    RubyProf.start
    RubyProf::C1.hello
  end

  def test_running
    assert(!RubyProf.running?)
    RubyProf.start
    assert(RubyProf.running?)
    RubyProf.stop
    assert(!RubyProf.running?)
  end

  def test_double_profile
    RubyProf.start
    assert_raise(RuntimeError) do
      RubyProf.start
    end
    RubyProf.stop
  end

  def test_no_block
    assert_raise(ArgumentError) do
      RubyProf.profile
    end
  end

  def test_traceback
    RubyProf.start
    assert_raise(NoMethodError) do
      RubyProf.xxx
    end

    RubyProf.stop
  end

  def test_leave_method
    start
    sleep 0.15
    profile = RubyProf.stop

    assert_equal(1, profile.threads.count)

    thread = profile.threads.first
    assert_in_delta(0.25, thread.total_time, 0.01)

    top_methods = thread.top_methods.sort
    assert_equal(2, top_methods.count)
    assert_equal("BasicTest#start", top_methods[0].full_name)
    assert_equal("BasicTest#test_leave_method", top_methods[1].full_name)

    assert_equal(4, thread.methods.length)
    methods = profile.threads.first.methods.sort

    # Check times
    assert_equal("<Class::RubyProf::C1>#hello", methods[0].full_name)
    assert_in_delta(0.1, methods[0].total_time, 0.01)
    assert_in_delta(0.0,  methods[0].wait_time, 0.01)
    assert_in_delta(0.0,  methods[0].self_time, 0.01)

    assert_equal("BasicTest#start", methods[1].full_name)
    assert_in_delta(0.1, methods[1].total_time, 0.01)
    assert_in_delta(0.0, methods[1].wait_time, 0.01)
    assert_in_delta(0.0, methods[1].self_time, 0.01)

    assert_equal("BasicTest#test_leave_method", methods[2].full_name)
    assert_in_delta(0.15, methods[2].total_time, 0.01)
    assert_in_delta(0.0, methods[2].wait_time, 0.01)
    assert_in_delta(0.0, methods[2].self_time, 0.01)

    assert_equal("Kernel#sleep", methods[3].full_name)
    assert_in_delta(0.25, methods[3].total_time, 0.01)
    assert_in_delta(0.0, methods[3].wait_time, 0.01)
    assert_in_delta(0.25, methods[3].self_time, 0.01)
  end

  def test_leave_method_2
    start
    RubyProf::C1.hello
    RubyProf::C1.hello
    profile = RubyProf.stop

    assert_equal(1, profile.threads.count)

    thread = profile.threads.first
    assert_in_delta(0.3, thread.total_time, 0.01)

    top_methods = thread.top_methods.sort
    assert_equal(2, top_methods.count)
    assert_equal("BasicTest#start", top_methods[0].full_name)
    assert_equal("BasicTest#test_leave_method_2", top_methods[1].full_name)

    assert_equal(4, thread.methods.length)
    methods = profile.threads.first.methods.sort

    # Check times
    assert_equal("BasicTest#start", methods[0].full_name)
    assert_in_delta(0.1, methods[0].total_time, 0.01)
    assert_in_delta(0.0, methods[0].wait_time, 0.01)
    assert_in_delta(0.0, methods[0].self_time, 0.01)

    assert_equal("BasicTest#test_leave_method_2", methods[1].full_name)
    assert_in_delta(0.2, methods[1].total_time, 0.01)
    assert_in_delta(0.0, methods[1].wait_time, 0.01)
    assert_in_delta(0.0, methods[1].self_time, 0.01)

    assert_equal("Kernel#sleep", methods[2].full_name)
    assert_in_delta(0.3, methods[2].total_time, 0.01)
    assert_in_delta(0.0, methods[2].wait_time, 0.01)
    assert_in_delta(0.3, methods[2].self_time, 0.01)

    assert_equal("<Class::RubyProf::C1>#hello", methods[3].full_name)
    assert_in_delta(0.3, methods[3].total_time, 0.01)
    assert_in_delta(0.0, methods[3].wait_time, 0.01)
    assert_in_delta(0.0, methods[3].self_time, 0.01)
  end
end