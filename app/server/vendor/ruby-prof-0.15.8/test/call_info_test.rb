#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class CallInfoTest < TestCase
  def setup
    # Need to use wall time for this test due to the sleep calls
    RubyProf::measure_mode = RubyProf::WALL_TIME
  end

#  def test_clone
#    result = RubyProf.profile do
#      RubyProf::C1.hello
#    end
#
#    method = result.threads.first.top_methods.first
#    call_info = method.call_infos.first
#    call_info_clone = call_info.clone
#
##    assert_equal(call_info.target, call_info_clone.target)
#    assert_equal(call_info.total_time, call_info_clone.total_time)
#  end

  def test_merge
    result1 = RubyProf.profile do
      RubyProf::C1.hello
    end

    methods = result1.threads.first.methods.sort.reverse
    assert_equal(3, methods.length)

    assert_equal('CallInfoTest#test_merge', methods[0].full_name)
    assert_in_delta(0.1, methods[0].total_time, 0.01)
    assert_in_delta(0, methods[0].wait_time, 0.01)
    assert_in_delta(0, methods[0].self_time, 0.01)
    assert_equal(1, methods[0].called)

    assert_equal('<Class::RubyProf::C1>#hello', methods[1].full_name)
    assert_in_delta(0.1, methods[1].total_time, 0.01)
    assert_in_delta(0, methods[1].wait_time, 0.01)
    assert_in_delta(0, methods[1].self_time, 0.01)
    assert_equal(1, methods[1].called)

    assert_equal('Kernel#sleep', methods[2].full_name)
    assert_in_delta(0.1, methods[2].total_time, 0.01)
    assert_in_delta(0, methods[2].wait_time, 0.01)
    assert_in_delta(0.1, methods[2].self_time, 0.01)
    assert_equal(1, methods[2].called)

    RubyProf.profile do
      RubyProf::C1.hello
    end

    # Merge the trees
    methods = result1.threads.first.methods.sort.reverse
    assert_equal(3, methods.length)

    assert_equal('CallInfoTest#test_merge', methods[0].full_name)
    assert_in_delta(0.1, methods[0].total_time, 0.01)
    assert_in_delta(0, methods[0].wait_time, 0.01)
    assert_in_delta(0, methods[0].self_time, 0.01)
    assert_equal(1, methods[0].called)

    assert_equal('<Class::RubyProf::C1>#hello', methods[1].full_name)
    assert_in_delta(0.1, methods[1].total_time, 0.01)
    assert_in_delta(0, methods[1].wait_time, 0.01)
    assert_in_delta(0, methods[1].self_time, 0.01)
    assert_equal(1, methods[1].called)

    assert_equal('Kernel#sleep', methods[2].full_name)
    assert_in_delta(0.1, methods[2].total_time, 0.01)
    assert_in_delta(0, methods[2].wait_time, 0.01)
    assert_in_delta(0.1, methods[2].self_time, 0.01)
    assert_equal(1, methods[2].called)
  end
end

