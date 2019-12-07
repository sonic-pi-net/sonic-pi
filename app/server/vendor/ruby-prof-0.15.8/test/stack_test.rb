#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

# Test data
#     A
#    / \
#   B   C
#        \
#         B

class StackClass
  def a
    sleep 1
    b
    c
  end

  def b
    sleep 2
  end

  def c
    sleep 3
    b
  end
end

class StackTest < TestCase
  def setup
    # Need to use wall time for this test due to the sleep calls
    RubyProf::measure_mode = RubyProf::WALL_TIME
  end

  def test_call_sequence
    c = StackClass.new
    result = RubyProf.profile do
      c.a
    end

    # Length should be 5:
    #   StackTest#test_call_sequence
    #   StackClass#a
    #   Kernel#sleep
    #   StackClass#c
    #   StackClass#b

    methods = result.threads.first.methods.sort.reverse
    assert_equal(5, methods.length)

    # Check StackTest#test_call_sequence
    method = methods[0]
    assert_equal('StackTest#test_call_sequence', method.full_name)
    assert_equal(1, method.called)
    assert_in_delta(8, method.total_time, 0.25)
    assert_in_delta(0, method.wait_time, 0.01)
    assert_in_delta(0, method.self_time, 0.01)
    assert_in_delta(8, method.children_time, 0.25)
    assert_equal(1, method.call_infos.length)

    call_info = method.call_infos[0]
    assert_equal('StackTest#test_call_sequence', call_info.call_sequence)
    assert_equal(1, call_info.children.length)

    # Check StackClass#a
    method = methods[1]
    assert_equal('StackClass#a', method.full_name)
    assert_equal(1, method.called)
    assert_in_delta(8, method.total_time, 0.15)
    assert_in_delta(0, method.wait_time, 0.01)
    assert_in_delta(0, method.self_time, 0.01)
    assert_in_delta(8, method.children_time, 0.05)
    assert_equal(1, method.call_infos.length)

    call_info = method.call_infos[0]
    assert_equal('StackTest#test_call_sequence->StackClass#a', call_info.call_sequence)
    assert_equal(3, call_info.children.length)

    # Check Kernel#sleep
    method = methods[2]
    assert_equal('Kernel#sleep', method.full_name)
    assert_equal(4, method.called)
    assert_in_delta(8, method.total_time, 0.05)
    assert_in_delta(0, method.wait_time, 0.01)
    assert_in_delta(8, method.self_time, 0.05)
    assert_in_delta(0, method.children_time, 0.05)
    assert_equal(4, method.call_infos.length)

    call_info = method.call_infos[0]
    assert_equal('StackTest#test_call_sequence->StackClass#a->Kernel#sleep', call_info.call_sequence)
    assert_equal(0, call_info.children.length)

    call_info = method.call_infos[1]
    assert_equal('StackTest#test_call_sequence->StackClass#a->StackClass#b->Kernel#sleep', call_info.call_sequence)
    assert_equal(0, call_info.children.length)

    call_info = method.call_infos[2]
    assert_equal('StackTest#test_call_sequence->StackClass#a->StackClass#c->Kernel#sleep', call_info.call_sequence)
    assert_equal(0, call_info.children.length)

    call_info = method.call_infos[3]
    assert_equal('StackTest#test_call_sequence->StackClass#a->StackClass#c->StackClass#b->Kernel#sleep', call_info.call_sequence)
    assert_equal(0, call_info.children.length)

    # Check StackClass#c
    method = methods[3]
    assert_equal('StackClass#c', method.full_name)
    assert_equal(1, method.called)
    assert_in_delta(5, method.total_time, 0.05)
    assert_in_delta(0, method.wait_time, 0.01)
    assert_in_delta(0, method.self_time, 0.01)
    assert_in_delta(5, method.children_time, 0.05)
    assert_equal(1, method.call_infos.length)

    call_info = method.call_infos[0]
    assert_equal('StackTest#test_call_sequence->StackClass#a->StackClass#c', call_info.call_sequence)
    assert_equal(2, call_info.children.length)

    # Check StackClass#b
    method = methods[4]
    assert_equal('StackClass#b', method.full_name)
    assert_equal(2, method.called)
    assert_in_delta(4, method.total_time, 0.05)
    assert_in_delta(0, method.wait_time, 0.01)
    assert_in_delta(0, method.self_time, 0.01)
    assert_in_delta(4, method.children_time, 0.05)
    assert_equal(2, method.call_infos.length)

    call_info = method.call_infos[0]
    assert_equal('StackTest#test_call_sequence->StackClass#a->StackClass#b', call_info.call_sequence)
    assert_equal(1, call_info.children.length)

    call_info = method.call_infos[1]
    assert_equal('StackTest#test_call_sequence->StackClass#a->StackClass#c->StackClass#b', call_info.call_sequence)
    assert_equal(1, call_info.children.length)
  end
end
